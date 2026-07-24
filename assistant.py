#!/usr/bin/env python3
"""
assistant.py - Emacs 依赖包管理助手

子命令:
    sync    同步依赖包（从 GitHub Archive 下载）
    add     添加新的依赖包到 packages.json
    check   检测本地包是否缺失或多余

用法:
    python assistant.py sync --all
    python assistant.py sync --all --clean
    python assistant.py sync company-mode yasnippet
    python assistant.py sync company-mode --replace
    python assistant.py add company-mode/company-mode
    python assistant.py add company-mode/company-mode --ref main --sync
    python assistant.py check

同步策略:
    - 按 branch ref 下载（archive/refs/heads/<ref>.zip），并发下载与解压（默认 6 并发）。
    - 下载带超时与重试，解压防御 zip-slip。
    - 默认覆盖拷贝（不删除目标目录，直接覆盖文件）。
    - --replace 选项：先删除目标目录再重新拷贝（适用于需要清理残留文件的场景）。
"""

import argparse
import concurrent.futures
import fnmatch
import glob
import io
import json
import os
import shutil
import sys
import threading
import time
import urllib.request
import zipfile

if sys.version_info < (3, 9):
    sys.exit("Error: assistant.py requires Python 3.9+ (uses str.removesuffix).")

# Windows GBK 编码兼容
sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding="utf-8", errors="replace")

# 全局默认排除规则（所有包生效，包级 ignores 在此基础上叠加）
GLOBAL_IGNORES = [
    "test*", "tests",
    "doc", "docs",
    ".github", ".travis*", ".circleci",
    "Makefile", "Cask", "Eask",
    "CONTRIBUTING*", "CHANGELOG*", "NEWS*",
    ".gitignore", ".dir-locals.el", ".elpaignore",
    "*.elc",
    "*.gif", "*.png", "*.svg",
]

STATUS_ICONS = {"success": "✅", "skipped": "⏭️", "failed": "❌"}

# 脚本所在目录即为仓库根目录
SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
PACKAGES_JSON = os.path.join(SCRIPT_DIR, "packages.json")
EXTENSIONS_DIR = os.path.join(SCRIPT_DIR, "site-lisp", "extensions")
# 临时目录：项目同级目录下
TEMP_DIR = os.path.join(os.path.dirname(SCRIPT_DIR), os.path.basename(SCRIPT_DIR) + "_temp")

# 同步并发度
SYNC_MAX_WORKERS = 6
# 下载超时与重试
DOWNLOAD_TIMEOUT = 60
DOWNLOAD_RETRIES = 3


# ---------------------------------------------------------------------------
# 公共工具
# ---------------------------------------------------------------------------

def load_packages():
    """加载 packages.json 配置"""
    if not os.path.exists(PACKAGES_JSON):
        print(f"Error: {PACKAGES_JSON} not found")
        sys.exit(1)
    with open(PACKAGES_JSON, "r", encoding="utf-8") as f:
        return json.load(f)


def save_packages(config):
    """保存 packages.json 配置"""
    with open(PACKAGES_JSON, "w", encoding="utf-8") as f:
        json.dump(config, f, indent=2, ensure_ascii=False)
        f.write("\n")


# ---------------------------------------------------------------------------
# 下载
# ---------------------------------------------------------------------------

def download_with_retry(url, dest, retries=DOWNLOAD_RETRIES, timeout=DOWNLOAD_TIMEOUT):
    """带超时与指数退避重试的下载。"""
    last_err = None
    for attempt in range(retries):
        try:
            req = urllib.request.Request(url, headers={
                "User-Agent": "assistant.py",
                "Cache-Control": "no-cache, no-store",
                "Pragma": "no-cache",
            })
            with urllib.request.urlopen(req, timeout=timeout) as resp, open(dest, "wb") as out:
                shutil.copyfileobj(resp, out)
            return
        except Exception as e:
            last_err = e
            if attempt < retries - 1:
                time.sleep(1.5 ** attempt)   # 1s, 1.5s, 2.25s
    raise last_err


def safe_extract(zf, dest):
    """解压 zip 并防御 zip-slip（路径逃逸）。"""
    dest_real = os.path.realpath(dest)
    for member in zf.infolist():
        target = os.path.realpath(os.path.join(dest, member.filename))
        if target != dest_real and not target.startswith(dest_real + os.sep):
            raise ValueError(f"Unsafe path in archive (zip-slip): {member.filename}")
    zf.extractall(dest)


# ---------------------------------------------------------------------------
# sync 子命令
# ---------------------------------------------------------------------------

def should_ignore(name, ignores):
    """检查文件名是否匹配任意排除规则（大小写敏感，跨平台一致）。"""
    for pattern in ignores:
        if fnmatch.fnmatchcase(name, pattern):
            return True
    return False


def copy_with_files(source_dir, target_dir, files_patterns):
    """指定 files 时：按 glob 模式拷贝匹配的文件和目录"""
    for pattern in files_patterns:
        matches = glob.glob(os.path.join(source_dir, pattern), recursive=True)
        for match in matches:
            rel = os.path.relpath(match, source_dir)
            dest = os.path.join(target_dir, rel)
            if os.path.isdir(match):
                if os.path.exists(dest):
                    shutil.rmtree(dest)
                shutil.copytree(match, dest)
            else:
                os.makedirs(os.path.dirname(dest), exist_ok=True)
                shutil.copy2(match, dest)


def copy_all_with_ignores(source_dir, target_dir, ignores):
    """未指定 files 时：全量拷贝，按排除规则过滤"""

    def _copy_recursive(src, dst, rel_prefix=""):
        for entry in os.listdir(src):
            rel = os.path.join(rel_prefix, entry) if rel_prefix else entry

            if should_ignore(entry, ignores) or should_ignore(rel, ignores):
                continue

            src_path = os.path.join(src, entry)
            dst_path = os.path.join(dst, entry)

            if os.path.isdir(src_path):
                os.makedirs(dst_path, exist_ok=True)
                _copy_recursive(src_path, dst_path, rel)
            else:
                os.makedirs(os.path.dirname(dst_path), exist_ok=True)
                shutil.copy2(src_path, dst_path)

    _copy_recursive(source_dir, target_dir)


def sync_package(pkg, extensions_dir, temp_dir, replace=False):
    """同步单个包：按 branch ref 下载 → 解压 → 安装。返回 (status, message)。

    replace=True 时先删除目标目录再拷贝；默认覆盖拷贝（不删除）。
    """
    name = pkg["name"]

    if pkg.get("manual"):
        return "skipped", "manual"

    repo = pkg.get("repo")
    ref = pkg.get("ref", "master")
    if not repo:
        return "failed", "missing repo"

    url = f"https://github.com/{repo}/archive/refs/heads/{ref}.zip?_={int(time.time())}"
    target_dir = os.path.join(extensions_dir, name)
    pkg_temp = os.path.join(temp_dir, name)

    # 每个包使用独立的临时子目录
    if os.path.exists(pkg_temp):
        shutil.rmtree(pkg_temp, ignore_errors=True)
    os.makedirs(pkg_temp, exist_ok=True)

    zip_path = os.path.join(pkg_temp, "pkg.zip")
    try:
        download_with_retry(url, zip_path)
    except Exception as e:
        return "failed", f"download: {e}"

    try:
        with zipfile.ZipFile(zip_path, "r") as zf:
            safe_extract(zf, pkg_temp)
    except Exception as e:
        return "failed", f"extract: {e}"
    finally:
        if os.path.exists(zip_path):
            os.remove(zip_path)

    extracted = None
    for entry in os.listdir(pkg_temp):
        path = os.path.join(pkg_temp, entry)
        if os.path.isdir(path):
            extracted = path
            break
    if not extracted:
        return "failed", "no directory in archive"

    if replace and os.path.exists(target_dir):
        shutil.rmtree(target_dir, ignore_errors=True)
    os.makedirs(target_dir, exist_ok=True)

    files = pkg.get("files")
    if files:
        copy_with_files(extracted, target_dir, files)
    else:
        ignores = GLOBAL_IGNORES + pkg.get("ignores", [])
        copy_all_with_ignores(extracted, target_dir, ignores)

    return "success", "updated"


def cmd_sync(args):
    """sync 子命令入口"""
    if not args.all and not args.packages:
        print("Error: specify package names or use --all")
        sys.exit(1)

    config = load_packages()
    packages = config["packages"]

    if args.all:
        targets = packages
    else:
        by_name = {p["name"]: p for p in packages}
        targets = []
        for name in args.packages:
            if name in by_name:
                targets.append(by_name[name])
            else:
                print(f"⚠️  {name:<20} not found in packages.json")

    # 创建临时目录（项目同级）
    os.makedirs(TEMP_DIR, exist_ok=True)

    results = {"success": 0, "skipped": 0, "failed": 0}
    lock = threading.Lock()

    def emit(name, status, msg):
        with lock:
            results[status] += 1
            print(f"{STATUS_ICONS[status]} {name:<20} {msg}")

    # 并发同步：每个包独立（独立临时目录、目标目录），无共享可变状态。
    if targets:
        workers = min(SYNC_MAX_WORKERS, len(targets))
        with concurrent.futures.ThreadPoolExecutor(max_workers=workers) as ex:
            future_map = {
                ex.submit(sync_package, p, EXTENSIONS_DIR, TEMP_DIR, args.replace): p
                for p in targets
            }
            for fut in concurrent.futures.as_completed(future_map):
                pkg = future_map[fut]
                try:
                    status, msg = fut.result()
                except Exception as e:
                    status, msg = "failed", str(e)
                emit(pkg["name"], status, msg)

    print("---")
    print(
        f"Done: {results['success']} success, "
        f"{results['skipped']} skipped, "
        f"{results['failed']} failed"
    )

    if args.clean:
        shutil.rmtree(TEMP_DIR, ignore_errors=True)
        print(f"🧹 Cleaned temp directory: {TEMP_DIR}")
    else:
        print(f"📁 Temp directory kept: {TEMP_DIR}")


# ---------------------------------------------------------------------------
# add 子命令
# ---------------------------------------------------------------------------

def cmd_add(args):
    """add 子命令入口"""
    config = load_packages()
    packages = config["packages"]

    repo = args.repo
    # 从 repo 推导 name：company-mode/company-mode -> company-mode
    name = args.name or repo.split("/")[-1].removesuffix(".el")

    # 检查重名
    existing_names = {p["name"] for p in packages}
    if name in existing_names:
        print(f"Error: package '{name}' already exists in packages.json")
        sys.exit(1)

    entry = {"name": name, "repo": repo, "ref": args.ref}

    if args.files:
        entry["files"] = args.files
    if args.ignores:
        entry["ignores"] = args.ignores
    if args.manual:
        entry = {"name": name, "manual": True}
        if args.note:
            entry["note"] = args.note
    elif args.note:
        entry["note"] = args.note

    packages.append(entry)
    save_packages(config)

    print(f"✅ Added '{name}' to packages.json")
    print(f"   repo: {repo}")
    if not args.manual:
        print(f"   ref:  {args.ref}")

    # 添加后自动同步
    if not args.manual and args.sync:
        print(f"\nSyncing {name}...")
        os.makedirs(TEMP_DIR, exist_ok=True)
        try:
            status, msg = sync_package(entry, EXTENSIONS_DIR, TEMP_DIR)
            print(f"{STATUS_ICONS.get(status, '?')} {name:<20} {msg}")
        except Exception as e:
            print(f"❌ {name:<20} {e}")


# ---------------------------------------------------------------------------
# check 子命令
# ---------------------------------------------------------------------------

def cmd_check(args):
    """check 子命令入口：检测本地包是否缺失或多余"""
    config = load_packages()
    packages = config["packages"]

    by_name = {p["name"]: p for p in packages}

    if args.packages:
        # 指定包检查
        for name in args.packages:
            if name not in by_name:
                print(f"⚠️  {name:<20} not found in packages.json")
                continue
            pkg_dir = os.path.join(EXTENSIONS_DIR, name)
            if os.path.isdir(pkg_dir):
                kind = "manual" if by_name[name].get("manual") else "synced"
                print(f"✅ {name:<20} present ({kind})")
            else:
                kind = "manual setup needed" if by_name[name].get("manual") else "need sync"
                print(f"❌ {name:<20} missing ({kind})")
        return

    # 全量检查
    pkg_names = set(by_name.keys())
    local_dirs = set()
    if os.path.isdir(EXTENSIONS_DIR):
        for entry in os.listdir(EXTENSIONS_DIR):
            if os.path.isdir(os.path.join(EXTENSIONS_DIR, entry)):
                local_dirs.add(entry)

    missing = pkg_names - local_dirs
    extra = local_dirs - pkg_names

    manual_pkgs = {p["name"] for p in packages if p.get("manual")}
    missing_manual = missing & manual_pkgs
    missing_sync = missing - manual_pkgs

    if not missing and not extra:
        print("✅ All packages are in sync.")
        return

    if missing_sync:
        print("❌ Missing (need sync):")
        for name in sorted(missing_sync):
            print(f"   - {name}")

    if missing_manual:
        print("⚠️  Missing (manual, need manual setup):")
        for name in sorted(missing_manual):
            print(f"   - {name}")

    if extra:
        print("📦 Extra directories (not in packages.json):")
        for name in sorted(extra):
            print(f"   - {name}")


# ---------------------------------------------------------------------------
# 主入口
# ---------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(
        description="Emacs package management assistant",
    )
    subparsers = parser.add_subparsers(dest="command", help="Available commands")

    # sync
    sync_parser = subparsers.add_parser("sync", help="Sync packages from GitHub")
    sync_parser.add_argument("packages", nargs="*", help="Package names to sync")
    sync_parser.add_argument("--all", action="store_true", help="Sync all packages")
    sync_parser.add_argument("--clean", action="store_true", help="Clean temp directory after sync")
    sync_parser.add_argument(
        "--replace", action="store_true",
        help="Delete target directory before copying (default: overwrite in place)",
    )

    # check
    check_parser = subparsers.add_parser("check", help="Check for missing or extra packages")
    check_parser.add_argument("packages", nargs="*", help="Package names to check (default: all)")

    # add
    add_parser = subparsers.add_parser("add", help="Add a new package")
    add_parser.add_argument("repo", help="GitHub repo (owner/repo)")
    add_parser.add_argument("--name", help="Package name (default: derived from repo)")
    add_parser.add_argument("--ref", default="master", help="Branch name (default: master)")
    add_parser.add_argument("--files", nargs="+", help="File patterns to include")
    add_parser.add_argument("--ignores", nargs="+", help="Extra ignore patterns")
    add_parser.add_argument("--manual", action="store_true", help="Mark as manually maintained")
    add_parser.add_argument("--note", help="Note/description")
    add_parser.add_argument("--sync", action="store_true", help="Sync immediately after adding")

    args = parser.parse_args()

    if not args.command:
        parser.print_help()
        sys.exit(1)

    if args.command == "sync":
        cmd_sync(args)
    elif args.command == "check":
        cmd_check(args)
    elif args.command == "add":
        cmd_add(args)


if __name__ == "__main__":
    main()
