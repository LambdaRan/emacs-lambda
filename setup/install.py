#!/usr/bin/env python3
"""
install.py - emacs-lambda 安装脚本

用法:
    python setup/install.py              # 交互模式（逐步确认）
    python setup/install.py --yes        # 非交互模式（全部自动确认）
    python setup/install.py --dry-run    # 仅显示将要执行的操作
"""

import argparse
import os
import shutil
import subprocess
import sys

# Windows GBK 编码兼容
import io
sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding="utf-8", errors="replace")
sys.stderr = io.TextIOWrapper(sys.stderr.buffer, encoding="utf-8", errors="replace")

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
REPO_ROOT = os.path.dirname(SCRIPT_DIR)

# 要复制到用户目录的文件
FILE_COPIES = [
    {
        "src": os.path.join(SCRIPT_DIR, "site-start.el"),
        "dst_name": ".emacs",
        "dst_dir": os.path.expanduser("~"),
        "desc": "Emacs 入口文件",
    },
    {
        "src": os.path.join(SCRIPT_DIR, "early-init.el"),
        "dst_name": "early-init.el",
        "dst_dir": os.path.join(os.path.expanduser("~"), ".emacs.d"),
        "desc": "Emacs early-init 文件",
    },
    {
        "src": os.path.join(SCRIPT_DIR, ".ctags"),
        "dst_name": ".ctags",
        "dst_dir": os.path.expanduser("~"),
        "desc": "Universal Ctags 配置",
    },
    {
        "src": os.path.join(SCRIPT_DIR, "dot-rgignore"),
        "dst_name": ".rgignore",
        "dst_dir": os.path.expanduser("~"),
        "desc": "ripgrep 忽略规则",
    },
]

# 需要手动安装的外部工具
EXTERNAL_TOOLS = [
    {"name": "ripgrep", "cmd": "rg --version", "install": "scoop install ripgrep"},
    {"name": "fd", "cmd": "fd --version", "install": "scoop install fd"},
    {"name": "ctags", "cmd": "ctags --version", "install": "从 https://github.com/universal-ctags/ctags-win32 下载"},
]


def run_cmd(cmd, check=False):
    """运行命令并返回 (returncode, stdout)"""
    try:
        result = subprocess.run(
            cmd, shell=True, capture_output=True, text=True, timeout=30
        )
        return result.returncode, result.stdout.strip()
    except (subprocess.TimeoutExpired, Exception) as e:
        return -1, str(e)


def confirm(prompt, auto_yes=False):
    """交互确认"""
    if auto_yes:
        print(f"{prompt} [自动确认]")
        return True
    try:
        answer = input(f"{prompt} [Y/n] ").strip().lower()
        return answer in ("", "y", "yes")
    except (EOFError, KeyboardInterrupt):
        print()
        return False


def check_tool(tool):
    """检查外部工具是否已安装"""
    code, output = run_cmd(tool["cmd"])
    if code == 0:
        first_line = output.split("\n")[0] if output else "已安装"
        return True, first_line
    return False, None


def step_copy_files(dry_run=False, auto_yes=False):
    """步骤1: 复制配置文件"""
    print("\n" + "=" * 60)
    print("步骤 1: 复制配置文件")
    print("=" * 60)

    copied = 0
    skipped = 0

    for item in FILE_COPIES:
        src = item["src"]
        dst_dir = item["dst_dir"]
        dst_path = os.path.join(dst_dir, item["dst_name"])

        if not os.path.exists(src):
            print(f"  ⚠️  源文件不存在: {src}")
            continue

        # 检查目标是否已存在
        if os.path.exists(dst_path):
            # 比较文件内容
            try:
                if open(src, "rb").read() == open(dst_path, "rb").read():
                    print(f"  ⏭️  {item['desc']} ({item['dst_name']}) 已存在且内容相同，跳过")
                    skipped += 1
                    continue
            except Exception:
                pass

            if not confirm(f"  ⚠️  {dst_path} 已存在，是否覆盖？", auto_yes):
                print(f"  ⏭️  跳过 {item['dst_name']}")
                skipped += 1
                continue

        if dry_run:
            print(f"  🔍 将复制: {src} -> {dst_path}")
            copied += 1
            continue

        # 确保目标目录存在
        os.makedirs(dst_dir, exist_ok=True)

        try:
            shutil.copy2(src, dst_path)
            print(f"  ✅ {item['desc']} -> {dst_path}")
            copied += 1
        except Exception as e:
            print(f"  ❌ 复制 {item['dst_name']} 失败: {e}")

    print(f"\n  复制: {copied}, 跳过: {skipped}")
    return copied > 0 or dry_run


def step_sync_packages(dry_run=False, auto_yes=False):
    """步骤2: 同步依赖包"""
    print("\n" + "=" * 60)
    print("步骤 2: 同步依赖包")
    print("=" * 60)

    assistant_py = os.path.join(REPO_ROOT, "assistant.py")
    if not os.path.exists(assistant_py):
        print(f"  ❌ assistant.py 不存在: {assistant_py}")
        return False

    # 检查 extensions 目录是否已有内容
    ext_dir = os.path.join(REPO_ROOT, "site-lisp", "extensions")
    if os.path.isdir(ext_dir):
        existing = [d for d in os.listdir(ext_dir) if os.path.isdir(os.path.join(ext_dir, d))]
        if existing:
            print(f"  ℹ️  extensions/ 下已有 {len(existing)} 个包目录")
            if not confirm("  是否重新同步所有包？", auto_yes):
                print("  ⏭️  跳过包同步")
                return True

    if dry_run:
        print("  🔍 将执行: python assistant.py sync --all")
        return True

    if not confirm("  是否同步所有依赖包？", auto_yes):
        print("  ⏭️  跳过包同步")
        return True

    print("  ⏳ 正在同步（可能需要几分钟）...")
    code, output = run_cmd(
        f'python "{assistant_py}" sync --all --clean',
    )
    if output:
        # 打印 assistant.py 的输出
        for line in output.split("\n"):
            print(f"    {line}")

    if code == 0:
        print("  ✅ 包同步完成")
        return True
    else:
        print("  ❌ 包同步失败，请稍后手动执行: python assistant.py sync --all")
        return False


def step_check_tools():
    """步骤3: 检查外部工具"""
    print("\n" + "=" * 60)
    print("步骤 3: 检查外部工具")
    print("=" * 60)

    all_ok = True
    for tool in EXTERNAL_TOOLS:
        installed, version = check_tool(tool)
        if installed:
            print(f"  ✅ {tool['name']}: {version}")
        else:
            print(f"  ❌ {tool['name']}: 未安装")
            print(f"     安装方式: {tool['install']}")
            all_ok = False

    # 检查 Emacs 中的字体
    print("\n  ℹ️  首次启动 Emacs 后，请执行以下命令安装图标字体:")
    print("     M-x all-the-icons-install-fonts")
    print("  ℹ️  推荐安装 JetBrains Mono 字体:")
    print("     https://www.jetbrains.com/zh-cn/lp/mono/")

    return all_ok


def main():
    parser = argparse.ArgumentParser(description="emacs-lambda 安装脚本")
    parser.add_argument("--yes", "-y", action="store_true", help="非交互模式，自动确认所有操作")
    parser.add_argument("--dry-run", "-n", action="store_true", help="仅显示将要执行的操作")
    args = parser.parse_args()

    print("emacs-lambda 安装脚本")
    print(f"仓库路径: {REPO_ROOT}")

    if args.dry_run:
        print("模式: 试运行（不执行任何操作）")
    elif args.yes:
        print("模式: 自动确认")

    # 步骤1: 复制文件
    step_copy_files(dry_run=args.dry_run, auto_yes=args.yes)

    # 步骤2: 同步包
    step_sync_packages(dry_run=args.dry_run, auto_yes=args.yes)

    # 步骤3: 检查外部工具（只读检查，无需确认）
    tools_ok = step_check_tools()

    # 总结
    print("\n" + "=" * 60)
    print("安装完成")
    print("=" * 60)

    if not tools_ok:
        print("  ⚠️  部分外部工具未安装，请手动安装后重启 Emacs")

    if not args.dry_run:
        print("  🚀 启动 Emacs 即可使用")
    else:
        print("  🔍 以上为试运行结果，未执行任何操作")


if __name__ == "__main__":
    main()
