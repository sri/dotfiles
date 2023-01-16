import sys, os
print("imported sys & os")

# try:
#     from rich import pretty as _pp
#     _pp.install()
#     _pp = print
# except ImportError:
#     from pprint import pprint as _pp

# def __displayhook(value):
#     if value is None:
#         return
#     __builtins__._ = value
#     _pp(value)

# sys.displayhook = __displayhook

running_under_ipython = hasattr(__builtins__, '__IPYTHON__')

if running_under_ipython:
    from IPython.core.magic import register_line_magic

    @register_line_magic("rev")
    def lmagic(line):
        "Rev line"
        return line[::-1]

    from IPython.terminal.prompts import Prompts, Token
    import subprocess
    import pathlib

    user_home = str(pathlib.Path.home())

    class MyPrompt(Prompts):
        def in_prompt_tokens(self, cli=None):
            cwd = os.getcwd()
            if cwd.startswith(user_home):
                cwd = '~' + cwd.removeprefix(user_home)

            (in_git_repo, current_git_branch) = subprocess.getstatusoutput(
                "git rev-parse --abbrev-ref HEAD")

            result = [(Token, cwd)]
            if in_git_repo == 0:
                result.append((Token, f"({current_git_branch})"))
            result.append((Token.Prompt, '>>> '))
            return result

    ip = get_ipython()
    ip.prompts = MyPrompt(ip)
