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

try:
    __IPYTHON__
    running_under_ipython = True
except NameError:
    running_under_ipython = False

if running_under_ipython:
    from IPython.core.magic import register_line_magic

    @register_line_magic("rev")
    def lmagic(line):
        "Rev line"
        return line[::-1]
