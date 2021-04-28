import sys, os
print("imported sys & os")

from pprint import pprint as _pp

def __displayhook(value):
    if value is None:
        return
    __builtins__._ = value
    _pp(value)

sys.displayhook = __displayhook
