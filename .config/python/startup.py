import atexit
import os
import readline

histfile = os.path.expanduser('~/.local/misc/python/history')
if not os.path.exists(os.path.dirname(histfile)):
    os.makedirs(os.path.dirname(histfile))
try:
    readline.read_history_file(histfile)
    readline.set_history_length(1000)
except FileNotFoundError:
    pass

atexit.register(readline.write_history_file, histfile)
