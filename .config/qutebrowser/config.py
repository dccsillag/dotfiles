from qutebrowser.api import interceptor
import sys, os

c.tabs.position = 'top'

# c.colors.webpage.darkmode.enabled = True
c.colors.webpage.darkmode.enabled = False

c.tabs.last_close = 'close'
# c.tabs.wrap = False
c.tabs.background = True

c.url.searchengines = {
    'DEFAULT': 'https://duckduckgo.com/?q={}',
    'aw': 'https://wiki.archlinux.org/index.php?search={}',
    'go': 'https://www.google.com/search?q={}',
    'lib': 'http://gen.lib.rus.ec/search.php?req={}',
}

# Adblocker
# sys.path.append(os.path.join(sys.path[0], 'jblock'))

config.source(os.path.expanduser('~/.config/qutebrowser/nord-qutebrowser.py'))

config.load_autoconfig()

# config.bind(",m", 'spawn devour mpv --force-window=immediate {url}')
config.bind(",m", 'spawn sh -c \'WID=$(xdo id); xdo hide; mpv --force-window=immediate --keep-open=yes --save-position-on-quit {url}; xdo show "$WID"\'')
