"""
My qutebrowser config.
"""

from shlex import quote

from dracula.draw import blood as dracula_theme


# To silence linter warnings:
try:
    c
    config
except NameError:
    c = None
    config = None

c.tabs.position = 'top'

# c.colors.webpage.darkmode.enabled = False
c.colors.webpage.darkmode.enabled = True
c.colors.webpage.darkmode.policy.images = 'smart'

c.tabs.last_close = 'close'
# c.tabs.wrap = False
c.tabs.background = True

c.search.wrap = False

c.url.searchengines = {
    'DEFAULT': 'https://duckduckgo.com/?q={}',
    'aw': 'https://wiki.archlinux.org/index.php?search={}',
    'go': 'https://www.google.com/search?q={}',
    'lib': 'http://gen.lib.rus.ec/search.php?req={}',
}

# Adblocker
# sys.path.append(os.path.join(sys.path[0], 'jblock'))

config.load_autoconfig()

# Keybindings

config.bind('m', 'enter-mode set_mark')
config.bind('M', 'nop')

config.bind('<Ctrl-N>', 'completion-item-focus --history next', mode='command')
config.bind('<Ctrl-P>', 'completion-item-focus --history prev', mode='command')

config.bind('<', 'tab-move -')
config.bind('>', 'tab-move +')

# config.bind(",m", 'spawn devour mpv --force-window=immediate {url}')
config.bind(",m", ('spawn sh -c ' + quote('WID=$(xdo id); playerctl pause; xdo hide; mpv --force-window=immediate --save-position-on-quit URL; xdo show "$WID"')).replace('URL', '{url}'))

# Theme
# config.source(os.path.expanduser('~/.config/qutebrowser/nord-qutebrowser.py'))
dracula_theme(c, {
    'spacing': {
        'vertical': 2,
        'horizontal': 4,
    }
})

c.tabs.indicator.width = 0

# Statusbar

c.statusbar.widgets = ['keypress', 'url', 'scroll', 'tabs', 'progress']
