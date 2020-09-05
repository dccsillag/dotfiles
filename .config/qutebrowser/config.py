r"""
  ___          _         ____
 / _ \  _   _ | |_  ___ | __ )  _ __  ___ __      __ ___   ___  _ __
| | | || | | || __|/ _ \|  _ \ | '__|/ _ \\ \ /\ / // __| / _ \| '__|
| |_| || |_| || |_|  __/| |_) || |  | (_) |\ V  V / \__ \|  __/| |
 \__\_\ \__,_| \__|\___||____/ |_|   \___/  \_/\_/  |___/ \___||_|

@author Daniel Csillag (aka. dccsillag)
@what My QuteBrowser configs.
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

c.session.lazy_restore = True

# c.colors.webpage.darkmode.enabled = False
c.colors.webpage.darkmode.enabled = True
c.colors.webpage.darkmode.policy.images = 'smart'
c.colors.webpage.bg = 'black'

c.content.javascript.can_access_clipboard = True

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
# config.bind(",m", ('spawn sh -c ' + quote('WID=$(xdo id); playerctl pause; xdo hide; mpv --force-window=immediate --save-position-on-quit URL; xdo show "$WID"')).replace('URL', '{url}'))
config.bind(",m", ('spawn sh -c ' + quote('playerctl pause; mpv --force-window=immediate --save-position-on-quit URL')).replace('URL', '{url}'))

# Theme
# config.source(os.path.expanduser('~/.config/qutebrowser/nord-qutebrowser.py'))
dracula_theme(c, {
    'spacing': {
        'vertical': 2,
        'horizontal': 4,
    }
})
# # Override some colors
c.colors.hints.bg = '#CCeedd00'
c.colors.hints.fg = '#222222'
c.hints.border = '1px solid #000000'
c.colors.hints.match.fg = '#666666'

c.tabs.indicator.width = 0
c.tabs.favicons.show = 'never'

# Statusbar

c.statusbar.widgets = ['keypress', 'url', 'scroll', 'tabs', 'progress']

# Fonts

UI_FONT = '12pt FantasqueSansMono Nerd Font'
BIGGER_UI_FONT = '14pt FantasqueSansMono Nerd Font'

c.fonts.completion.category = 'bold ' + BIGGER_UI_FONT
c.fonts.completion.entry = UI_FONT
# c.fonts.contextmenu (keep default)
c.fonts.debug_console = UI_FONT
c.fonts.downloads = 'italic ' + UI_FONT
c.fonts.hints = 'bold ' + UI_FONT
c.fonts.keyhint = 'bold ' + UI_FONT
c.fonts.messages.error = 'bold ' + UI_FONT
c.fonts.messages.info = UI_FONT
c.fonts.messages.warning = 'italic ' + UI_FONT
c.fonts.prompts = UI_FONT
c.fonts.statusbar = UI_FONT
c.fonts.tabs.selected = 'bold ' + UI_FONT
c.fonts.tabs.unselected = UI_FONT
