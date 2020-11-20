r"""
  ___          _         ____
 / _ \  _   _ | |_  ___ | __ )  _ __  ___ __      __ ___   ___  _ __
| | | || | | || __|/ _ \|  _ \ | '__|/ _ \\ \ /\ / // __| / _ \| '__|
| |_| || |_| || |_|  __/| |_) || |  | (_) |\ V  V / \__ \|  __/| |
 \__\_\ \__,_| \__|\___||____/ |_|   \___/  \_/\_/  |___/ \___||_|

@author Daniel Csillag (aka. dccsillag)
@what My QuteBrowser configs.
"""

# pylint: skip-file
# flake8: noqa

from shlex import quote


# To silence linter warnings:
try:
    c
    config
except NameError:
    c = None
    config = None

config.load_autoconfig()

# Misc Interface settings (behaviour)
c.tabs.position = 'top'
c.session.lazy_restore = True
c.search.wrap = False
c.tabs.last_close = 'close'
c.tabs.background = True
# c.tabs.wrap = False

# Dark mode
c.colors.webpage.darkmode.enabled = True
c.colors.webpage.darkmode.policy.images = 'smart'
c.colors.webpage.bg = 'black'

# Behaviour
c.content.javascript.can_access_clipboard = True

# Search Engines
c.url.searchengines = {
    'DEFAULT': 'https://duckduckgo.com/?q={}',
    'aw': 'https://wiki.archlinux.org/index.php?search={}',
    'go': 'https://www.google.com/search?q={}',
    'lib': 'http://gen.lib.rus.ec/search.php?req={}',
}

# Keybindings
config.bind('m', 'enter-mode set_mark')
config.bind('M', 'nop')
config.bind('<Ctrl-N>', 'completion-item-focus --history next', mode='command')
config.bind('<Ctrl-P>', 'completion-item-focus --history prev', mode='command')
config.bind('<', 'tab-move -')
config.bind('>', 'tab-move +')
config.bind(",m", ('spawn sh -c ' + quote('playerctl pause; mpv --force-window=immediate --save-position-on-quit URL')).replace('URL', '{url}'))

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

# Theme / Colors
c.hints.border = '1px solid #000000'
c.tabs.indicator.width = 0
c.tabs.favicons.show = 'never'
