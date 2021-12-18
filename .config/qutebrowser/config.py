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

# Misc Interface settings (behaviour) {{{

c.tabs.position = 'top'
c.session.lazy_restore = True
c.search.wrap = False
c.tabs.last_close = 'close'
c.tabs.background = True
# c.tabs.wrap = False

# }}}

# Behaviour {{{

c.content.javascript.can_access_clipboard = True
c.url.default_page = 'about:blank'
c.content.autoplay = False
c.hints.chars = "fdsewg"

# }}}

# Adblocking {{{

c.content.blocking.method = 'adblock'
c.content.blocking.adblock.lists = ["https://easylist.to/easylist/easylist.txt",
                                    "https://easylist.to/easylist/easyprivacy.txt",
                                    "https://secure.fanboy.co.nz/fanboy-annoyance.txt",
                                    "https://easylist-downloads.adblockplus.org/antiadblockfilters.txt",
                                    "https://www.fanboy.co.nz/fanboy-espanol.txt"]

# }}}

# Search Engines {{{

c.url.searchengines = {
    'DEFAULT': 'https://www.google.com/search?q={}',
    'brv': 'https://search.brave.com/search?q={}&source=web',
    'ddg': 'https://duckduckgo.com/?q={}',
    'aw': 'https://wiki.archlinux.org/index.php?search={}',
    # 'go': 'https://www.google.com/search?q={}',
    'lib': 'http://gen.lib.rus.ec/search.php?req={}',
}

# }}}

# Keybindings {{{

config.bind('m', 'mode-enter set_mark')
config.bind('M', 'nop')
config.bind('<Ctrl-N>', 'completion-item-focus --history next', mode='command')
config.bind('<Ctrl-P>', 'completion-item-focus --history prev', mode='command')
config.bind('<', 'tab-move -')
config.bind('>', 'tab-move +')

MPV_COMMAND = 'mpv --script-opts=ytdl_hook-ytdl_path=yt-dlp "--ytdl-format=best[width<=1920]" --force-window=immediate --save-position-on-quit URL'
config.bind(",m", ('spawn sh -c ' + quote(MPV_COMMAND)).replace('URL', '{url}'))

# }}}

# Statusbar {{{

c.statusbar.widgets = ['keypress', 'progress', 'history', 'scroll', 'tabs']

# }}}

# Theme / Colors {{{

c.colors.completion.fg                          = '#ffffff'
c.colors.completion.category.fg                 = '#ffffff'
c.colors.completion.category.bg                 = '#4b5356'
c.colors.completion.category.border.bottom      = '#000000'
c.colors.completion.category.border.top         = '#000000'
c.colors.completion.even.bg                     = '#2b3336'
c.colors.completion.odd.bg                      = '#2b3336'
c.colors.completion.item.selected.fg            = '#ffffff'
c.colors.completion.item.selected.bg            = '#3b4346'
c.colors.completion.item.selected.border.bottom = '#00333333'
c.colors.completion.item.selected.border.top    = '#00333333'
c.colors.completion.item.selected.match.fg      = '#FC994F'
c.colors.completion.match.fg                    = '#FC994F'
c.colors.completion.scrollbar.fg                = '#dddddd'
c.colors.completion.scrollbar.bg                = '#2b3336'
c.colors.contextmenu.menu.fg                    = '#ffffff'
c.colors.contextmenu.menu.bg                    = '#1b2326'
c.colors.contextmenu.disabled.fg                = '#aaaaaa'
c.colors.contextmenu.disabled.bg                = '#1b2326'
c.colors.contextmenu.selected.fg                = '#ffffff'
c.colors.contextmenu.selected.bg                = '#3b4346'
c.colors.downloads.bar.bg                       = '#333333'
c.colors.downloads.error.fg                     = '#ffffff'
c.colors.downloads.error.bg                     = '#E71B1B'
c.colors.downloads.start.fg                     = '#ffffff'
c.colors.downloads.start.bg                     = '#333333'
c.colors.downloads.stop.fg                      = '#ffffff'
c.colors.downloads.stop.bg                      = '#075C00'
c.colors.downloads.system.fg                    = 'hsv'
c.colors.downloads.system.bg                    = 'hsv'
c.colors.hints.fg                               = '#222222'
c.colors.hints.bg                               = '#CCeedd00'
c.colors.hints.match.fg                         = '#666666'
c.colors.keyhint.fg                             = '#ffffffff'
c.colors.keyhint.bg                             = '#aa333333'
c.colors.messages.error.fg                      = '#FF6666'
c.colors.messages.error.bg                      = '#1b2326'
c.colors.messages.error.border                  = '#000000'
c.colors.messages.warning.fg                    = '#FFE742'
c.colors.messages.warning.bg                    = '#1b2326'
c.colors.messages.warning.border                = '#000000'
c.colors.messages.info.fg                       = '#ffffff'
c.colors.messages.info.bg                       = '#1b2326'
c.colors.messages.info.border                   = '#000000'
c.colors.prompts.fg                             = '#ffffff'
c.colors.prompts.bg                             = '#1b2326'
c.colors.prompts.border                         = '#dddddd'
c.colors.prompts.selected.bg                    = '#803b4346'
c.colors.statusbar.caret.fg                     = '#ffffff'
c.colors.statusbar.caret.bg                     = '#80101C78'
c.colors.statusbar.caret.selection.fg           = '#ffffff'
c.colors.statusbar.caret.selection.bg           = '#802E1BBC'
c.colors.statusbar.command.fg                   = '#ffffff'
c.colors.statusbar.command.bg                   = '#80333333'
c.colors.statusbar.command.private.fg           = '#ffffff'
c.colors.statusbar.command.private.bg           = '#805B1282'
c.colors.statusbar.insert.fg                    = '#000000'
c.colors.statusbar.insert.bg                    = '#803DE305'
c.colors.statusbar.normal.fg                    = '#ffffff'
c.colors.statusbar.normal.bg                    = '#80333333'
c.colors.statusbar.passthrough.fg               = '#ffffff'
c.colors.statusbar.passthrough.bg               = '#80CF009F'
c.colors.statusbar.private.fg                   = '#ffffff'
c.colors.statusbar.private.bg                   = '#805B1282'
c.colors.statusbar.progress.bg                  = '#80999999'
c.colors.statusbar.url.fg                       = '#ffffff'
c.colors.statusbar.url.error.fg                 = '#FF7B7B'
c.colors.statusbar.url.hover.fg                 = '#808080'
c.colors.statusbar.url.success.http.fg          = '#ddddff'
c.colors.statusbar.url.success.https.fg         = '#ddddff'
c.colors.statusbar.url.warn.fg                  = '#FFFCB1'
c.colors.tabs.bar.bg                            = '#80555558'
c.colors.tabs.even.fg                           = '#ffffff'
c.colors.tabs.even.bg                           = '#80555558'
c.colors.tabs.odd.fg                            = '#ffffff'
c.colors.tabs.odd.bg                            = '#80555558'
c.colors.tabs.indicator.error                   = '#00D80707'
c.colors.tabs.indicator.start                   = '#00000000'
c.colors.tabs.indicator.stop                    = '#00000000'
c.colors.tabs.indicator.system                  = 'none'
c.colors.tabs.pinned.even.fg                    = '#000000'
c.colors.tabs.pinned.even.bg                    = 'seagreen'
c.colors.tabs.pinned.odd.fg                     = '#000000'
c.colors.tabs.pinned.odd.bg                     = 'seagreen'
c.colors.tabs.pinned.selected.even.fg           = '#000000'
c.colors.tabs.pinned.selected.even.bg           = 'darkseagreen'
c.colors.tabs.pinned.selected.odd.fg            = '#000000'
c.colors.tabs.pinned.selected.odd.bg            = 'darkseagreen'
c.colors.tabs.selected.even.fg                  = '#ffffff'
c.colors.tabs.selected.even.bg                  = '#80333333'
c.colors.tabs.selected.odd.fg                   = '#ffffff'
c.colors.tabs.selected.odd.bg                   = '#80333333'
c.colors.webpage.bg                             = '#1b2326'
c.window.transparent                            = False

# }}}

# Fonts {{{

UI_FONT = '13px FantasqueSansMono Nerd Font'

c.fonts.completion.category = 'bold ' + UI_FONT
c.fonts.completion.entry = UI_FONT
c.fonts.debug_console = UI_FONT
c.fonts.downloads = 'italic ' + UI_FONT
c.fonts.hints = UI_FONT
c.fonts.keyhint = UI_FONT
c.fonts.messages.error = 'bold ' + UI_FONT
c.fonts.messages.info = UI_FONT
c.fonts.messages.warning = 'italic ' + UI_FONT
c.fonts.prompts = UI_FONT
c.fonts.statusbar = UI_FONT
c.fonts.tabs.selected = 'bold ' + UI_FONT
c.fonts.tabs.unselected = UI_FONT
c.fonts.contextmenu = UI_FONT
c.fonts.default_family = 'Lato Regular'
c.fonts.default_size = '13px'
c.fonts.web.family.fixed = 'FantasqueSansMono Nerd Font'
c.fonts.web.size.default_fixed = 13
c.fonts.web.family.sans_serif = 'Lato Regular'
c.fonts.web.family.serif = 'LiterationSerif Nerd Font'
c.fonts.web.family.standard = 'Lato Regular'
c.fonts.web.size.default = 13
c.fonts.web.size.minimum = 6
c.fonts.web.size.minimum_logical = 6
# c.fonts.web.family.cursive = ...
# c.fonts.web.family.fantasy = ...

# }}}

# Other appearance stuff {{{

c.hints.border = '1px solid #000000'
c.tabs.indicator.width = 0
c.tabs.favicons.show = 'never'

c.tabs.padding = {"bottom": 5, "top": 5, "left": 5, "right": 5}
c.statusbar.padding = {"bottom": 5, "top": 5, "left": 5, "right": 5}
c.tabs.title.format = "{audio}{perc}{current_title}"

# # Dark mode {{{

c.colors.webpage.darkmode.enabled = True
c.colors.webpage.darkmode.policy.images = 'smart'
c.colors.webpage.bg = 'black'

# }}}

# }}}

# vim: fdm=marker
