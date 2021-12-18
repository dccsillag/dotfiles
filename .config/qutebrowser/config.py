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

# TODO: c.bindings.default = {}

config.bind('m', 'mode-enter set_mark')
config.bind('M', 'nop')
config.bind('<Ctrl-N>', 'completion-item-focus --history next', mode='command')
config.bind('<Ctrl-P>', 'completion-item-focus --history prev', mode='command')
config.bind('<', 'tab-move -')
config.bind('>', 'tab-move +')
config.bind('%', 'open qute://back')

MPV_COMMAND = 'mpv --script-opts=ytdl_hook-ytdl_path=yt-dlp "--ytdl-format=best[width<=1920]" --force-window=immediate --save-position-on-quit URL'
config.bind(",m", ('spawn sh -c ' + quote(MPV_COMMAND)).replace('URL', '{url}'))

# }}}

# Statusbar {{{

c.statusbar.widgets = ['keypress', 'progress', 'history', 'scroll', 'tabs']

# }}}

# Theme / Colors {{{

TEXTFG = '#ffffff'
DISABLEDTEXTFG = '#aaaaaa'
STDBG = '#282C33'
SURFACEBG = '#31363f'
SURFACEBG2 = '#3b4049'

c.colors.completion.fg                          = TEXTFG
c.colors.completion.category.fg                 = TEXTFG
c.colors.completion.category.bg                 = SURFACEBG
c.colors.completion.category.border.bottom      = SURFACEBG
c.colors.completion.category.border.top         = SURFACEBG
c.colors.completion.even.bg                     = SURFACEBG
c.colors.completion.odd.bg                      = SURFACEBG
c.colors.completion.item.selected.fg            = TEXTFG
c.colors.completion.item.selected.bg            = SURFACEBG2
c.colors.completion.item.selected.border.bottom = SURFACEBG2
c.colors.completion.item.selected.border.top    = SURFACEBG2
c.colors.completion.item.selected.match.fg      = '#FC994F' # TODO
c.colors.completion.match.fg                    = '#FC994F' # TODO
c.colors.completion.scrollbar.fg                = TEXTFG
c.colors.completion.scrollbar.bg                = SURFACEBG2
c.colors.contextmenu.menu.fg                    = TEXTFG
c.colors.contextmenu.menu.bg                    = SURFACEBG
c.colors.contextmenu.disabled.fg                = DISABLEDTEXTFG
c.colors.contextmenu.disabled.bg                = SURFACEBG
c.colors.contextmenu.selected.fg                = TEXTFG
c.colors.contextmenu.selected.bg                = SURFACEBG2

c.colors.downloads.bar.bg                       = SURFACEBG
c.colors.downloads.error.fg                     = TEXTFG
c.colors.downloads.error.bg                     = '#E71B1B' # TODO
c.colors.downloads.start.fg                     = TEXTFG
c.colors.downloads.start.bg                     = SURFACEBG2 # TODO
c.colors.downloads.stop.fg                      = TEXTFG
c.colors.downloads.stop.bg                      = '#075C00' # TODO
c.colors.downloads.system.fg                    = 'none'
c.colors.downloads.system.bg                    = 'none'
c.colors.hints.fg                               = '#000000'
c.colors.hints.bg                               = '#eedd00'  # TODO
c.colors.hints.match.fg                         = c.colors.hints.bg
c.colors.keyhint.fg                             = TEXTFG
c.colors.keyhint.bg                             = SURFACEBG
c.colors.messages.error.fg                      = '#FF6666'  # TODO
c.colors.messages.error.bg                      = SURFACEBG
c.colors.messages.error.border                  = SURFACEBG
c.colors.messages.warning.fg                    = '#FFE742'  # TODO
c.colors.messages.warning.bg                    = SURFACEBG
c.colors.messages.warning.border                = SURFACEBG
c.colors.messages.info.fg                       = TEXTFG
c.colors.messages.info.bg                       = SURFACEBG
c.colors.messages.info.border                   = SURFACEBG
c.colors.prompts.fg                             = TEXTFG
c.colors.prompts.bg                             = SURFACEBG
c.colors.prompts.border                         = TEXTFG
c.colors.prompts.selected.bg                    = SURFACEBG2
c.colors.statusbar.caret.fg                     = TEXTFG
c.colors.statusbar.caret.bg                     = '#80101C78'  # TODO
c.colors.statusbar.caret.selection.fg           = TEXTFG
c.colors.statusbar.caret.selection.bg           = '#802E1BBC'  # TODO
c.colors.statusbar.command.fg                   = TEXTFG
c.colors.statusbar.command.bg                   = SURFACEBG
c.colors.statusbar.command.private.fg           = TEXTFG
c.colors.statusbar.command.private.bg           = SURFACEBG
c.colors.statusbar.insert.fg                    = '#000000'  # TODO
c.colors.statusbar.insert.bg                    = '#803DE305'  # TODO
c.colors.statusbar.normal.fg                    = TEXTFG
c.colors.statusbar.normal.bg                    = SURFACEBG
c.colors.statusbar.passthrough.fg               = TEXTFG
c.colors.statusbar.passthrough.bg               = '#80CF009F'  # TODO
c.colors.statusbar.private.fg                   = TEXTFG
c.colors.statusbar.private.bg                   = '#805B1282'  # TODO
c.colors.statusbar.progress.bg                  = '#80999999'  # TODO
c.colors.statusbar.url.fg                       = TEXTFG
c.colors.statusbar.url.error.fg                 = TEXTFG
c.colors.statusbar.url.hover.fg                 = TEXTFG
c.colors.statusbar.url.success.http.fg          = TEXTFG
c.colors.statusbar.url.success.https.fg         = TEXTFG
c.colors.statusbar.url.warn.fg                  = TEXTFG
c.colors.tabs.bar.bg                            = SURFACEBG
c.colors.tabs.even.fg                           = TEXTFG
c.colors.tabs.even.bg                           = SURFACEBG
c.colors.tabs.odd.fg                            = TEXTFG
c.colors.tabs.odd.bg                            = SURFACEBG
c.colors.tabs.indicator.error                   = '#00D80707'  # TODO
c.colors.tabs.indicator.start                   = '#00000000'  # TODO
c.colors.tabs.indicator.stop                    = '#00000000'  # TODO
c.colors.tabs.indicator.system                  = 'none'  # TODO
c.colors.tabs.pinned.even.fg                    = '#000000'  # TODO
c.colors.tabs.pinned.even.bg                    = 'seagreen'  # TODO
c.colors.tabs.pinned.odd.fg                     = '#000000'  # TODO
c.colors.tabs.pinned.odd.bg                     = 'seagreen'  # TODO
c.colors.tabs.pinned.selected.even.fg           = '#000000'  # TODO
c.colors.tabs.pinned.selected.even.bg           = 'darkseagreen'  # TODO
c.colors.tabs.pinned.selected.odd.fg            = '#000000'  # TODO
c.colors.tabs.pinned.selected.odd.bg            = 'darkseagreen'  # TODO
c.colors.tabs.selected.even.fg                  = TEXTFG
c.colors.tabs.selected.even.bg                  = SURFACEBG2
c.colors.tabs.selected.odd.fg                   = TEXTFG
c.colors.tabs.selected.odd.bg                   = SURFACEBG2
c.colors.webpage.bg                             = STDBG
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

c.tabs.padding = {"bottom": 5, "top": 5, "left": 7, "right": 7}
c.statusbar.padding = {"bottom": 5, "top": 5, "left": 7, "right": 7}
c.tabs.title.format = "{perc}{current_title}"

c.completion.scrollbar.width = 0

# # Dark mode {{{

c.colors.webpage.darkmode.enabled = True
c.colors.webpage.darkmode.policy.images = 'smart'
c.colors.webpage.bg = STDBG

# }}}

# }}}

# }}}

# vim: fdm=marker
