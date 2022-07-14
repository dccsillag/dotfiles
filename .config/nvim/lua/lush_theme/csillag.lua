-- Enable lush.ify on this file, run:
--
--  `:Lushify`
--
--  or
--
--  `:lua require('lush').ify()`

local lush = require('lush')
local hsl = lush.hsl

local background = hsl(218, 22, 20)
local background_popup = background.lighten(20)
local background_gutter = hsl(200, 10, 20)
local foreground = hsl(0, 0, 100)
local foreground_ui = hsl(0, 0, 50)
local foreground_ui_highlight = hsl(200, 40, 85)
local foreground_virtual = hsl(200, 80, 80)
local cursor_highlight = background.lighten(10)
local background_search = hsl(50, 100, 50)

local diff_add = hsl(120, 100, 50)
local diff_change = hsl(39, 100, 50)
local diff_delete = hsl(22, 100, 56)

local error = hsl(0, 100, 50)
local warning = hsl(60, 100, 50)

local syntax_comment = foreground.darken(25)
local syntax_string = hsl(120, 40, 70)
local syntax_number = hsl(190, 60, 70)
local syntax_statement = hsl(20, 80, 72)
local syntax_label = hsl(280, 90, 80)
local syntax_operator = foreground
local syntax_attribute = foreground.darken(10)
local syntax_special = hsl(190, 60, 70)

local theme = lush(function()
  return {
    -- The following are all the Neovim default highlight groups from the docs
    -- as of 0.5.0-nightly-446, to aid your theme creation. Your themes should
    -- probably style all of these at a bare minimum.
    --
    -- Referenced/linked groups must come before being referenced/lined,
    -- so the order shown ((mostly) alphabetical) is likely
    -- not the order you will end up with.
    --
    -- You can uncomment these and leave them empty to disable any
    -- styling for that group (meaning they mostly get styled as Normal)
    -- or leave them commented to apply vims default colouring or linking.

    Ignore                            { }, -- (preferred) left blank, hidden  |hl-Ignore|
    Normal                            { fg = foreground, bg = background }, -- normal text
    NormalNC                          { fg = foreground, bg = background }, -- normal text in non-current windows
    NormalFloat                       { fg = foreground, bg = background_popup }, -- Normal text in floating windows.
    Pmenu                             { fg = foreground, bg = background_popup }, -- Popup menu: normal item.
    PmenuSel                          { fg = foreground, bg = background_popup, gui = "standout,bold" }, -- Popup menu: selected item.
    PmenuSbar                         { fg = foreground, bg = background_popup }, -- Popup menu: scrollbar.
    PmenuThumb                        { fg = foreground, bg = background_popup }, -- Popup menu: Thumb of the scrollbar.
    PopupNotification                 { fg = foreground, bg = background_popup }, -- Popup menu for notifications
    IndentBlanklineChar               { fg = foreground_ui.darken(20) }, -- Color to use for indent guides (indent-blankline.nvim)
    VirtualIndentBlanklineChar        { fg = foreground_ui.darken(30) }, -- Color to use for indent guides in virtual lines
    ColorColumn                       { bg = foreground_ui.darken(20) }, -- used for the columns set with 'colorcolumn'
    Whitespace                        { fg = foreground_ui }, -- "nbsp", "space", "tab" and "trail" in 'listchars'
    NonText                           { fg = foreground_ui }, -- '@' at the end of the window, characters from 'showbreak' and other characters that do not really exist in the text (e.g., ">" displayed when a double-wide character doesn't fit at the end of the line). See also |hl-EndOfBuffer|.
    Conceal                           { fg = foreground_virtual }, -- placeholder characters substituted for concealed text (see 'conceallevel')
    SpecialKey                        { fg = foreground_virtual.rotate(100), gui = "bold" }, -- Unprintable characters: text displayed differently from what it really is.  But not 'listchars' whitespace. |hl-Whitespace|
    Cursor                            { gui = "standout" }, -- character under the cursor
    lCursor                           { gui = "standout" }, -- the character under the cursor when |language-mapping| is used (see 'guicursor')
    CursorIM                          { gui = "standout" }, -- like Cursor, but used when in IME mode |CursorIM|
    TermCursor                        { gui = "standout" }, -- cursor in a focused terminal
    TermCursorNC                      { gui = "standout" }, -- cursor in an unfocused terminal
    MatchParen                        { fg = hsl(0, 0, 100), bg = background.lighten(30), gui = "bold" }, -- The character under the cursor or just before it, if it is a paired bracket, and its match. |pi_paren.txt|
    CursorColumn                      { bg = cursor_highlight }, -- Screen-column at the cursor, when 'cursorcolumn' is set.
    CursorLine                        { bg = cursor_highlight }, -- Screen-line at the cursor, when 'cursorline' is set.  Low-priority if foreground (ctermfg OR guifg) is not set.
    Visual                            { bg = background.lighten(10) }, -- Visual mode selection
    VisualNOS                         { bg = background.lighten(10) }, -- Visual mode selection when vim is "Not Owning the Selection".
    Directory                         { fg = foreground }, -- directory names (and other special names in listings)
    DiffAdd                           { fg = diff_add }, -- diff mode: Added line |diff.txt|
    diffAdded                         { fg = diff_add },    -- Color to use for added text in Fugitive inline diffs
    GitSignsAdd                       { fg = diff_add, bg = background_gutter },
    DiffAddStatus                     { fg = diff_add },
    DiffChange                        { fg = diff_change }, -- diff mode: Changed line |diff.txt|
    GitSignsChange                    { fg = diff_change, bg = background_gutter },
    DiffChangeStatus                  { fg = diff_change },
    DiffDelete                        { fg = diff_delete }, -- diff mode: Deleted line |diff.txt|
    diffRemoved                       { fg = diff_delete }, -- Color to use for removed text in Fugitive inline diffs
    GitSignsDelete                    { fg = diff_delete, bg = background_gutter },
    DiffDeleteStatus                  { fg = diff_delete },
    DiffText                          { fg = diff_change }, -- diff mode: Changed text within a changed line |diff.txt|
    ConflictMarkerBegin               { bg = "#2f7366" },
    ConflictMarkerOurs                { bg = "#2e5049" },
    ConflictMarkerTheirs              { bg = "#344f69" },
    ConflictMarkerEnd                 { bg = "#2f628e" },
    ConflictMarkerCommonAncestorsHunk { bg = "#754a81" },
    EndOfBuffer                       { bg = background.darken(5) }, -- filler lines (~) after the end of the buffer.  By default, this is highlighted like |hl-NonText|.
    VertSplit                         { fg = foreground_ui }, -- the column separating vertically split windows
    Folded                            { fg = foreground_ui, bg = background.lighten(15) }, -- line used for closed folds
    FoldColumn                        { fg = foreground_ui, bg = background_gutter }, -- 'foldcolumn'
    SignColumn                        { fg = foreground_ui, bg = background_gutter }, -- column where |signs| are displayed
    LineNr                            { fg = foreground_ui, bg = background_gutter }, -- Line number for ":number" and ":#" commands, and when 'number' or 'relativenumber' option is set.
    CursorLineNr                      { fg = foreground_ui_highlight, bg = background_gutter }, -- Like LineNr when 'cursorline' or 'relativenumber' is set for the cursor line.
    SignatureMarkText                 { fg = "#f2b409", bg = background_gutter, gui = "bold" }, -- Color to use for marker signs (signature.vim)
    MsgArea                           { fg = foreground_ui, bg = background }, -- Area for messages and cmdline
    ErrorMsg                          { fg = error, bg = background, gui = "bold" }, -- error messages on the command line
    WarningMsg                        { fg = warning, bg = background, gui = "bold" }, -- warning messages
    ModeMsg                           { fg = foreground_ui, bg = background }, -- 'showmode' message (e.g., "-- INSERT -- ")
    MsgSeparator                      { fg = foreground_ui, bg = background }, -- Separator for scrolled messages, `msgsep` flag of 'display'
    MoreMsg                           { fg = foreground_ui, bg = background }, -- |more-prompt|
    Question                          { fg = foreground, bg = background }, -- |hit-enter| prompt and yes/no questions
    Search                            { fg = hsl(0, 0, 0), bg = background_search, gui = "bold" }, -- Last search pattern highlighting (see 'hlsearch').  Also used for similar items that need to stand out.
    IncSearch                         { fg = hsl(0, 0, 0), bg = background_search, gui = "bold" }, -- 'incsearch' highlighting; also used for the text replaced with ":s///c"
    Substitute                        { fg = hsl(0, 0, 0), bg = background_search, gui = "bold" }, -- |:substitute| replacement text highlighting
    Specs                             { bg = background.lighten(60) }, -- Color to use in specs.nvim
    TelescopeNormal                   { bg = background },
    TelescopeSelection                { bg = background.lighten(20), gui = 'bold' },
    TelescopeMatching                 { bg = background_search },
    TelescopePromptPrefix             { fg = foreground_ui, gui = 'bold' },
    TelescopeSelectionCaret           { fg = foreground_ui, bg = TelescopeSelection.bg, gui = 'bold' },
    TelescopeMultiSelection           { bg = background.lighten(15), gui = 'bold' },
    -- TelescopeBorder
    -- TelescopePromptBorder
    -- TelescopeResultsBorder
    -- TelescopePreviewBorder

    -- TODO better colors
    StatusLine           { fg = foreground_ui, bg = background_gutter }, -- status line of current window
    StatusLineNC         { fg = foreground_ui, bg = background_gutter.darken(10) }, -- status lines of not-current windows Note: if this is equal to "StatusLine" Vim will use "^^^" in the status line of the current window.
    TabLine              { fg = foreground_ui, bg = background_gutter }, -- tab pages line, not active tab page label
    TabLineFill          { fg = foreground_ui, bg = background_gutter }, -- tab pages line, where there are no labels
    TabLineSel           { fg = foreground_ui_highlight, bg = background_gutter.lighten(10), gui = 'bold' }, -- tab pages line, active tab page label

    SpellBad             { gui = "undercurl" }, -- Word that is not recognized by the spellchecker. |spell| Combined with the highlighting used otherwise. 
    SpellCap             { gui = "undercurl" }, -- Word that should start with a capital. |spell| Combined with the highlighting used otherwise.
    SpellLocal           { }, -- Word that is recognized by the spellchecker as one that is used in another region. |spell| Combined with the highlighting used otherwise.
    SpellRare            { }, -- Word that is recognized by the spellchecker as one that is hardly ever used.  |spell| Combined with the highlighting used otherwise.

    Underlined           { gui = "underline" },
    TSUnderline          { gui = "underline" };    -- For text to be represented with an underline.
    Bold                 { gui = "bold" },
    Italic               { gui = "italic" },
    TSEmphasis           { gui = "italic" };    -- For text to be represented with emphasis.
    TSStrike             { gui = "strikethrough" };    -- For strikethrough text.
    TSText               { };    -- For strings considered text in a markup language.

    Comment              { fg = syntax_comment, gui = "italic" }, -- any comment
    TSComment            { fg = syntax_comment, gui = "italic" };    -- For comment blocks.
    SpecialComment       { fg = syntax_comment }, -- special things inside a comment
    Title                { fg = foreground, gui = "bold,underline" }, -- titles for output from ":set all", ":autocmd" etc.
    TSTitle              { fg = foreground, gui = "bold,underline" };    -- Text that is part of a title.
    String               { fg = syntax_string }, --   a string constant: "this is a string"
    TSString             { fg = syntax_string };    -- For strings.
    TSStringRegex        { fg = syntax_string };    -- For regexes.
    TSLiteral            { fg = syntax_string };    -- Literal text.
    Character            { fg = syntax_string }, --  a character constant: 'c', '\n'
    TSCharacter          { fg = syntax_string };    -- For characters.
    TSStringEscape       { fg = syntax_string.lighten(40).saturate(50), gui = 'bold' };    -- For escape characters within a string.
    Number               { fg = syntax_number }, --   a number constant: 234, 0xff
    Float                { fg = syntax_number }, --    a floating point constant: 2.3e10
    TSNumber             { fg = syntax_number };    -- For all numbers
    TSFloat              { fg = syntax_number };    -- For floats.
    Operator             { fg = syntax_operator }, -- "sizeof", "+", "*", etc.
    TSOperator           { fg = syntax_operator };    -- For any operator: `+`, but also `->` and `*` in C.
    Statement            { fg = syntax_statement }, -- (preferred) any statement
    Conditional          { fg = syntax_statement }, --  if, then, else, endif, switch, etc.
    TSConditional        { fg = syntax_statement };    -- For keywords related to conditionnals.
    Repeat               { fg = syntax_statement }, --   for, do, while, etc.
    TSRepeat             { fg = syntax_statement };    -- For keywords related to loops.
    Exception            { fg = syntax_statement }, --  try, catch, throw
    TSException          { fg = syntax_statement };    -- For exception related keywords.
    Keyword              { fg = syntax_statement }, --  any other keyword
    TSKeyword            { fg = syntax_statement };    -- For keywords that don't fall in previous categories.
    TSKeywordFunction    { fg = syntax_statement };    -- For keywords used to define a fuction.
    Include              { fg = syntax_statement }, --  preprocessor #include
    TSInclude            { fg = syntax_statement };    -- For includes: `#include` in C, `use` or `extern crate` in Rust, or `require` in Lua.
    PreCondit            { fg = syntax_statement }, --  preprocessor #if, #else, #endif, etc.
    Define               { fg = syntax_statement }, --   preprocessor #define
    Macro                { fg = syntax_statement }, --    same as Define
    PreProc              { fg = syntax_statement }, -- (preferred) generic Preprocessor
    StorageClass         { fg = syntax_statement }, -- static, register, volatile, etc.
    Structure            { fg = syntax_statement }, --  struct, union, enum, etc.
    Typedef              { fg = syntax_statement }, --  A typedef
    TSTag                { fg = syntax_statement };    -- Tags like html tag names.
    TSTagDelimiter       { };    -- Tag delimiter like `<` `>` `/`
    Label                { fg = syntax_statement }, --    case, default, etc.
    TSLabel              { fg = syntax_label };    -- For labels: `label:` in C and `:label:` in Lua.
    TSAnnotation         { fg = syntax_attribute };    -- For C++/Dart attributes, annotations that can be attached to the code to denote some kind of meta information.
    TSAttribute          { fg = syntax_attribute };    -- (unstable) TODO: docs
    Constant             { }, -- (preferred) any constant
    TSConstMacro         { };    -- For constants that are defined by macros: `NULL` in C.
    TSConstant           { };    -- For constants
    TSVariableBuiltin    { fg = syntax_special };    -- Variable names that are defined by the languages, like `this` or `self`.
    TSConstBuiltin       { fg = syntax_special };    -- For constant that are built in the language: `nil` in Lua. FIXME other color?
    Boolean              { }, --  a boolean constant: TRUE, false
    TSBoolean            { };    -- For booleans.
    Identifier           { }, -- (preferred) any variable name
    TSNamespace          { };    -- For identifiers referring to modules and namespaces.
    TSVariable           { };    -- Any variable name that does not have another highlight.
    TSField              { };    -- For fields.
    TSProperty           { };    -- Same as `TSField`.
    TSParameter          { };    -- For parameters of a function.
    TSParameterReference { };    -- For references to parameters of a function.
    TSSymbol             { };    -- For identifiers referring to symbols or atoms.
    Function             { }, -- function name (also: methods for classes)
    TSFunction           { };    -- For function (calls and definitions).
    TSFuncMacro          { };    -- For macro defined fuctions (calls and definitions): each `macro_rules` in Rust.
    TSFuncBuiltin        { };    -- For builtin functions: `table.insert` in Lua.
    TSMethod             { };    -- For method calls and definitions.
    Type                 { }, -- (preferred) int, long, char, etc.
    TSType               { };    -- For types.
    TSTypeBuiltin        { };    -- For builtin types.
    Special              { }, -- (preferred) any special symbol
    SpecialChar          { }, --  special character in a constant
    Delimiter            { }, --  character that needs attention
    TSPunctDelimiter     { };    -- For delimiters ie: `.`
    TSPunctBracket       { };    -- For brackets and parens.
    TSPunctSpecial       { };    -- For special punctutation that does not fall in the catagories before.
    TSConstructor        { };    -- For constructor calls and definitions: ` { }` in Lua, and Java constructors.
    Debug                { }, --    debugging statements
    Error                { }, -- (preferred) any erroneous construct
    TSError              { };    -- For syntax/parser errors.
    TSURI                { fg = hsl(190, 80, 65), gui = 'underline' };    -- Any URI like a link or email.
    Todo                 { fg = hsl(0, 0, 0), bg = hsl(60, 100, 50), gui = "bold" }, -- (preferred) anything that needs extra attention; mostly the keywords TODO FIXME and XXX
    -- TSNone               { };    -- TODO: docs

    Tag               { bg = background.lighten(10) }, --    you can use CTRL-] on this

    -- TODO update:
    QuickFixLine      { gui = "bold" }, -- Current |quickfix| item in the quickfix window. Combined with |hl-CursorLine| when the cursor is there.
    WildMenu          { fg = "#000000", bg = "#FFFF00", gui = "bold" }, -- current match in 'wildmenu' completion

    -- These groups are not listed as default vim groups,
    -- but they are defacto standard group names for syntax highlighting.
    -- commented out groups should chain up to their "preferred" group by
    -- default,
    -- Uncomment and edit if you want more specific syntax highlighting.

    -- ("Ignore", below, may be invisible...)

    -- These groups are for the native LSP client. Some other LSP clients may
    -- use these groups, or use their own. Consult your LSP client's
    -- documentation.

    -- LspReferenceText                     { }, -- used for highlighting "text" references
    -- LspReferenceRead                     { }, -- used for highlighting "read" references
    -- LspReferenceWrite                    { }, -- used for highlighting "write" references

    -- LspDiagnosticsDefaultError           { }, -- Used as the base highlight group. Other LspDiagnostic highlights link to this by default (except Underline)
    -- LspDiagnosticsDefaultWarning         { }, -- Used as the base highlight group. Other LspDiagnostic highlights link to this by default (except Underline)
    -- LspDiagnosticsDefaultInformation     { }, -- Used as the base highlight group. Other LspDiagnostic highlights link to this by default (except Underline)
    -- LspDiagnosticsDefaultHint            { }, -- Used as the base highlight group. Other LspDiagnostic highlights link to this by default (except Underline)

    DiagnosticVirtualTextError { fg = "#ff795B", bg = "#822234", gui = "bold" }, -- Used for "Error" diagnostic virtual text
    DiagnosticVirtualTextWarn  { fg = "#efc809", bg = "#625234", gui = "bold" }, -- Used for "Warning" diagnostic virtual text
    DiagnosticVirtualTextInfo  { fg = "#cccccc", bg = "#444454", gui = "bold" }, -- Used for "Information" diagnostic virtual text
    DiagnosticVirtualTextHint  { fg = "#cccccc", bg = "#444454", gui = "bold" }, -- Used for "Hint" diagnostic virtual text

    DiagnosticUnderlineError { bg = "#b23224" }, -- Used for "Error" diagnostic virtual text
    DiagnosticUnderlineWarn  { bg = "#a28244" }, -- Used for "Warning" diagnostic virtual text
    DiagnosticUnderlineInfo  { bg = "#666666" }, -- Used for "Information" diagnostic virtual text
    DiagnosticUnderlineHint  { bg = "#666666" }, -- Used for "Hint" diagnostic virtual text

    -- LspDiagnosticsSignError              { }, -- Used for "Error" signs in sign column
    -- LspDiagnosticsSignWarning            { }, -- Used for "Warning" signs in sign column
    -- LspDiagnosticsSignInformation        { }, -- Used for "Information" signs in sign column
    -- LspDiagnosticsSignHint               { }, -- Used for "Hint" signs in sign column

    SymbolsOutlineConnector { Normal }, -- connector highlight for symbols-outline.nvim
    FocusedSymbol { Normal, gui = "bold" }, -- focused symbol highlight for symbols-outline.nvim
    SymbolIcon { Comment, gui = "NONE" }, -- icon highlight for symbols-outline.nvim

  }
end)

-- return our parsed theme for extension or use else where.
return theme

-- vi:nowrap
