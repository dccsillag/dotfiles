-- Enable lush.ify on this file, run:
--
--  `:Lushify`
--
--  or
--
--  `:lua require('lush').ify()`

local lush = require('lush')
local hsl = lush.hsl

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

    Normal            { fg = "#FFFFFF", bg = "#282C33" }, -- normal text
    Comment           { fg = "#A5A6A9", gui = "italic" }, -- any comment
    ColorColumn       { bg = "#494d54" }, -- used for the columns set with 'colorcolumn'
    Conceal           { }, -- placeholder characters substituted for concealed text (see 'conceallevel')
    Cursor            { gui = "standout" }, -- character under the cursor
    lCursor           { Cursor }, -- the character under the cursor when |language-mapping| is used (see 'guicursor')
    CursorIM          { Cursor }, -- like Cursor, but used when in IME mode |CursorIM|
    CursorColumn      { bg = "#353940" }, -- Screen-column at the cursor, when 'cursorcolumn' is set.
    CursorLine        { bg = "#353940" }, -- Screen-line at the cursor, when 'cursorline' is set.  Low-priority if foreground (ctermfg OR guifg) is not set.
    Directory         { fg = "#FFFFFF" }, -- directory names (and other special names in listings)
    DiffAdd           { fg = "#00FF00" }, -- diff mode: Added line |diff.txt|
    DiffChange        { fg = "#FFA500" }, -- diff mode: Changed line |diff.txt|
    DiffDelete        { fg = "#FF7020" }, -- diff mode: Deleted line |diff.txt|
    DiffText          { DiffChange }, -- diff mode: Changed text within a changed line |diff.txt|
    EndOfBuffer       { bg = "#1e2229" }, -- filler lines (~) after the end of the buffer.  By default, this is highlighted like |hl-NonText|.
    TermCursor        { Cursor }, -- cursor in a focused terminal
    TermCursorNC      { Cursor }, -- cursor in an unfocused terminal
    ErrorMsg          { fg = "#FF0000", gui = "bold" }, -- error messages on the command line
    VertSplit         { fg = "#B0B0B0" }, -- the column separating vertically split windows
    Folded            { fg = "#AAAAAA", gui = "bold" }, -- line used for closed folds
    FoldColumn        { fg = "#FFFFFF", bg = "#383C43", gui = "bold" }, -- 'foldcolumn'
    SignColumn        { fg = "#FFFFFF", bg = "#383C43" }, -- column where |signs| are displayed
    Substitute        { fg = "#EEEEEE", gui = "bold,reverse" }, -- |:substitute| replacement text highlighting
    LineNr            { fg = "#7C7D7F", bg = "#383C43" }, -- Line number for ":number" and ":#" commands, and when 'number' or 'relativenumber' option is set.
    CursorLineNr      { LineNr, fg = "#aeafc5" }, -- Like LineNr when 'cursorline' or 'relativenumber' is set for the cursor line.
    MatchParen        { bg = "#575859", gui = "bold" }, -- The character under the cursor or just before it, if it is a paired bracket, and its match. |pi_paren.txt|
    MsgArea           { bg = "#23272e" }, -- Area for messages and cmdline
    ModeMsg           { MsgArea }, -- 'showmode' message (e.g., "-- INSERT -- ")
    MsgSeparator      { MsgArea }, -- Separator for scrolled messages, `msgsep` flag of 'display'
    MoreMsg           { MsgArea }, -- |more-prompt|
    NonText           { fg = "#777777" }, -- '@' at the end of the window, characters from 'showbreak' and other characters that do not really exist in the text (e.g., ">" displayed when a double-wide character doesn't fit at the end of the line). See also |hl-EndOfBuffer|.
    NormalFloat       { fg = "#FFFFFF", bg = "#30343b" }, -- Normal text in floating windows.
    NormalNC          { Normal }, -- normal text in non-current windows
    Pmenu             { NormalFloat }, -- Popup menu: normal item.
    PmenuSel          { Pmenu, gui = "standout,bold" }, -- Popup menu: selected item.
    PmenuSbar         { Pmenu }, -- Popup menu: scrollbar.
    PmenuThumb        { PmenuSel }, -- Popup menu: Thumb of the scrollbar.
    PopupNotification { Pmenu }, -- Popup menu for notifications
    Question          { gui = "bold" }, -- |hit-enter| prompt and yes/no questions
    QuickFixLine      { gui = "bold" }, -- Current |quickfix| item in the quickfix window. Combined with |hl-CursorLine| when the cursor is there.
    Search            { fg = "#CCCCCC", gui = "bold,reverse" }, -- Last search pattern highlighting (see 'hlsearch').  Also used for similar items that need to stand out.
    IncSearch         { Search }, -- 'incsearch' highlighting; also used for the text replaced with ":s///c"
    SpecialKey        { fg = "#00afea", gui = "bold" }, -- Unprintable characters: text displayed differently from what it really is.  But not 'listchars' whitespace. |hl-Whitespace|
    SpellBad          { gui = "undercurl" }, -- Word that is not recognized by the spellchecker. |spell| Combined with the highlighting used otherwise. 
    SpellCap          { fg = "#BEBEBE", gui = "undercurl" }, -- Word that should start with a capital. |spell| Combined with the highlighting used otherwise.
    SpellLocal        { fg = "#BEBEBE" }, -- Word that is recognized by the spellchecker as one that is used in another region. |spell| Combined with the highlighting used otherwise.
    SpellRare         { fg = "#BEBEBE" }, -- Word that is recognized by the spellchecker as one that is hardly ever used.  |spell| Combined with the highlighting used otherwise.
    StatusLine        { fg = "#B0B0B0" }, -- status line of current window
    StatusLineNC      { fg = "#909090" }, -- status lines of not-current windows Note: if this is equal to "StatusLine" Vim will use "^^^" in the status line of the current window.
    TabLine           { fg = "#000000", bg = "#777777" }, -- tab pages line, not active tab page label
    TabLineFill       { fg = "#000000", bg = "#AAAAAA" }, -- tab pages line, where there are no labels
    TabLineSel        { fg = "#000000", bg = "#555555" }, -- tab pages line, active tab page label
    Title             { fg = "#FFFFFF", gui = "bold,underline" }, -- titles for output from ":set all", ":autocmd" etc.
    Visual            { bg = "#444444" }, -- Visual mode selection
    VisualNOS         { Visual }, -- Visual mode selection when vim is "Not Owning the Selection".
    WarningMsg        { fg = "#FFFF00", bg = MsgArea.bg, gui = "underline" }, -- warning messages
    Whitespace        { fg = "#888888" }, -- "nbsp", "space", "tab" and "trail" in 'listchars'
    WildMenu          { fg = "#000000", bg = "#FFFF00", gui = "bold" }, -- current match in 'wildmenu' completion

    -- These groups are not listed as default vim groups,
    -- but they are defacto standard group names for syntax highlighting.
    -- commented out groups should chain up to their "preferred" group by
    -- default,
    -- Uncomment and edit if you want more specific syntax highlighting.

    Constant       { Normal }, -- (preferred) any constant
    String         { Normal }, --   a string constant: "this is a string"
    Character      { Normal }, --  a character constant: 'c', '\n'
    Number         { Normal }, --   a number constant: 234, 0xff
    Boolean        { Normal }, --  a boolean constant: TRUE, false
    Float          { Normal }, --    a floating point constant: 2.3e10

    Identifier     { Normal }, -- (preferred) any variable name
    Function       { Normal }, -- function name (also: methods for classes)

    Statement      { Normal }, -- (preferred) any statement
    Conditional    { Normal }, --  if, then, else, endif, switch, etc.
    Repeat         { Normal }, --   for, do, while, etc.
    Label          { Normal }, --    case, default, etc.
    Operator       { Normal }, -- "sizeof", "+", "*", etc.
    Keyword        { Normal }, --  any other keyword
    Exception      { Normal }, --  try, catch, throw

    PreProc        { Normal }, -- (preferred) generic Preprocessor
    Include        { Normal }, --  preprocessor #include
    Define         { Normal }, --   preprocessor #define
    Macro          { Normal }, --    same as Define
    PreCondit      { Normal }, --  preprocessor #if, #else, #endif, etc.

    Type           { Normal }, -- (preferred) int, long, char, etc.
    StorageClass   { Normal }, -- static, register, volatile, etc.
    Structure      { Normal }, --  struct, union, enum, etc.
    Typedef        { Normal }, --  A typedef

    Special        { Normal }, -- (preferred) any special symbol
    SpecialChar    { Normal }, --  special character in a constant
    Tag            { Normal }, --    you can use CTRL-] on this
    Delimiter      { Normal }, --  character that needs attention
    SpecialComment { Normal, fg = "#A5A6A9" }, -- special things inside a comment
    Debug          { Normal }, --    debugging statements

    Underlined { Normal, gui = "underline" }, -- (preferred) text that stands out, HTML links
    Bold       { Normal, gui = "bold" },
    Italic     { Normal, gui = "italic" },

    -- ("Ignore", below, may be invisible...)
    Ignore         { }, -- (preferred) left blank, hidden  |hl-Ignore|

    Error          { gui = "underline" }, -- (preferred) any erroneous construct

    Todo           { fg = "#000000", bg = "#FFFF00", gui = "bold" }, -- (preferred) anything that needs extra attention; mostly the keywords TODO FIXME and XXX

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

    -- These groups are for the neovim tree-sitter highlights.
    -- As of writing, tree-sitter support is a WIP, group names may change.
    -- By default, most of these groups link to an appropriate Vim group,
    -- TSError -> Error for example, so you do not have to define these unless
    -- you explicitly want to support Treesitter's improved syntax awareness.

    -- TSAnnotation         { };    -- For C++/Dart attributes, annotations that can be attached to the code to denote some kind of meta information.
    -- TSAttribute          { };    -- (unstable) TODO: docs
    -- TSBoolean            { };    -- For booleans.
    -- TSCharacter          { };    -- For characters.
    -- TSComment            { };    -- For comment blocks.
    -- TSConstructor        { };    -- For constructor calls and definitions: ` { }` in Lua, and Java constructors.
    -- TSConditional        { };    -- For keywords related to conditionnals.
    -- TSConstant           { };    -- For constants
    -- TSConstBuiltin       { };    -- For constant that are built in the language: `nil` in Lua.
    -- TSConstMacro         { };    -- For constants that are defined by macros: `NULL` in C.
    -- TSError              { };    -- For syntax/parser errors.
    -- TSException          { };    -- For exception related keywords.
    -- TSField              { };    -- For fields.
    -- TSFloat              { };    -- For floats.
    -- TSFunction           { };    -- For function (calls and definitions).
    -- TSFuncBuiltin        { };    -- For builtin functions: `table.insert` in Lua.
    -- TSFuncMacro          { };    -- For macro defined fuctions (calls and definitions): each `macro_rules` in Rust.
    -- TSInclude            { };    -- For includes: `#include` in C, `use` or `extern crate` in Rust, or `require` in Lua.
    -- TSKeyword            { };    -- For keywords that don't fall in previous categories.
    -- TSKeywordFunction    { };    -- For keywords used to define a fuction.
    -- TSLabel              { };    -- For labels: `label:` in C and `:label:` in Lua.
    -- TSMethod             { };    -- For method calls and definitions.
    -- TSNamespace          { };    -- For identifiers referring to modules and namespaces.
    -- TSNone               { };    -- TODO: docs
    -- TSNumber             { };    -- For all numbers
    -- TSOperator           { };    -- For any operator: `+`, but also `->` and `*` in C.
    -- TSParameter          { };    -- For parameters of a function.
    -- TSParameterReference { };    -- For references to parameters of a function.
    -- TSProperty           { };    -- Same as `TSField`.
    -- TSPunctDelimiter     { };    -- For delimiters ie: `.`
    -- TSPunctBracket       { };    -- For brackets and parens.
    -- TSPunctSpecial       { };    -- For special punctutation that does not fall in the catagories before.
    -- TSRepeat             { };    -- For keywords related to loops.
    -- TSString             { };    -- For strings.
    -- TSStringRegex        { };    -- For regexes.
    -- TSStringEscape       { };    -- For escape characters within a string.
    -- TSSymbol             { };    -- For identifiers referring to symbols or atoms.
    -- TSType               { };    -- For types.
    -- TSTypeBuiltin        { };    -- For builtin types.
    -- TSVariable           { };    -- Any variable name that does not have another highlight.
    -- TSVariableBuiltin    { };    -- Variable names that are defined by the languages, like `this` or `self`.

    -- TSTag                { };    -- Tags like html tag names.
    -- TSTagDelimiter       { };    -- Tag delimiter like `<` `>` `/`
    -- TSText               { };    -- For strings considered text in a markup language.
    -- TSEmphasis           { };    -- For text to be represented with emphasis.
    -- TSUnderline          { };    -- For text to be represented with an underline.
    -- TSStrike             { };    -- For strikethrough text.
    -- TSTitle              { };    -- Text that is part of a title.
    -- TSLiteral            { };    -- Literal text.
    -- TSURI                { };    -- Any URI like a link or email.

    -- Below here are highlights that are used by plugins.

    Specs { bg = "#FFFFFF" }, -- Color to use in specs.nvim

    diffAdded   { DiffAdd },    -- Color to use for added text in Fugitive inline diffs
    diffRemoved { DiffDelete }, -- Color to use for removed text in Fugitive inline diffs

    GitSignsAdd { fg = DiffAdd.fg, bg = SignColumn.bg },
    GitSignsChange { fg = DiffChange.fg, bg = SignColumn.bg },
    GitSignsDelete { fg = DiffDelete.fg, bg = SignColumn.bg },

    SignatureMarkText { fg = "#f2b409", gui = "bold" }, -- Color to use for marker signs (signature.vim)

    IndentBlanklineChar { fg = "#666666" }, -- Color to use for indent guides (indent-blankline.nvim)
    VirtualIndentBlanklineChar { fg = "#444444" }, -- Color to use for indent guides in virtual lines

    -- HopNextKey   { fg = "#FFFFFF", bg = "#4444EE", gui = "bold" }, -- color for hop sequence of length 1
    HopNextKey   { fg = "#FFFFFF", bg = "#44AA44", gui = "bold" }, -- color for hop sequence of length 1
    HopNextKey1  { HopNextKey }, -- color for hop head of sequence
    HopNextKey2  { HopNextKey, fg = "#000000" }, -- color for hop tail of sequence
    HopUnmatched { Normal, fg = "#666666" }, -- color for hop unmatched chars

    SymbolsOutlineConnector { Normal }, -- connector highlight for symbols-outline.nvim
    FocusedSymbol { Normal, gui = "bold" }, -- focused symbol highlight for symbols-outline.nvim
    SymbolIcon { Comment, gui = "NONE" }, -- icon highlight for symbols-outline.nvim

    --

    ConflictMarkerBegin { bg = "#2f7366" },
    ConflictMarkerOurs { bg = "#2e5049" },
    ConflictMarkerTheirs { bg = "#344f69" },
    ConflictMarkerEnd { bg = "#2f628e" },
    ConflictMarkerCommonAncestorsHunk { bg = "#754a81" },

    --

    DiffAddStatus     { DiffAdd, bg = "NONE" },
    DiffChangeStatus  { DiffChange, bg = "NONE" },
    DiffDeleteStatus  { DiffDelete, bg = "NONE" },

  }
end)

-- return our parsed theme for extension or use else where.
return theme

-- vi:nowrap
