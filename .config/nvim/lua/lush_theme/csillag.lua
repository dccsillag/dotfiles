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
local background_gutter = background -- hsl(200, 10, 20)
local background_tabs = background.lighten(7)
local foreground = hsl(0, 0, 100)
local foreground_ui = hsl(0, 0, 27)
local foreground_ui2 = hsl(0, 0, 50)
local foreground_ui_highlight = hsl(200, 40, 85)
local foreground_virtual = hsl(200, 80, 80)
local cursor_highlight = background.lighten(10)
local background_search = hsl(50, 100, 50)

local diff_add = hsl(120, 100, 50)
local diff_change = hsl(39, 100, 50)
local diff_delete = hsl(22, 100, 56)

local error = hsl(0, 100, 50)
local warning = hsl(60, 100, 50)
local success = hsl(120, 100, 50)
local skipped = foreground.darken(50)

local mark = "#f2b409"
local special = hsl(280, 100, 81)

local syntax_comment = foreground.darken(25)
local syntax_string = hsl(120, 40, 82)
local syntax_number = hsl(190, 60, 70)
local syntax_statement = foreground -- hsl(10, 80, 75)
-- local syntax_identifier = hsl(300, 100, 92)
local syntax_identifier = foreground -- hsl(50, 100, 90) -- hsl(100, 90, 96) -- hsl(300, 0, 88)  -- hsl(250, 80, 85)
local syntax_local_identifier = foreground -- hsl(50, 100, 90)  -- hsl(230, 60, 93)
local syntax_operator = foreground
local syntax_attribute = foreground.darken(10)
local syntax_special = hsl(190, 60, 70)

local theme = lush(function(injected_functions)
        local sym = injected_functions.sym

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

                Ignore {},                                                                     -- (preferred) left blank, hidden  |hl-Ignore|
                Normal { fg = foreground, bg = background },                                   -- normal text
                NormalNC { fg = foreground, bg = background },                                 -- normal text in non-current windows
                NormalFloat { fg = foreground, bg = background },                              -- Normal text in floating windows.
                Pmenu { fg = foreground, bg = background_popup },                              -- Popup menu: normal item.
                PmenuSel { fg = foreground, bg = background_popup, gui = "standout,bold" },    -- Popup menu: selected item.
                PmenuSbar { fg = foreground, bg = background_popup },                          -- Popup menu: scrollbar.
                PmenuThumb { fg = foreground, bg = background_popup },                         -- Popup menu: Thumb of the scrollbar.
                PopupNotification { fg = foreground, bg = background_popup },                  -- Popup menu for notifications
                IndentBlanklineChar { fg = foreground_ui.darken(20) },                         -- Color to use for indent guides (indent-blankline.nvim)
                VirtualIndentBlanklineChar { fg = foreground_ui.darken(30) },                  -- Color to use for indent guides in virtual lines
                IblIndent { fg = foreground_ui2.darken(30) },                                  -- Color to use for indent guides (indent-blankline.nvim)
                IblScope { fg = foreground_ui2.darken(30) },                                   -- Color to use for indent guides (indent-blankline.nvim)
                ColorColumn {},                                                                -- used for the columns set with 'colorcolumn'
                Whitespace { fg = foreground_ui2 },                                             -- "nbsp", "space", "tab" and "trail" in 'listchars'
                NonText { fg = foreground_ui2 },                                                -- '@' at the end of the window, characters from 'showbreak' and other characters that do not really exist in the text (e.g., ">" displayed when a double-wide character doesn't fit at the end of the line). See also |hl-EndOfBuffer|.
                Conceal { fg = foreground_virtual },                                           -- placeholder characters substituted for concealed text (see 'conceallevel')
                SpecialKey { fg = foreground_virtual.rotate(100), gui = "bold" },              -- Unprintable characters: text displayed differently from what it really is.  But not 'listchars' whitespace. |hl-Whitespace|
                Cursor { gui = "standout" },                                                   -- character under the cursor
                lCursor { gui = "standout" },                                                  -- the character under the cursor when |language-mapping| is used (see 'guicursor')
                CursorIM { gui = "standout" },                                                 -- like Cursor, but used when in IME mode |CursorIM|
                TermCursor { gui = "standout" },                                               -- cursor in a focused terminal
                TermCursorNC { gui = "standout" },                                             -- cursor in an unfocused terminal
                MatchParen { fg = hsl(0, 0, 100), bg = background.lighten(30), gui = "bold" }, -- The character under the cursor or just before it, if it is a paired bracket, and its match. |pi_paren.txt|
                CursorColumn { bg = cursor_highlight },                                        -- Screen-column at the cursor, when 'cursorcolumn' is set.
                CursorLine { bg = cursor_highlight },                                          -- Screen-line at the cursor, when 'cursorline' is set.  Low-priority if foreground (ctermfg OR guifg) is not set.
                Visual { bg = background.lighten(20) },                                        -- Visual mode selection
                VisualNOS { bg = background.lighten(20) },                                     -- Visual mode selection when vim is "Not Owning the Selection".
                Directory { fg = foreground },                                                 -- directory names (and other special names in listings)
                DiffAdd { fg = diff_add },                                                     -- diff mode: Added line |diff.txt|
                diffAdded { fg = diff_add },                                                   -- Color to use for added text in Fugitive inline diffs
                GitSignsAdd { fg = diff_add, bg = background_gutter },
                DiffAddStatus { fg = diff_add },
                GitSignsAddInline { bg = background.hue(diff_add.h).saturate(80) },
                DiffChange { fg = diff_change }, -- diff mode: Changed line |diff.txt|
                GitSignsChange { fg = diff_change, bg = background_gutter },
                GitSignsChangeInline { bg = background.hue(diff_change.h).lighten(10).saturate(100) },
                DiffChangeStatus { fg = diff_change },
                DiffDelete { fg = diff_delete },  -- diff mode: Deleted line |diff.txt|
                diffRemoved { fg = diff_delete }, -- Color to use for removed text in Fugitive inline diffs
                GitSignsDelete { fg = diff_delete, bg = background_gutter },
                GitSignsDeleteInline { bg = background.hue(diff_delete.h).lighten(10).saturate(100) },
                DiffDeleteStatus { fg = diff_delete },
                DiffText { fg = diff_change }, -- diff mode: Changed text within a changed line |diff.txt|
                ConflictMarkerBegin { bg = "#2f7366" },
                ConflictMarkerOurs { bg = "#2e5049" },
                ConflictMarkerTheirs { bg = "#344f69" },
                ConflictMarkerEnd { bg = "#2f628e" },
                ConflictMarkerCommonAncestorsHunk { bg = "#754a81" },
                EndOfBuffer {},                                                         -- filler lines (~) after the end of the buffer.  By default, this is highlighted like |hl-NonText|.
                VertSplit { fg = foreground_ui },                                       -- the column separating vertically split windows
                WinSeparator { fg = foreground_ui },                                       -- the column separating vertically split windows
                Folded {},                                                              -- line used for closed folds
                FoldColumn { fg = foreground_ui2, bg = background_gutter },              -- 'foldcolumn'
                SignColumn { fg = foreground_ui2, bg = background_gutter },              -- column where |signs| are displayed
                LineNr { fg = foreground_ui2, bg = background_gutter },                  -- Line number for ":number" and ":#" commands, and when 'number' or 'relativenumber' option is set.
                CursorLineNr { fg = foreground_ui_highlight, bg = background_gutter },  -- Like LineNr when 'cursorline' or 'relativenumber' is set for the cursor line.
                SignatureMarkText { fg = mark, bg = background_gutter, gui = "bold" },  -- Color to use for marker signs (signature.vim)
                MsgArea { fg = foreground_ui2, bg = background },                        -- Area for messages and cmdline
                ErrorMsg { fg = error, bg = background, gui = "bold" },                 -- error messages on the command line
                WarningMsg { fg = warning, bg = background, gui = "bold" },             -- warning messages
                ModeMsg { fg = foreground_ui2, bg = background },                        -- 'showmode' message (e.g., "-- INSERT -- ")
                MsgSeparator { fg = foreground_ui2, bg = background },                   -- Separator for scrolled messages, `msgsep` flag of 'display'
                MoreMsg { fg = foreground_ui2, bg = background },                        -- |more-prompt|
                Question { fg = foreground, bg = background },                          -- |hit-enter| prompt and yes/no questions
                Search { fg = hsl(0, 0, 0), bg = background_search, gui = "bold" },     -- Last search pattern highlighting (see 'hlsearch').  Also used for similar items that need to stand out.
                IncSearch { fg = hsl(0, 0, 0), bg = background_search, gui = "bold" },  -- 'incsearch' highlighting; also used for the text replaced with ":s///c"
                Substitute { fg = hsl(0, 0, 0), bg = background_search, gui = "bold" }, -- |:substitute| replacement text highlighting
                Specs { bg = background.lighten(60) },                                  -- Color to use in specs.nvim
                TelescopeNormal { bg = background },
                TelescopeSelection { bg = background.lighten(20), gui = 'bold' },
                TelescopeMatching { bg = background_search },
                TelescopePromptPrefix { fg = foreground_ui2, gui = 'bold' },
                TelescopeSelectionCaret { fg = foreground_ui2, bg = TelescopeSelection.bg, gui = 'bold' },
                TelescopeMultiSelection { bg = background.lighten(15), gui = 'bold' },
                -- TelescopeBorder
                -- TelescopePromptBorder
                -- TelescopeResultsBorder
                -- TelescopePreviewBorder

                -- TODO better colors
                StatusLine { fg = foreground_ui, bg = background_gutter },                                   -- status line of current window
                StatusLineNC { fg = foreground_ui, bg = background_gutter.darken(10) },                      -- status lines of not-current windows Note: if this is equal to "StatusLine" Vim will use "^^^" in the status line of the current window.
                TabLine { fg = foreground_ui2, bg = background_tabs },                                        -- tab pages line, not active tab page label
                TabLineFill { fg = foreground_ui2, bg = background_tabs },                                    -- tab pages line, where there are no labels
                TabLineSel { fg = foreground_ui_highlight, bg = background_tabs.lighten(20), gui = 'bold' }, -- tab pages line, active tab page label

                SpellBad { gui = "undercurl" },                                                              -- Word that is not recognized by the spellchecker. |spell| Combined with the highlighting used otherwise.
                SpellCap { gui = "undercurl" },                                                              -- Word that should start with a capital. |spell| Combined with the highlighting used otherwise.
                SpellLocal {},                                                                               -- Word that is recognized by the spellchecker as one that is used in another region. |spell| Combined with the highlighting used otherwise.
                SpellRare {},                                                                                -- Word that is recognized by the spellchecker as one that is hardly ever used.  |spell| Combined with the highlighting used otherwise.

                Underlined { gui = "underline" },
                Bold { gui = "bold" },
                Italic { gui = "italic" },
                TSEmphasis { gui = "italic" },      -- For text to be represented with emphasis.
                TSStrike { gui = "strikethrough" }, -- For strikethrough text.
                TSText { fg = foreground },                          -- For strings considered text in a markup language.

                sym("@lsp.type.namespace") { fg = syntax_identifier },
                sym("@lsp.type.type") { fg = syntax_identifier },
                sym("@lsp.type.typeAlias") { fg = syntax_identifier },
                sym("@lsp.type.class") { fg = syntax_identifier },
                sym("@lsp.type.enum") { fg = syntax_identifier },
                sym("@lsp.type.interface") { fg = syntax_identifier },
                sym("@lsp.type.struct") { fg = syntax_identifier },
                sym("@lsp.type.typeParameter") { fg = syntax_local_identifier },
                sym("@lsp.type.parameter") { fg = syntax_local_identifier },
                sym("@lsp.type.variable") { fg = syntax_local_identifier },
                sym("@lsp.type.pol") { fg = syntax_local_identifier },
                sym("@lsp.type.property") { fg = syntax_identifier },
                sym("@lsp.type.enumMember") { fg = syntax_identifier },
                -- sym("@lsp.type.event") { fg = syntax_identifier },
                sym("@lsp.type.function") { fg = syntax_identifier },
                sym("@lsp.type.method") { fg = syntax_identifier },
                sym("@lsp.type.macro") { fg = syntax_identifier },
                sym("@lsp.type.keyword") { fg = syntax_statement },
                sym("@lsp.type.modifier") { fg = syntax_statement },
                sym("@lsp.type.controlFlow") { fg = syntax_statement },
                sym("@lsp.type.comment") { fg = syntax_comment },
                sym("@lsp.type.string") { fg = syntax_string },
                sym("@lsp.type.number") { fg = syntax_number },
                sym("@lsp.type.bool") { fg = syntax_number },
                sym("@lsp.type.regexp") { fg = syntax_string, gui = "bold" },
                sym("@lsp.type.operator") { fg = syntax_operator },
                sym("@lsp.type.decorator") { fg = syntax_statement },

                sym("@lsp.mod.declaration") { fg = syntax_identifier },
                sym("@lsp.mod.definition") { fg = syntax_identifier },
                sym("@lsp.mod.readonly") { fg = syntax_identifier },
                sym("@lsp.mod.static") { fg = syntax_identifier },
                sym("@lsp.mod.deprecated") { fg = syntax_identifier, gui = "strikethrough" },
                sym("@lsp.mod.abstract") { fg = syntax_identifier },
                sym("@lsp.mod.async") { fg = syntax_identifier },
                sym("@lsp.mod.modification") { fg = syntax_identifier },
                sym("@lsp.mod.documentation") { fg = syntax_comment.darken(2), gui = "bold" },
                sym("@lsp.mod.defaultLibrary") { fg = syntax_identifier },
                sym("@lsp.typemod.comment.documentation") { fg = syntax_comment, gui = "bold" },
                Comment { fg = syntax_comment },                                           -- any comment
                SpecialComment { fg = syntax_comment, gui = "bold" },                      -- special things inside a comment
                Title { fg = foreground, gui = "bold" },                                   -- titles for output from ":set all", ":autocmd" etc.
                String { fg = syntax_string },                                             --   a string constant: "this is a string"
                SpecialChar { fg = syntax_string.lighten(40).saturate(50), gui = 'bold' }, --  special character in a constant
                Character { fg = syntax_string },                                          --  a character constant: 'c', '\n'
                Number { fg = syntax_number },                                             --   a number constant: 234, 0xff
                Float { fg = syntax_number },                                              --    a floating point constant: 2.3e10
                Boolean { fg = syntax_number },                                            --  a boolean constant: TRUE, false
                Operator { fg = syntax_operator },                                         -- "sizeof", "+", "*", etc.
                Statement { fg = syntax_statement },                                       -- (preferred) any statement
                Conditional { fg = syntax_statement },                                     --  if, then, else, endif, switch, etc.
                Repeat { fg = syntax_statement },                                          --   for, do, while, etc.
                TSRepeat { fg = syntax_statement },                                        -- For keywords related to loops.
                Exception { fg = syntax_statement },                                       --  try, catch, throw
                Keyword { fg = syntax_statement },                                         --  any other keyword
                Include { fg = syntax_statement },                                         --  preprocessor #include
                PreCondit { fg = syntax_statement },                                       --  preprocessor #if, #else, #endif, etc.
                Define { fg = syntax_statement },                                          --   preprocessor #define
                Macro { fg = syntax_statement },                                           --    same as Define
                Variable { fg = syntax_identifier },                                       --    same as Define
                sym("@variable") { fg = syntax_identifier },                               --    same as Define
                PreProc { fg = syntax_statement },                                         -- (preferred) generic Preprocessor
                StorageClass { fg = syntax_statement },                                    -- static, register, volatile, etc.
                Structure { fg = syntax_statement },                                       --  struct, union, enum, etc.
                Typedef { fg = syntax_statement },                                         --  A typedef
                Tag { fg = syntax_statement },                                             -- Tags like html tag names.
                Label { fg = syntax_statement },                                           --    case, default, etc.
                TSAnnotation { fg = syntax_attribute },                                    -- For C++/Dart attributes, annotations that can be attached to the code to denote some kind of meta information.
                TSAttribute { fg = syntax_attribute },                                     -- (unstable) TODO: docs
                Constant { fg = foreground },                                                               -- (preferred) any constant
                TSVariableBuiltin { fg = syntax_special },                                 -- Variable names that are defined by the languages, like `this` or `self`.
                TSConstBuiltin { fg = syntax_special },                                    -- For constant that are built in the language: `nil` in Lua. FIXME other color?
                TSBoolean { fg = foreground },                                                              -- For booleans.
                Identifier { fg = foreground },                                                             -- (preferred) any variable name
                Function { fg = foreground },                                                               -- function name (also: methods for classes)
                Type { fg = foreground },                                                                   -- (preferred) int, long, char, etc.
                Special { fg = foreground },                                                                -- (preferred) any special symbol
                Delimiter { fg = foreground },                                                              --  character that needs attention
                TSPunctDelimiter { fg = foreground },                                                       -- For delimiters ie: `.`
                TSPunctBracket { fg = foreground },                                                         -- For brackets and parens.
                TSPunctSpecial { fg = foreground },                                                         -- For special punctutation that does not fall in the catagories before.
                TSConstructor { fg = foreground },                                                          -- For constructor calls and definitions: ` { }` in Lua, and Java constructors.
                Debug { fg = foreground },                                                                  --    debugging statements
                Error { fg = foreground },                                                                  -- (preferred) any erroneous construct
                TSError { fg = foreground },                                                                -- For syntax/parser errors.
                TSURI { fg = hsl(190, 80, 65), gui = 'underline' },                        -- Any URI like a link or email.
                Todo { fg = hsl(0, 0, 0), bg = hsl(60, 100, 50), gui = "bold" },           -- (preferred) anything that needs extra attention; mostly the keywords TODO FIXME and XXX
                -- TSNone               { };    -- TODO: docs

                AerialClassIcon { fg = syntax_statement, gui = 'bold' },
                AerialClass { fg = foreground },
                AerialConstructorIcon { fg = syntax_statement, gui = 'bold' },
                AerialConstructor { fg = foreground },
                AerialEnumIcon { fg = syntax_statement, gui = 'bold' },
                AerialEnum { fg = foreground },
                AerialFunctionIcon { fg = syntax_statement, gui = 'bold' },
                AerialFunction { fg = foreground },
                AerialInterfaceIcon { fg = syntax_statement, gui = 'bold' },
                AerialInterface { fg = foreground },
                AerialModuleIcon { fg = syntax_statement, gui = 'bold' },
                AerialModule { fg = foreground },
                AerialMethodIcon { fg = syntax_statement, gui = 'bold' },
                AerialMethod { fg = foreground },
                AerialStructIcon { fg = syntax_statement, gui = 'bold' },
                AerialStruct { fg = foreground },
                AerialLine { fg = foreground },
                AerialLineNC { fg = foreground },
                AerialGuide { fg = foreground_ui },

                -- TODO update:
                QuickFixLine { gui = "bold" },                             -- Current |quickfix| item in the quickfix window. Combined with |hl-CursorLine| when the cursor is there.
                WildMenu { fg = "#000000", bg = "#FFFF00", gui = "bold" }, -- current match in 'wildmenu' completion

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
                DiagnosticVirtualTextWarn { fg = "#efc809", bg = "#625234", gui = "bold" },  -- Used for "Warning" diagnostic virtual text
                DiagnosticVirtualTextInfo { fg = "#cccccc", bg = "#444454", gui = "bold" },  -- Used for "Information" diagnostic virtual text
                DiagnosticVirtualTextHint { fg = "#cccccc", bg = "#444454", gui = "bold" },  -- Used for "Hint" diagnostic virtual text

                DiagnosticUnderlineError {},                                                 -- Used for "Error" diagnostic virtual text
                DiagnosticUnderlineWarn {},                                                  -- Used for "Warning" diagnostic virtual text
                DiagnosticUnderlineInfo {},                                                  -- Used for "Information" diagnostic virtual text
                DiagnosticUnderlineHint {},                                                  -- Used for "Hint" diagnostic virtual text

                -- LspDiagnosticsSignError              { }, -- Used for "Error" signs in sign column
                -- LspDiagnosticsSignWarning            { }, -- Used for "Warning" signs in sign column
                -- LspDiagnosticsSignInformation        { }, -- Used for "Information" signs in sign column
                -- LspDiagnosticsSignHint               { }, -- Used for "Hint" signs in sign column

                SymbolsOutlineConnector { Normal },     -- connector highlight for symbols-outline.nvim
                FocusedSymbol { Normal, gui = "bold" }, -- focused symbol highlight for symbols-outline.nvim
                SymbolIcon { Comment, gui = "NONE" },   -- icon highlight for symbols-outline.nvim
                FidgetTitle { fg = foreground, gui = "bold" },
                FidgetTask { fg = foreground_ui },
                VirtColumn { fg = background.lighten(10) },

                -- Neotest:
                NeotestAdapterName { fg = special, bg = background, gui = 'bold' },
                NeotestBorder { fg = foreground_ui.darken(20) },
                NeotestExpandMarker { fg = foreground_ui.darken(20) },
                NeotestIndent { fg = foreground_ui.darken(20) },
                NeotestFailed { fg = error, bg = background, gui = 'bold' },
                NeotestPassed { fg = success, bg = background, gui = 'bold' },
                NeotestRunning { fg = foreground, bg = background, gui = 'bold' },
                NeotestSkipped { fg = skipped, bg = background, gui = 'bold' },
                NeotestUnknown { fg = foreground, bg = background },
                NeotestDir { fg = foreground_ui.lighten(20), bg = background },
                NeotestFile { fg = foreground_ui.lighten(20), bg = background },
                NeotestNamespace { fg = foreground_ui.lighten(20), bg = background },
                NeotestTarget { fg = foreground.darken(15), bg = background, gui = 'bold' },
                NeotestTest { fg = foreground, bg = background, gui = 'bold' },
                NeotestFocused {},
                NeotestMarked { fg = mark, bg = background, gui = 'bold' },
                NeotestWinSelect {}, -- ???

                -- NeoTreeBufferNumber
                -- NeoTreeCursorLine
                -- NeoTreeDimText
                -- NeoTreeDirectoryIcon
                -- NeoTreeDirectoryName
                -- NeoTreeDotfile
                -- NeoTreeFileIcon
                -- NeoTreeFileName
                -- NeoTreeFileNameOpened
                -- NeoTreeFilterTerm
                -- NeoTreeFloatBorder
                -- NeoTreeFloatTitle
                -- NeoTreeTitleBar
                NeoTreeGitAdded { fg = diff_add, gui = 'bold' },
                NeoTreeGitConflict { fg = error, gui = 'bold' },
                NeoTreeGitDeleted { fg = diff_delete, gui = 'bold' },
                NeoTreeGitIgnored { fg = syntax_comment, gui = 'bold' },
                NeoTreeGitModified { fg = diff_change, gui = 'bold' },
                NeoTreeGitUnstaged { fg = foreground_ui },
                NeoTreeGitUntracked { fg = foreground_ui, gui = 'bold' },
                NeoTreeGitStaged { fg = diff_add },
                -- NeoTreeHiddenByName
                -- NeoTreeIndentMarker
                -- NeoTreeExpander
                -- NeoTreeNormal
                -- NeoTreeNormalNC
                -- NeoTreeSignColumn
                -- NeoTreeStatusLine
                -- NeoTreeStatusLineNC
                -- NeoTreeVertSplit
                -- NeoTreeWinSeparator
                -- NeoTreeEndOfBuffer
                -- NeoTreeRootName
                -- NeoTreeSymbolicLinkTarget
                -- NeoTreeTitleBar
                -- NeoTreeWindoowsHidden

                TreeHighlight { bg = background.lighten(2) }, -- Highlight of the current treesitter node.

                InclineContent { bg = background.lighten(10) },
                InclineNormal { Normal },
                InclineNormalNC { InclineNormal }
        }
end)

-- return our parsed theme for extension or use else where.
return theme

-- vi:nowrap
