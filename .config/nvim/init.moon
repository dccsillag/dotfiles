-- Plugins

vim.cmd 'packadd packer.nvim'

plugins = ->
    plug = (name, table) ->
        table or= {}
        table[1] = name
        use table

    plugown = (name, table) ->
        maybepath = "~/code/#{name}"
        local resolved_name
        if (os.execute "[ -d #{maybepath} ]") == 1
            resolved_name = maybepath
        else
            resolved_name = "dccsillag/#{name}"
        plug resolved_name, table

    -- Core
    plug 'wbthomason/packer.nvim' -- plugin manager
    plug 'svermeulen/vimpeccable', config: -> -- convenience Lua functions for config
        vimp = require 'vimp'

        vimp.always_override = true
    plug 'LionC/nest.nvim' -- easy API for creating mappings
    plug 'nvim-lua/plenary.nvim' -- convenience Lua functions for plugins

    -- Interface
    plug 'junegunn/fzf', run: './install --bin' -- fuzzy finder
    plug 'ibhagwan/fzf-lua', requires: {'vijaymarupudi/nvim-fzf'}, config: -> -- fuzzy finder from neovim
        -- require 'fzf-lua'
    plug 'glepnir/dashboard-nvim', config: -> -- nice startup screen
        is_in_git_repo = os.execute("git rev-parse --is-inside-work-tree > /dev/null 2>&1") == 0

        vim.g.dashboard_custom_header = {
            -- ' ███╗   ██╗ ███████╗ ██████╗  ██╗   ██╗ ██╗ ███╗   ███╗',
            -- ' ████╗  ██║ ██╔════╝██╔═══██╗ ██║   ██║ ██║ ████╗ ████║',
            -- ' ██╔██╗ ██║ █████╗  ██║   ██║ ██║   ██║ ██║ ██╔████╔██║',
            -- ' ██║╚██╗██║ ██╔══╝  ██║   ██║ ╚██╗ ██╔╝ ██║ ██║╚██╔╝██║',
            -- ' ██║ ╚████║ ███████╗╚██████╔╝  ╚████╔╝  ██║ ██║ ╚═╝ ██║',
            -- ' ╚═╝  ╚═══╝ ╚══════╝ ╚═════╝    ╚═══╝   ╚═╝ ╚═╝     ╚═╝',
            [[                                                88                    ]],
            [[                                                                      ]],
            [[ 8b,dPPYba,   ,adPPYba,  ,adPPYba,  8b       d8 88 88,dPYba,,adPYba,  ]],
            [[ 88P'   `"8a a8P_____88 a8"     "8a `8b     d8' 88 88P'   "88"    "8a ]],
            [[ 88       88 8PP""""""" 8b       d8  `8b   d8'  88 88      88      88 ]],
            [[ 88       88 "8b,   ,aa "8a,   ,a8"   `8b,d8'   88 88      88      88 ]],
            [[ 88       88  `"Ybbd8"'  `"YbbdP"'      "8"     88 88      88      88 ]],
        }
        vim.g.dashboard_custom_footer = {"  @dccsillag"}
        vim.g.dashboard_custom_section =
            a:
                description: {"  New Buffer                                             :enew"}
                command: "enew"
            b:
                description: {"  New Markdown Buffer                    :enew | setf markdown"}
                command: "enew | setf markdown"
            c:
                description: do
                    if is_in_git_repo
                        {"  Search for files in git repo               :FzfLua git_files"}
                    else
                        {"  Search for files                               :FzfLua files"}
                command: do
                    if is_in_git_repo
                        "FzfLua git_files"
                    else
                        "FzfLua files"
            d: do
                if is_in_git_repo
                    {
                        description: {"  Open Fugitive                                      :G | only"}
                        command: "G | only"
                    }
                else
                    nil
            e:
                description: {"ﮮ  Update Plugins                                 :PackerUpdate"}
                command: "PackerUpdate"
            f:
                description: {"  Install Plugins                               :PackerInstall"}
                command: "PackerInstall"
        vim.cmd [[
            autocmd FileType dashboard set fillchars+=eob:\  | autocmd WinLeave <buffer> set fillchars-=eob
        ]]
    plug 'machakann/vim-highlightedyank' -- briefly highlight yanked region
    plug 'edluffy/specs.nvim', config: -> -- highlight cursor jumps
        export specs
        specs = require 'specs'
        specs.setup
            show_jumps: false -- (vim.fn.exists 'neovide') == 0
            min_jump: 2
            popup:
                delay_ms: 0
                inc_ms: 8
                blend: 60
                width: 25
                winhl: 'Specs'
                fader: specs.empty_fader
                resizer: specs.shrink_resizer
            ignore_filetypes: {"dashboard"}
            ignore_buftypes: {"nofile": true}
    plug 'folke/zen-mode.nvim', config: -> -- toggleable zen mode for editing
        (require "zen-mode").setup!
    plug 'lewis6991/gitsigns.nvim', config: -> -- show git diff in the signcolumn
        (require 'gitsigns').setup
            keymaps:
                noremap: true
                ['n [c']: { expr: true, "&diff ? '[c' : '<cmd>lua require\"gitsigns.actions\".prev_hunk()<CR>'" }
                ['n ]c']: { expr: true, "&diff ? ']c' : '<cmd>lua require\"gitsigns.actions\".next_hunk()<CR>'" }

                ["n <leader>gs"]: '<cmd>lua require"gitsigns".stage_hunk()<CR>'
                ["v <leader>gs"]: '<cmd>lua require"gitsigns".stage_hunk({vim.fn.line("."), vim.fn.line("v")})<CR>'
                ["n <leader>gu"]: '<cmd>lua require"gitsigns".undo_stage_hunk()<CR>'
                ["n <leader>gx"]: '<cmd>lua require"gitsigns".reset_hunk()<CR>'
                ["v <leader>gx"]: '<cmd>lua require"gitsigns".reset_hunk({vim.fn.line("."), vim.fn.line("v")})<CR>'
                -- ["n <leader>gX"]: '<cmd>lua require"gitsigns".reset_buffer()<CR>'
                ["n <leader>gd"]: '<cmd>lua require"gitsigns".preview_hunk()<CR>'
                ["n <leader>gb"]: '<cmd>lua require"gitsigns".blame_line(true)<CR>'

                ["o ih"]: ':<C-U>lua require"gitsigns.actions".select_hunk()<CR>'
                ["x ih"]: ':<C-U>lua require"gitsigns.actions".select_hunk()<CR>'
    plug 'junegunn/limelight.vim', config: -> -- a spotlight for code, good for presenting bit-by-bit
        vim.g.limelight_conceal_ctermfg = 242
        vim.g.limelight_conceal_guifg = '#606060'
    plug 'junegunn/goyo.vim' -- make things pretty, for more elegant presentations
    plug 'lukas-reineke/indent-blankline.nvim', config: -> -- indent guides
        vim.g.indent_blankline_char = '⎜'
        vim.g.indent_blankline_show_trailing_blankline_indent = false
        vim.g.indent_blankline_buftype_exclude = {'terminal'}
        vim.g.indent_blankline_filetype_exclude = {'aerial', 'dashboard', 'packer'}
    -- plug 'romgrk/nvim-treesitter-context', config: -> -- show code context on top of the buffer
    --     (require 'treesitter-context').setup
    --         enable: true
    --         throttle: true
    plug 'wellle/context.vim', config: -> -- show code context on top of the buffer
        vim.g.context_nvim_no_redraw = 1
    plug 'kshenoy/vim-signature' -- show marks in the sign column
    plug 'rcarriga/nvim-notify', config: -> -- show notifications
        vim.notify = require 'notify'
    plug 'dstein64/vim-startuptime' -- profile startup time neatly

    -- Behaviour
    plug 'tpope/vim-repeat' -- better `.` key
    plug 'chaoren/vim-wordmotion', config: -> -- improve the `w` key and similar
        vim.g.wordmotion_spaces = ''
    plug 'haya14busa/vim-asterisk' -- improve `*` and `#`
    plug 'embear/vim-localvimrc', config: -> -- for using local [e.g., project-specific] vimrcs
        vim.g.localvimrc_persistent = 1
        vim.g.localvimrc_persistence_file = vim.fn.expand "~/.local/misc/localvimrc_persistent"
    plug 'vim-scripts/let-modeline.vim' -- have a specific modeline for configuring plugins
    plug 'lambdalisue/suda.vim', config: -> -- for editting files which require root permission
        vim.g.suda_smart_edit = 1

    -- Peripherals
    plug 'tpope/vim-eunuch' -- add nice commands for shell commands
    plug 'skywind3000/asyncrun.vim' -- for running stuff in the background, async
    plug 'itspriddle/vim-shellcheck' -- run shellcheck from Vim as a :compiler
    plug 'tpope/vim-fugitive' -- use git from vim
    plug 'christoomey/vim-conflicted' -- easily solve git merge conflicts
    plug 'junegunn/gv.vim' -- git commit browser
    plugown 'magma-nvim', run: ':UpdateRemotePlugins', config: -> -- interact with Jupyter
        vim.g.magma_automatically_open_output = false
        vim.g.magma_show_mimetype_debug = false

    -- LSP / TreeSitter / Formatting
    plug 'hrsh7th/nvim-compe', config: -> -- completion framework for NeoVim
        (require 'compe').setup
            source:
                spell: false
                path: true
                nvim_lsp: true
                buffer: false
                calc: true
                nvim_lua: false
                vsnip: false
                ultisnips: false
                luasnip: false
    plug 'nvim-lua/lsp_extensions.nvim' -- extra easy configurations for LSP
    plug 'stevearc/aerial.nvim', config: -> -- window with outline of symbols
        vim.g.aerial =
            default_direction: "left"
            post_jump_cmd: [[normal zt\qa]]
            min_width: 40
            max_width: 40

        vim.cmd [[autocmd FileType aerial setl listchars-=trail:┈]]
    plug 'neovim/nvim-lspconfig', config: -> -- easily configure LSP
        nvim_lsp = require 'lspconfig'
        aerial = require 'aerial'

        on_attach = (client) ->
            vim.notify "Ready.", "info", title: "LSP", timeout: 500
            aerial.on_attach client

            import nnoremap, vnoremap from require 'vimp'

            nnoremap {'silent'}, '<C-k>',     -> vim.lsp.diagnostic.show_line_diagnostics()
            nnoremap {'silent'}, '<C-]>',     -> vim.lsp.buf.definition()
            nnoremap {'silent'}, 'K',         -> vim.lsp.buf.hover()
            nnoremap {'silent'}, 'gd',        -> vim.lsp.buf.declaration()
            nnoremap {'silent'}, 'gD',        -> vim.lsp.buf.implementation()
            nnoremap {'silent'}, '1gD',       -> vim.lsp.buf.type_definition()
            nnoremap {'silent'}, 'gr',        -> vim.lsp.buf.references()
            nnoremap {'silent'}, 'g0',        -> vim.lsp.buf.document_symbol()
            nnoremap {'silent'}, '<Leader>a', -> vim.lsp.buf.code_action()
            vnoremap {'silent'}, '<Leader>a', -> vim.lsp.buf.range_code_action()
            nnoremap {'silent'}, 'gq',        -> vim.lsp.buf.formatting()

            nnoremap {'silent'}, '[g', -> vim.lsp.diagnostic.goto_prev()
            nnoremap {'silent'}, ']g', -> vim.lsp.diagnostic.goto_next()

            nnoremap {'silent'}, '<Leader>la', -> vim.cmd 'AerialOpen'
            nnoremap {'silent'}, '<Leader>qa', -> vim.cmd 'AerialClose'
            nnoremap {'silent'}, '[[', -> vim.cmd 'AerialPrev'
            nnoremap {'silent'}, ']]', -> vim.cmd 'AerialNext'

        -- Configure LSPs
        nvim_lsp.rust_analyzer.setup
            :on_attach
            settings:
                ["rust-analyzer"]:
                    cargo:
                        loadOutDirsFromCheck: true
                    procMacro:
                        enable: true
                    diagnostics:
                        enable: false
        nvim_lsp.clangd.setup
            :on_attach
            cmd: {"clangd", "--background-index", "--clang-tidy"}
        nvim_lsp.sumneko_lua.setup
            :on_attach
            cmd: {"lua-language-server", "-E", "/usr/share/lua-language-server/main.lua"}
        nvim_lsp.pyright.setup :on_attach
        nvim_lsp.hls.setup :on_attach
        nvim_lsp.texlab.setup :on_attach
        nvim_lsp.vimls.setup :on_attach

        vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with vim.lsp.diagnostic.on_publish_diagnostics,
                virtual_text: true
                underline: true
                signs: false
                update_in_insert: true
    plug 'nvim-treesitter/nvim-treesitter', 'run': ':TSUpdate', config: -> -- easy treesitter config
        (require 'nvim-treesitter.configs').setup
            highlight:
                enable: true
                additional_vim_regex_highlighting: false
            incremental_selection:
                enable: true
                keymaps:
                    init_selection: 'g.'
                    node_decremental: ','
                    node_incremental: '.'
            indent:
                enable: false -- currently, this is broken.
    plug 'folke/trouble.nvim', config: -> -- list code troubles
        (require 'trouble').setup
            icons: false
            signs:
                error: "E"
                warning: "W"
                information: "I"
                hint: "H"
            use_lsp_diagnostic_signs: true

    -- Color Schemes
    plug 'rktjmp/lush.nvim', branch: 'main' -- easily create color schemes for NeoVim
    plug 'joshdick/onedark.vim' -- onedark colorscheme from Atom
    plug 'pbrisbin/vim-colors-off' -- a plain colorscheme that pretty much disables highlighting
    plug 'arcticicestudio/nord-vim' -- nord colorscheme
    plug 'arzg/vim-substrata' -- a cold, dark colorscheme for Vim

    -- Editing Help
    plug 'tpope/vim-surround' -- surround text with stuff [parentheses, brackets, and much more]
    plug 'tomtom/tcomment_vim' -- comment/uncomment code
    plug 'tmsvg/pear-tree', config: -> -- automatic delimiter pair closing
        vim.g.pear_tree_repeatable_expand = 0

        export all_peartree_pairs
        all_peartree_pairs =
            basic_pairs: -- parentheses, bracktes, etc.
                ["("]: closer: ")"
                ["["]: closer: "]"
                ["{"]: closer: "}"
            quotes_and_apostrophes: -- quotes, apostrophes, etc.
                ["\""]: closer: "\""
                ["'"]: closer: "'"
                ["`"]: closer: "`"
            math_dolars: -- inline&display math
                ["$"]: closer: "$"
                ["$$"]: closer: "$$"
            texmath: -- TeX Math
                -- -- delimiters
                ["\\\\("]: closer: "\\\\)"
                ["\\\\["]: closer: "\\\\]"
                ["\\\\left("]: closer: "\\\\right)"
                ["\\\\left["]: closer: "\\\\right]"
                ["\\\\{"]: closer: "\\\\}"
                ["\\\\left\\\\{"]: closer: "\\\\right\\\\}"
                ["\\\\lceil"]: closer: "\\\\rceil"
                ["\\\\left\\\\lceil"]: closer: "\\\\right\\\\rceil"
                ["\\\\lfloor"]: closer: "\\\\rfloor"
                ["\\\\left\\\\lfloor"]: closer: "\\\\right\\\\rfloor"
                ["\\\\lvert"]: closer: "\\\\rvert"
                ["\\\\left\\\\lvert"]: closer: "\\\\right\\\\rvert"
                ["\\\\lVert"]: closer: "\\\\rVert"
                ["\\\\left\\\\lVert"]: closer: "\\\\right\\\\rVert"
                -- -- multi-args
                ["\\\\frac"]: closer: "{}{}"
                ["\\\\inn"]: closer: "{}{}"
                ["\\\\diff"]: closer: "{}{}"
                ["\\\\pdiff"]: closer: "{}{}"
                ["\\\\sqrt["]: closer: "]{}"
                -- -- LaTeX environments
                ["\\\\begin{*}"]: closer: "\\\\end{*}", until: "}"
            xml: -- XML tags
                ["<!--"]: closer: "-->"
                ["<*>"]: closer: "</*>", until: "\\W"
            markdown: -- Markdown formatting
                ["\\*"]: closer: "\\*"
                ["\\*\\*"]: closer: "\\*\\*"
                ["```"]: closer: "```"
                [":::"]: closer: ":::"
            templates: -- c++ templates / rust type variables
                ["<"]: closer: ">", not_at: {"\\W", "^"}

        export pairs_per_filetype, set_pairs_for_filetype
        pairs_per_filetype =
            xml:        {'xml'}
            html:       {'xml'}
            tex:        {'basic_pairs', 'math_dolars', 'texmath'}
            markdown:   {'basic_pairs', 'math_dolars', 'markdown', 'xml'}
            python:     {'basic_pairs', 'quotes_and_apostrophes', 'templates'}
            rust:       {'basic_pairs', 'quotes_and_apostrophes', 'templates'}
            cpp:        {'basic_pairs', 'quotes_and_apostrophes', 'templates'}
            python:     {'basic_pairs', 'quotes_and_apostrophes'}
            -- TODO: javascript
            -- TODO: haskell
            -- TODO: lua
        set_pairs_for_filetype = (filetype) ->
            outpairs = {}
            pair_names_for_filetype = pairs_per_filetype[filetype]
            if pair_names_for_filetype == nil
                outpairs = vim.g.pear_tree_pairs
            else
                for _, subpairs_name in ipairs pair_names_for_filetype
                    subpairs = all_peartree_pairs[subpairs_name]
                    for k, v in pairs subpairs
                        outpairs[k] = v
            vim.b.pear_tree_pairs = outpairs

        vim.cmd [[autocmd FileType * lua set_pairs_for_filetype(vim.o.filetype)]]

        vim.g.pear_tree_pairs = basic_pairs
    plug 'junegunn/vim-easy-align' -- align code
    plug 'AndrewRadev/splitjoin.vim' -- change code between inline and multiline forms
    plug 'dhruvasagar/vim-table-mode', config: -> -- painlessly edit tables
        vim.g.table_mode_map_prefix = '<Leader><Bar>'
        vim.g.table_mode_toggle_map = '<Bar>'
    plug 'tommcdo/vim-exchange' -- exchange text around
    plug 'svermeulen/vim-subversive' -- replace text with current yank
    plug 'godlygeek/tabular' -- aligns text, required by vim-markdown
    plug 'monaqa/dial.nvim' -- better increment/decrement

    -- Text Objects
    plug 'wellle/targets.vim' -- better text objects
    plug 'michaeljsmith/vim-indent-object' -- text object for indented text
    plug 'kana/vim-textobj-entire' -- text object for the entire buffer
    plug 'kana/vim-textobj-syntax' -- text object for text in the same highlight group
    plug 'kana/vim-textobj-user' -- framework for creating text objects [used by other plugins]

    -- Language Support
    plug 'vim-python/python-syntax', config: -> -- better Python syntax highlight
        vim.g.python_version_2                          = false
        vim.g.python_highlight_builtins                 = true
        vim.g.python_highlight_builtin_objs             = true
        vim.g.python_highlight_builtin_types            = true
        vim.g.python_highlight_builtin_funcs            = true
        vim.g.python_highlight_builtin_funcs_kwarg      = false
        vim.g.python_highlight_exceptions               = true
        vim.g.python_highlight_string_formatting        = true
        vim.g.python_highlight_string_format            = true
        vim.g.python_highlight_string_templates         = true
        vim.g.python_highlight_indent_errors            = false
        vim.g.python_highlight_space_errors             = false
        vim.g.python_highlight_doctests                 = true
        vim.g.python_highlight_func_calls               = false
        vim.g.python_highlight_class_vars               = true
        vim.g.python_highlight_operators                = true
        vim.g.python_highlight_file_headers_as_comments = true
    plug 'petRUShka/vim-sage' -- language support for SageMath
    plug 'Vimjas/vim-python-pep8-indent' -- indent Python code according to PEP8
    plug 'bfrg/vim-cpp-modern', config: -> -- better C++ syntax highlight
        vim.g.cpp_no_function_highlight        = false
        vim.g.cpp_named_requirements_highlight = true
    plug 'gabrielelana/vim-markdown', config: -> -- better Markdown support
        vim.g.markdown_enable_insert_mode_leader_mappings = false
    plug 'leafo/moonscript-vim' -- language support for MoonScript
    plug 'rubik/vim-dg' -- language support for DogeLang [aka. dg]
    plug 'manicmaniac/coconut.vim' -- language support for Coconut
    plug 'rust-lang/rust.vim' -- better Rust syntax support
    plug 'neovimhaskell/haskell-vim' -- better support for Haskell
    plug 'edwinb/idris2-vim' -- language support for Idris
    plug 'mrk21/yaml-vim' -- better language support for YAML
    plug 'cespare/vim-toml' -- language support for TOML
    plug 'LnL7/vim-nix' -- language support for Nix
    plug 'MaxMEllon/vim-jsx-pretty' -- language support for JSX
    plug 'dart-lang/dart-vim-plugin' -- language support for Dart
    plug 'tikhomirov/vim-glsl' -- language support for GLSL
    plug 'aklt/plantuml-syntax' -- for PlantUML syntax support
    plug 'wlangstroth/vim-racket' -- language support for Racket
    plug 'goerz/jupytext.vim' -- edit Jupyter notebooks in vim
    plug 'pest-parser/pest.vim' -- language support for Pest grammars

packer_config =
    max_jobs: 8
    display:
        open_fn: -> (require 'packer.util').float border: 'single'
        working_sym: "•"
        error_sym: "✗"
        done_sym: "✓"
        removed_sym: "-"
        moved_sym: "→"
        header_sym: "━"
(require 'packer').startup {plugins, config: packer_config}

-- Neovide (GUI) options
vim.g.neovide_transparency = 0.75
vim.o.guifont = 'FantasqueSansMono Nerd Font:12'

-- Lots of Vim options
do
    -- Set the regex engine
    vim.o.regexpengine = 1

    --- Enable the mouse
    vim.o.mouse = 'a' -- enable all mouse features

    --- Setup statusbar
    vim.o.showmode = false -- don't show the current mode below the statusbar
    vim.o.laststatus = 0 -- only show the statusline when in between two windows
    vim.o.statusline = [[%{repeat('―', nvim_win_get_width(0))}]] -- set the statusline to a horizontal separator
    vim.o.ruler = false -- remove the ruler

    --- Hide tabbar
    vim.o.showtabline = 0 -- never show the tabline

    --- Setup :grep
    vim.o.grepprg = [[grep -R --exclude-dir=.git -n $* . /dev/null]]

    --- Setup line wrap
    vim.o.wrap = false -- wrap lines
    vim.o.linebreak = true -- don't break words on wrap
    vim.o.breakindent = true -- indent wrapped lines
    vim.o.showbreak = "… " -- prefix for wrapped lines

    --- Set "start of line" mode
    vim.o.startofline = true

    --- Set case sensitiveness of the search
    vim.o.ignorecase = true -- by default, case insensitive
    vim.o.smartcase = true -- but if there are capital letters, then case sensitive

    --- Setup the foldcolumn
    vim.o.foldcolumn = 'auto:5' -- automatically manage foldcolumn width

    --- Show the signcolumn
    vim.o.signcolumn = 'auto:3' -- automatically resize signcolumn to show at most 3 signs

    --- Setup folding
    vim.o.foldenable = false -- disable folding
    vim.o.foldminlines = 1 -- require at least 10 lines of content to create a fold
    vim.o.foldnestmax = 5 -- set maximum amount of fold nesting

    --- Use 4 spaces instead of tabs
    vim.o.expandtab = true -- expand tabs
    vim.o.shiftwidth = 4   -- how many sapces to use for >> and <<
    vim.o.softtabstop = 4  -- how many sapces to use for tab

    --- Keep indentation structure
    vim.o.autoindent = true -- use same indentation level for the neighbouring lines
    vim.o.cinoptions = [[L0,N-s,E-s,(0,mN,j1,J1,P1]] -- setup better C/C++ autoindent

    --- Highlight past the textwidth
    vim.o.colorcolumn = '+1' -- highlight the column after the one specified in 'textwidth'

    --- Set the updatetime
    vim.o.updatetime = 250 -- how much time between updates, in ms

    --- Remove the bell sound
    vim.o.belloff = 'all' -- disable the bell everywhere

    --- Fix backspace
    vim.o.backspace = "2" -- when in insert mode, backspace will remove indentation and newlines

    --- Open new vertical splits to the right and horizontal splits below
    vim.o.splitright = true -- open new vertical splits to the right
    vim.o.splitbelow = true -- open new horizontal splits below

    --- Setup undo persistence
    vim.o.undofile = true -- save undos to an undo file

    --- Allow hidden buffers
    vim.o.hidden = true -- hide buffers when leaving them, instead of deleting them

    --- Setup views
    vim.o.viewoptions = 'cursor' -- save only the cursor position in a view

    --- Highlight search results
    vim.o.hlsearch = true -- highlight search results

    --- Preview search/substitute pattern matches
    vim.o.incsearch = true -- live preview search results
    vim.o.inccommand = 'nosplit' -- live preview command (:s, :g, etc.) results

    --- Show possible Ex command completions above
    vim.o.wildmenu = true -- show completion menu in Ex commands

    --- Setup conceal
    vim.o.concealcursor = '' -- don't conceal the current line in any mode
    vim.o.conceallevel = 2 -- conceal everything, hiding concealed text completely

    --- Show trailing whitespace and tabs
    vim.o.list = true -- show listchars
    vim.o.listchars = [[tab:--,trail:┈]] -- highlight tabs and trailing whitespace

    --- Setup autocompletion in a way that is better
    vim.o.completeopt = 'menuone,noinsert,noselect' -- menuone: show a menu even if there's only one match
                                                    -- noinsert: only insert when we select
                                                    -- noselect: no automatic selection
    vim.o.complete = '.,i,d' -- look for completions in the current buffer (.) and in the included files (i,d)

    --- Setup verbosity
    vim.o.shortmess = [[filnxtToOFc]] -- don't show messages regarding completion

    --- Always keep 2 lines around the cursor
    vim.o.scrolloff = 2 -- keep 2 lines above&below the cursor at all times

    --- Setup sessions
    vim.o.sessionoptions = [[blank,buffers,curdir,help,tabpages,winsize,tabpages,globals]]

    --- Accelerate Esc presses
    vim.o.ttimeout = true -- enable timeout
    vim.o.ttimeoutlen = 50 -- timeout for keycodes
    vim.o.timeoutlen = 500 -- timeout for mappings

    --- Enable truecolors
    vim.o.termguicolors = true -- use trucolor in a terminal (i.e., use gui colors in a terminal)

-- Apply my color scheme
vim.cmd 'colorscheme csillag'

-- Remappings
do
    import nnoremap, vnoremap, onoremap, tnoremap, nmap, vmap, map from require 'vimp'

    -- Abbreviate :w to :up
    vim.cmd [[cnoreabbrev w up]]

-- Autocommands
do
    import map_command from require 'vimp'

    -- Automatically compile init.moon into init.lua
    map_command 'CompileInit', ->
        vim.cmd 'cexpr system("moonc ~/.config/nvim/init.moon")'
        vim.cmd 'silent !nvim -u ~/.config/nvim/init.lua -Es +PackerCompile'
    vim.cmd "autocmd BufWritePost #{vim.fn.expand('~/.config/nvim/init.moon')} CompileInit"

    -- AutoView
    -- vim.cmd [[
    --     augroup autoview
    --         autocmd!
    --         autocmd BufWinLeave,VimLeave,BufWritePost * if expand("%") != "" | silent! mkview | endif
    --         autocmd BufWinEnter,BufReadPost * if expand("%") != "" | silent! loadview | endif
    --     augroup END
    -- ]]

    -- SHADA
    vim.cmd [[
        augroup shada
            autocmd!
            autocmd FocusGained * if exists(':rshada') | rshada | endif
            autocmd FocusLost   * if exists(':wshada') | wshada | endif
        augroup END
    ]]

    -- SpellCheck
    vim.cmd [[
        augroup spellcheck
            autocmd!
            autocmd FileType markdown  set spell
            autocmd FileType tex       set spell
            autocmd FileType html      set spell
            autocmd FileType gitcommit set spell
        augroup END
    ]]

    -- More filetype detection
    vim.cmd [[
        augroup FtdetectExtra
            autocmd!
            autocmd BufRead,BufNewFile *.lmd setf markdown
            autocmd BufRead,BufNewFile *.pmd setf markdown
        augroup END
    ]]

    -- Autommatic compilation of markup documents
    vim.cmd [[
        augroup autocompile
            autocmd!
            autocmd BufReadPre *.tex               compiler latexrun
            autocmd BufReadPre *.lmd               compiler pan-latex
            autocmd BufReadPre *.pmd               compiler pan-revealjs
            autocmd BufReadPre *.mmd               compiler mermaid
            autocmd BufReadPre *.uml               compiler plantuml
            autocmd BufReadPre *.ly,*.ily          compiler lilypond
            autocmd BufReadPre *.c,*.h,*.cpp,*.hpp compiler tap

            autocmd BufWritePost *.tex,*.lmd,*.pmd,*.mmd,*.uml,*.ly,*.ily AsyncStop | sleep 100m | AsyncRun -program=make
        augroup END
    ]]

-- Functionality
do
    export open_corresponding_pdf
    open_corresponding_pdf = ->
        filename = vim.fn.expand("%:r") .. ".pdf"
        if not vim.fn.filereadable filename
            vim.cmd "echoerr 'No such file: #{filename}'"
            return
        vim.cmd "silent exec '!nohup zathura #{filename} > /dev/null 2>&1 &'"

    export insert_line_above
    insert_line_above = ->
        count = vim.v.count1 - 1
        vim.cmd [[norm! Oa]]
        for _ = 1, count
            vim.cmd [[put _]]
            vim.cmd [[norm! k]]
        vim.cmd [[norm! ^"_D]]
        vim.cmd [[startinsert!]]
    export insert_line_below
    insert_line_below = ->
        count = vim.v.count1 - 1
        vim.cmd [[norm! oa]]
        for _ = 1, count
            vim.cmd [[put! _]]
            vim.cmd [[norm! j]]
        vim.cmd [[norm! ^"_D]]
        vim.cmd [[startinsert!]]

-- Mappings
do
    import applyKeymaps from require 'nest'

    EXPR_PREFIX = '<expr>'
    RECURSIVE_PREFIX = '<rec>'
    VISUAL_PREFIX = '<visual>'
    INSERT_PREFIX = '<insert>'
    myKeymaps = (maps) ->
        out = {}
        for k, v in pairs maps
            local mode
            if k\sub(1, #VISUAL_PREFIX) == VISUAL_PREFIX
                k = k\sub(#VISUAL_PREFIX+1, #k)
                mode = 'v'
            elseif k\sub(1, #INSERT_PREFIX) == INSERT_PREFIX
                k = k\sub(#INSERT_PREFIX+1, #k)
                mode = 'i'
            else
                mode = 'n'

            local expr
            if k\sub(1, #EXPR_PREFIX) == EXPR_PREFIX
                k = k\sub(#EXPR_PREFIX+1, #k)
                expr = true
            else
                expr = fase

            local noremap
            if k\sub(1, #RECURSIVE_PREFIX) == RECURSIVE_PREFIX
                k = k\sub(#RECURSIVE_PREFIX+1, #k)
                noremap = false
            else
                noremap = true

            if type(v) == 'table'
                v = myKeymaps v

            out[#out+1] =
                [1]: k
                [2]: v
                :mode
                options:
                    :expr
                    :noremap
        out

    vimcmd = (cmd) -> ':' .. cmd .. '<CR>'
    luacmd = (cmd) -> '<cmd>lua ' .. cmd .. '<CR>'
    plug = (op) -> '<Plug>(' .. op .. ')'
    addspecs = (op) -> op .. ':lua specs.show_specs()<CR>'
    fzflua = (provider) -> vimcmd 'FzfLua ' .. provider

    applyKeymaps myKeymaps
        '<Leader>':
            -- FZF
            'e': fzflua 'files'
            'ge': fzflua 'git_files'
            'b': fzflua 'buffers'
            'H': fzflua 'help_tags'
            -- 'F': fzflua 'Filetypes'

            -- LSP-related
            'l':
                'r': fzflua 'lsp_references'
                'g': fzflua 'lsp_live_workspace_symbols'
                'G': fzflua 'lsp_document_symbols'
                'i': fzflua 'lsp_implementations'
                ']': fzflua 'lsp_definitions'
                'd': fzflua 'lsp_workspace_diagnostics'
                'D': fzflua 'lsp_document_diagnostics'

            -- magma-nvim
            'r':
                '<expr>': 'nvim_exec("MagmaEvaluateOperator", v:true)'
                '<visual>': ':<C-u>MagmaEvaluateVisual<CR>'
                'r': vimcmd 'MagmaEvaluateLine'
                'c': vimcmd 'MagmaReevaluateCell'
                'd': vimcmd 'MagmaDelete'
                'o': vimcmd 'MagmaShowOutput'
                'n': vimcmd 'MagmaInit'

            'G': vimcmd 'G' -- open fugitive

            ';': luacmd 'specs.show_specs()' -- highlight the cursor

            'S': vimcmd 'set spell!' -- toggle spell
            -- 's': ':set spelllang=' -- set spell language

            'p': luacmd 'open_corresponding_pdf()' -- open corresponding PDF for the current [markup file]

            '.': vimcmd 'nohl' -- :nohl

            'o': -- open windows
                'c': vimcmd 'copen' -- open quickfix window
                't': vimcmd 'Trouble' -- open trouble window
            'q': -- close windows
                'c': vimcmd 'cclose' -- close quickfix window
                't': vimcmd 'TroubleClose' -- close trouble window

        -- substitute
        '<C-s>':
            '<rec>':      plug 'SubversiveSubstitute'
            '<rec><C-s>': plug 'SubversiveSubstituteLine'

        -- dial.nvim
        '<rec><C-a>':         plug 'dial-increment'
        '<rec><C-x>':         plug 'dial-decrement'
        '<rec><visual><C-a>': plug 'dial-increment'
        '<rec><visual><C-x>': plug 'dial-decrement'
        '<rec>g<C-a>':        plug 'dial-increment-additional'
        '<rec>g<C-x>':        plug 'dial-decrement-additional'

        'g':
            -- text alignment
            -- TODO: xmap ga <Plug>(EasyAlign)
            '<rec>a': plug 'EasyAlign'

            -- window movement
            'h': '<C-w>h'
            'j': '<C-w>j'
            'k': '<C-w>k'
            'l': '<C-w>l'

            -- window restructuring
            'H': '<C-w>H'
            'J': '<C-w>J'
            'K': '<C-w>K'
            'L': '<C-w>L'

            -- misc window operations
            'w':
                'n': '<C-w>n'
                'o': '<C-w>o'
                'q': '<C-w>q'

        -- nvim-compe
        '<insert><expr><C-n>': 'compe#complete()'
        '<insert><expr><C-p>': 'compe#complete()'
        '<insert><expr><C-l>': 'compe#confirm("<C-l>")'
        '<insert><expr><C-c>': 'compe#close("<C-e>")'

        -- specs & asterisk
        'n': addspecs 'n'
        'N': addspecs 'N'
        '<rec>*': addspecs plug 'asterisk-*'
        '<rec>#': addspecs plug 'asterisk-#'
        '<rec>g*': addspecs plug 'asterisk-g*'
        '<rec>g#': addspecs plug 'asterisk-g#'
        '<rec>z*': addspecs plug 'asterisk-z*'
        '<rec>z#': addspecs plug 'asterisk-z#'
        '<rec>gz*': addspecs plug 'asterisk-gz*'
        '<rec>gz#': addspecs plug 'asterisk-gz#'

        -- swap "go to mark" maps
        "'": "`"
        "`": "'"

        -- Make Y work like D
        'Y': 'y$'

        -- Unmap Q
        'Q': '<nop>'

        -- Change S to behave kinda like X
        'S': '"_Xi<CR><Esc>l'

        -- Change s to ys (normal) / S (visual)
        '<rec>s': 'ys'
        '<visual><rec>s': 'S'

        -- Change o/O to not insert text at multiple lines, when used with a count
        'O': luacmd "insert_line_above()"
        'o': luacmd "insert_line_below()"
