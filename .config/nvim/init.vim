"  _         _  _            _
" (_) _ __  (_)| |_  __   __(_) _ __ ___
" | || '_ \ | || __| \ \ / /| || '_ ` _ \
" | || | | || || |_  _\ V / | || | | | | |
" |_||_| |_||_| \__|(_)\_/  |_||_| |_| |_|
"
" @author Daniel Csillag (aka. dccsillag)
" @what My NeoVim configuration.

" Plugins
"{{{
" Check if vim-plug is installed. If it isn't, than install it (XXX: only
"    works on linux):
" if empty(glob('~/.vim/autoload/plug.vim'))
"     echom "Installing vim-plug..."
"     silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
"                 \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
"     autocmd VimEnter * PlugInstall
" endif

" A function to plug my own plugins, which if available should be used from
" the local machine.
function s:PlugOwn(plugin_name) "{{{
    if isdirectory(expand('~/code/' . a:plugin_name))
        execute "Plug '" . '~/code/' . a:plugin_name . "/'"
    else
        execute "Plug 'dccsillag/" . a:plugin_name . "'"
    endif
endfunction "}}}

"{{{

" netrw (configure NetRW, mainly the file tree)
let g:netrw_liststyle=3 " {{{}}}

" haskell.vim (setup Haskell syntax highlight)
let hs_highlight_delimiters        = 1 "{{{
let hs_highlight_boolean           = 1
let hs_highlight_types             = 1
let hs_highlight_more_types        = 1
let hs_highlight_debug             = 1
let hs_allow_hash_operator         = 1
let g:haskell_classic_highlighting = 1 "}}}


if has("nvim") "{{{
    call plug#begin(stdpath('data') . '/plugged')
else
    call plug#begin('~/.vim/plugged')
endif

" My colorscheme
call s:PlugOwn('csillag-color') " {{{}}}

" Coc (LSP)
Plug 'neoclide/coc.nvim', {'branch': 'release'} "{{{

" Use Tab for completion
function! s:check_back_space() abort
    let col = col('.') - 1
    return !col || getline('.')[col - 1] =~# '\s'
endfunction
inoremap <silent><expr> <Tab>
            \ pumvisible() ? "\<C-n>" :
            \ <SID>check_back_space() ? "\<Tab>" :
            \ coc#refresh()
inoremap <expr><S-Tab> pumvisible() ? "\<C-p>" : "\<C-h>"
" Use [e and ]e to navigate diagnostics
nmap <silent> [e <Plug>(coc-diagnostic-prev)
nmap <silent> ]e <Plug>(coc-diagnostic-next)
" GoTo code navigation
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> grg <Plug>(coc-references)
" Use K to show documentation in preview window
function! s:show_documentation()
    if (index(['vim', 'help'], &filetype) >= 0)
        execute 'h '.expand('<cword>')
    else
        call CocAction('doHover')
    endif
endfunction
nnoremap <silent> K :call <SID>show_documentation()<CR>
" Symbol renaming
nmap <Leader>n <Plug>(coc-rename)
" TODO - text objects (in CoC README)
" List stuff
nnoremap <silent><nowait> <space>a :<C-u>CocList diagnostics<CR>
nnoremap <silent><nowait> <space>o :<C-u>CocList outline<CR>
nnoremap <silent><nowait> <space>j :<C-u>CocNext<CR>
nnoremap <silent><nowait> <space>k :<C-u>CocPrev<CR>

let g:coc_global_extensions = [
            \ "coc-json",
            \ "coc-python",
            \ "coc-vimlsp",
            \ "coc-sh",
            \ "coc-markdownlint",
            \ "coc-lsp-wl",
            \ "coc-html",
            \ "coc-tsserver"
            \ ]

"}}}

" vim-shellcheck (for running shellcheck from Vim, without using ALE)
Plug 'itspriddle/vim-shellcheck' "{{{

nnoremap <Leader>a :ShellCheck!<CR>

" }}}

" asyncrun.vim (??? required by another plugin, don't remember which)
Plug 'skywind3000/asyncrun.vim' "{{{}}}

" auto-pairs-gentle (automatically close parentheses and much more)
Plug 'vim-scripts/auto-pairs-gentle' "{{{
Plug 'jiangmiao/auto-pairs'

let g:AutoPairsUseInsertedCount = 1

" }}}

" coconut.vim (language support for Coconut)
Plug 'manicmaniac/coconut.vim' "{{{}}}

" fzf (fuzzy finder)
Plug 'junegunn/fzf' "{{{

augroup fzfcsillag
    autocmd!
    autocmd FileType fzf set laststatus=0
                \| autocmd BufLeave <buffer> set laststatus=2
augroup END

"}}}

" fzf.vim ("official" fzf addons)
Plug 'junegunn/fzf.vim' "{{{

nnoremap <Leader>e  :Files<CR>
nnoremap <Leader>t  :BTags<CR>
nnoremap <Leader>T  :Tags<CR>
nnoremap <Leader>b  :Buffers<CR>
nnoremap <Leader>ge :GFiles<CR>
nnoremap <Leader>gc :Commits<CR>
nnoremap <Leader>gC :BCommits<CR>
nnoremap <Leader>H  :Helptags<CR>
nnoremap <Leader>F  :Filetypes<CR>

"}}}

" dart-vim-plugin (language support for Dart)
Plug 'dart-lang/dart-vim-plugin' "{{{}}}

" let-modeline.vim (have a specific modeline for configuring plugins)
Plug 'vim-scripts/let-modeline.vim' "{{{}}}

" limelight.vim (a spotlight for code, good for presenting bit-by-bit)
Plug 'junegunn/limelight.vim' "{{{

let g:limelight_conceal_ctermfg = 242
let g:limelight_conceal_guifg   = '#606060'

"}}}

" goyo.vim (make things pretty, useful for when I'm more tired or for more
"           elegant presentations)
Plug 'junegunn/goyo.vim' "{{{}}}

" onedark.vim (OneDark colorscheme from Atom)
Plug 'joshdick/onedark.vim' "{{{}}}

" targets.vim (better text objects)
Plug 'wellle/targets.vim' "{{{}}}

" vCoolor.vim (color selector)
Plug 'KabbAmine/vCoolor.vim' "{{{

let g:vcoolor_disable_mappings = 1
" let g:vcoolor_map              = '<Leader>cc'

nnoremap <Leader>crgb :VCoolIns r<CR>
nnoremap <Leader>chsl :VCoolIns h<CR>
nnoremap <Leader>chex :VCoolor<CR>

"}}}

" tcomment_vim (comment/uncomment code)
Plug 'tomtom/tcomment_vim'

" vim-cpp-modern (better C++ syntax highlight)
Plug 'bfrg/vim-cpp-modern' "{{{

let g:cpp_no_function_highlight        = 0
let g:cpp_named_requirements_highlight = 1

"}}}

" vim-dg (language support for DogeLang [lol])
Plug 'rubik/vim-dg' "{{{}}}

" vim-easy-align (align code)
Plug 'junegunn/vim-easy-align' "{{{

xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)

"}}}

" vim-fugitive (git wrapper)
Plug 'tpope/vim-fugitive' "{{{

nnoremap <Leader>G :G<CR>

augroup fugitivecustom
    autocmd!
    autocmd FileType fugitive nnoremap <buffer> <Leader>p :term cd %:p:h && git pull<CR>i
    autocmd FileType fugitive nnoremap <buffer> <Leader>P :term cd %:p:h && git push<CR>i
    autocmd BufReadPre ~/.dotfiles.git/index let g:fugitive_git_executable = 'config'
augroup END

"}}}

" vim-gitgutter (show git diff in the gutter)
Plug 'airblade/vim-gitgutter' "{{{

let g:gitgutter_map_keys = 0

nmap [c <Plug>(GitGutterPrevHunk)
nmap ]c <Plug>(GitGutterNextHunk)

"}}}

" vim-glsl (language support for GLSL)
Plug 'tikhomirov/vim-glsl' "{{{}}}

" vim-jsx-pretty (language support for JSX)
Plug 'MaxMEllon/vim-jsx-pretty' "{{{}}}

" vim-pandoc (lots of tools for Pandoc)
Plug 'vim-pandoc/vim-pandoc' "{{{

let g:pandoc#modules#enabled = [
            \ "formatting",
            \ "folding",
            \ "yaml",
            \ "toc",
            \ "spell",
            \ "hypertext"
            \ ]

"}}}

" vim-pandoc-syntax (language support for Pandoc Markdown)
Plug 'vim-pandoc/vim-pandoc-syntax' "{{{

let g:pandoc#syntax#protect#codeblocks = 0

"}}}

" vim-racket (language support for Racket)
Plug 'wlangstroth/vim-racket' "{{{

set lispwords-=if

" }}}

" vim-repeat (better . command)
Plug 'tpope/vim-repeat' "{{{}}}

" vim-slime (send commands to a terminal)
Plug 'jpalardy/vim-slime' "{{{

let g:slime_target = "neovim"
let g:slime_vimterminal_config = {
\   "term_finish": "close"
\ }
let g:slime_no_mappings = 1

" let g:slime_python_ipython = 1

xmap <Leader><Leader><Leader> <Plug>SlimeRegionSend
nmap <Leader><Leader><Leader> <Plug>SlimeParagraphSend
nmap <Leader><Leader>m <Plug>SlimeMotionSend

"}}}

" vim-surround (surround text with stuff [parenteses, brackets, and much more])
Plug 'tpope/vim-surround' "{{{}}}

" vim-vinegar (improve NetRW)
Plug 'tpope/vim-vinegar' "{{{}}}

" vim-wordmotion (improve the `w` key and similar)
Plug 'chaoren/vim-wordmotion' "{{{

let g:wordmotion_spaces = ''

"}}}

" wolfram-vim (language support for Wolfram Language)
Plug 'arnoudbuzing/wolfram-vim' "{{{

" Setup wolfram.vim (WL syntax highlight)
autocmd BufNewFile,BufRead *.wl set ft=wl
autocmd BufNewFile,BufRead *.wls set ft=wl

"}}}

" yaml-vim (better language support for YAML)
Plug 'mrk21/yaml-vim' "{{{}}}

" vim-indent-object (text object for indented text)
Plug 'michaeljsmith/vim-indent-object' "{{{}}}

" vim-textobj-user (framework for creating text objects [used by other plugins])
Plug 'kana/vim-textobj-user' "{{{}}}

" vim-textobj-entire (text object for the entire buffer)
Plug 'kana/vim-textobj-entire' "{{{}}}

" vim-textobj-fold (text object for a fold)
Plug 'kana/vim-textobj-fold' "{{{}}}

" vim-textobj-syntax (text object for text in the same highlight group)
Plug 'kana/vim-textobj-syntax' "{{{}}}

" vim-eunuch (for adding nice commands for shell commands)
Plug 'tpope/vim-eunuch' "{{{}}}

" vim-python-pep8-indent (indent according to PEP8)
Plug 'Vimjas/vim-python-pep8-indent' "{{{}}}

" vim-haskell-indent (proper autoindent for Haskell)
Plug 'itchyny/vim-haskell-indent' "{{{
" consider using axelf4/vim-haskell? }}}

" vim-coiled-snake (automatic folding for Python)
Plug 'kalekundert/vim-coiled-snake' "{{{}}}

" FastFold (to accelerate folding with `expr`)
Plug 'Konfekt/FastFold' "{{{}}}

" vim-localvimrc (for using local [e.g. project-specific] vimrcs)
Plug 'embear/vim-localvimrc' "{{{

let g:localvimrc_persistent = 1
let g:localvimrc_persistence_file = "/home/daniel/.local/misc/localvimrc_persistent"

"}}}

" magma.nvim (Magma [see magma.vim above] reimplementation for NeoVim)
call s:PlugOwn('magma.nvim') " {{{

" map <Return> <Plug>MagmaEvaluateN
" vmap <Return> <Plug>MagmaEvaluateV

" }}}

" lightline.vim (a nice statusline)
Plug 'itchyny/lightline.vim' "{{{

function! GitSummaryStr()
    let [a,m,r] = GitGutterGetHunkSummary()
    if a == 0 && m == 0 && r == 0
        return FugitiveHead() . ""
    else
        return FugitiveHead() . "'"
    endif
    " return printf("+%d ~%d -%d", a, m, r)
endfunction

let g:lightline = {
            \ 'active': {
            \     'left': [ [ 'mode', 'paste' ],
            \               [ 'git' ],
            \               [ 'filename',
            \                 'readonly',
            \                 'modified',
            \               ]
            \             ],
            \     'right': [ [ 'lineinfo' ],
            \                [ 'percent' ],
            \                [ 'filetype' ],
            \                [ 'fileformat', 'fileencoding' ]
            \              ]
            \     },
            \ 'component_function': {
            \   'git': 'GitSummaryStr'
            \ }
            \ }

"}}}

" suda.vim (for editting files which require root permission)
Plug 'lambdalisue/suda.vim' "{{{

let g:suda_smart_edit = 1

"}}}

" vimtex (for editing LaTeX sanely)
Plug 'lervag/vimtex' "{{{

let g:tex_flavor = 'latex'
let g:vimtex_fold_enabled = v:true

"}}}

" vim-gutentags (for automatically running ctags when necessary)
Plug 'ludovicchabant/vim-gutentags' "{{{}}}

" python-syntax (for better Python syntax highlight)
Plug 'vim-python/python-syntax' " {{{

let g:python_version_2                          = 0
let g:python_highlight_builtins                 = 1
let g:python_highlight_builtin_objs             = 1
let g:python_highlight_builtin_types            = 1
let g:python_highlight_builtin_funcs            = 1
let g:python_highlight_builtin_funcs_kwarg      = 0
let g:python_highlight_exceptions               = 1
let g:python_highlight_string_formatting        = 1
let g:python_highlight_string_format            = 1
let g:python_highlight_string_templates         = 1
let g:python_highlight_indent_errors            = 0
let g:python_highlight_space_errors             = 0
let g:python_highlight_doctests                 = 1
let g:python_highlight_func_calls               = 0
let g:python_highlight_class_vars               = 1
let g:python_highlight_operators                = 1
let g:python_highlight_file_headers_as_comments = 1

" }}}

" vim-toml (for TOML syntax highlight)
Plug 'cespare/vim-toml' "{{{}}}

" vimspector (for debugging)
Plug 'puremourning/vimspector' "{{{}}}

" calendar.vim (for a way too fancy calendar)
Plug 'itchyny/calendar.vim' "{{{

let g:calendar_date_endian = "little"
let g:calendar_date_separator = "/"

let g:calendar_frame = "unicode"

let g:calendar_google_calendar = 1
let g:calendar_google_task     = 1
if filereadable("~/.cache/calendar.vim/credentials.vim")
    source ~/.cache/calendar.vim/credentials.vim
endif

"}}}

" vim-table-mode (for painlessly editing tables)
Plug 'dhruvasagar/vim-table-mode' "{{{
" TODO: change default mappings
"}}}

" vim-highlightedyank (for briefly highlighting yanked regions)
Plug 'machakann/vim-highlightedyank' "{{{}}}

" beacon.nvim (show large cursor jumps)
Plug 'danilamihailov/beacon.nvim' "{{{

let g:beacon_enable = 0
let g:beacon_ignore_filetypes = ['fzf']

nnoremap <silent> n n:Beacon<CR>
nnoremap <silent> N N:Beacon<CR>
nnoremap <silent> * *:Beacon<CR>
nnoremap <silent> # #:Beacon<CR>

"}}}

" vim-runit (for playing around with the code in your buffer with ease)
call s:PlugOwn('vim-runit') "{{{

nmap <Leader>r <Plug>(RunIt)
nmap <Leader>R <Plug>(ReplIt)

"}}}

" bullets.vim (for automatic bullet lists in markdown)
Plug 'dkarter/bullets.vim' "{{{

let g:bullets_enabled_file_types = [
            \ 'markdown',
            \ 'pandoc',
            \ 'gitcommit',
            \ 'toq',
            \ 'text',
            \ ]

let g:bullets_renumber_on_change = 0
let g:bullets_enable_in_empty_buffers = 0
let g:bullets_outline_levels = ['ROM', 'ABC', 'num', 'abc', 'rom', 'std-']

"}}}

" toq.vim (for my todo management)
call s:PlugOwn('toq.vim') "{{{

augroup toq
    autocmd!
    autocmd FileType toq setl sw=2
augroup END

"}}}

" vim-closetag (for automatically closing HTML tags)
Plug 'alvan/vim-closetag' "{{{}}}

" plantuml-syntax (for PlantUML syntax support)
Plug 'aklt/plantuml-syntax' "{{{}}}

" rust.vim (for better Rust syntax support)
Plug 'rust-lang/rust.vim' "{{{}}}

call plug#end() "}}}
"}}}
"}}}

" If running in GVim
"{{{
if has('gui_running') " If in GVim
    " Hide the noob tools (GVim)
    set guioptions-=m  " remove menubar
    set guioptions-=T  " remove toolbar
    set guioptions-=r  " remove right-hand scroll bar
    set guioptions-=L  " remove left-hand scroll bar
    " Set the font (it's still the default, but we need to set this in
    "   order to be able to change the font size)
    set guifont=Monospace\ 10
    " Disable scrolling
    map <ScrollWheelUp> <nop>
    map <ScrollWheelDown> <nop>
    map <S-ScrollWheelUp> <nop>
    map <S-ScrollWheelDown> <nop>
endif

" Add hotkeys for changing the font size
" {{{
" Gently taken/adapted from https://vim.fandom.com/wiki/Change_font_size_quickly

function! AdjustFontSize(amount)
    let s:pattern = '^\(.* \)\([1-9][0-9]*\)$'
    let s:minfontsize = 6
    let s:maxfontsize = 16
    if has("gui_gtk2") && has("gui_running")
        let fontname = substitute(&guifont, s:pattern, '\1', '')
        let cursize = substitute(&guifont, s:pattern, '\2', '')
        let newsize = cursize + a:amount
        if (newsize >= s:minfontsize) && (newsize <= s:maxfontsize)
            let newfont = fontname . newsize
            let &guifont = newfont
        endif

        redraw
    else
        echoerr "You need to run the GTK2 version of Vim to use this function."
    endif
endfunction

function! LargerFont()
    call AdjustFontSize(1)
endfunction
command! LargerFont call LargerFont()

function! SmallerFont()
    call AdjustFontSize(-1)
endfunction
command! SmallerFont call SmallerFont()

nnoremap <LocalLeader>+ :LargerFont<CR>
nnoremap <LocalLeader>- :SmallerFont<CR>

" TODO: add default font? Then use <LocalLeader>0 to go back to it.
"}}}
"}}}

" Lots of Vim options
"{{{
"" Enable the mouse
set mouse=a " enable all features using the mouse

"" Let only LightLine show the current mode
set noshowmode " don't show the current mode below the statusbar

"" Use FISH as the shell
" set shell=/usr/bin/fish
set shell=/bin/dash
" set shell=/bin/zsh

"" Setup line wrap
set wrap          " wrap lines
set linebreak     " don't break words on wrap
set breakindent   " indent wrapped lines
set showbreak=…\  " prefix for wrapped lines

"" Set start of line mode
set startofline

"" Set case sensitiveness of the search
set ignorecase smartcase " if everything is lowercase, case insensitive.
                         "   Otherwise, it's case sensitive.

"" Show line numbers
" set number " show absolute line numbers

"" Show the foldcolumn
" set foldcolumn=1 " have only one column in the fold column
set foldcolumn=auto:9 " automatically manage foldcolumn width (NeoVim only)

"" Set the foldmethod
" set foldmethod=manual " set folding to manual

"" Use 4 spaces instead of tabs
set expandtab     " expand tabs
set shiftwidth=4  " how many spaces to use for >> and <<
set softtabstop=4 " how many spaces to use for tab

"" Keep indentation structure
set autoindent " use same indentation level for the neighboring lines

"" Highlight past the textwidth
set colorcolumn=+1 " highlight the column after the one specified in 'textwidth'

"" Raise the updatetime (both for .swp and for plugins such as vim-latex-live-preview)
""set updatetime=100
""set updatetime=500
set updatetime=250 " how much time between updates, in milliseconds

"" Remove the bell sound
set belloff=all " disable the bell everywhere

"" Fix the backspace
set backspace=2 " when on insert mode, backspace will remove indentation and newlines

"" Open new vertical splits to the right and horizontal splits below
set splitright " open new vertical splits to the right
set splitbelow " open new horizontal splits below

"" Setup undo persistence
" set undodir=~/.vim/undodir " where to save undos
set undofile               " save undos to the file

"" Allow hidden buffers
set hidden " hide buffers when leaving them, instead of deleting them

"" Remove options from views
set viewoptions-=options " remove options local to a window/buffer from mkview views

"" Open new buffers in a split instead of replacing the current one
set switchbuf=split " split instead of replacing current window on new buffers

"" Highlight search results
set hlsearch " highlight search results

"" Preview search/substitute pattern matches
set incsearch          " live preview search results
set inccommand=nosplit " live preview command (:s, :g, etc.) results

"" Show possible tab completions above
set wildmenu " show completion menu in Ex commands

"" Never conceal the current line
set concealcursor= " don't conceal the current line in any mode

"" Show trailing whitespace and tabs
set list                       " show listchars
set listchars=tab:\ \ ,trail:┈ " show tabs as spaces and highlight trailing whitespace

"" Setup autocompletion in a way that is better
set completeopt=          " clear autocompletion options
set completeopt+=menu     " Show additional information in a popup menu
" set completeopt+=noinsert " Don't insert text automatically

"" Always keep 2 lines around the cursor
set scrolloff=2 " keep 2 lines above & below the cursor at all times

"" Save tabs in sessions
set sessionoptions+=tabpages,globals " save tabpages and global vars in sessions

"" Always show the tabline
" set showtabline=2 " show tab line even if only with one tab open

"" Accelerate Esc presses
set ttimeout        " Enable timeout
set ttimeoutlen=50  " Timeout for keycodes
set timeoutlen=3000 " Timeout for mappings

"" Setup pseudo-transparency for popup&floating windows
set pumblend=25 " Pseudo-transparency for popup menus
set winblend=25 " Pseudo-transparency for floating windows

"}}}

" Setup the statusline
"{{{
set laststatus=2
function! LinterStatus() abort "{{{
    if g:ale_enabled == 1
        let l:counts = ale#statusline#Count(bufnr(''))

        let l:errors = l:counts.error
        let l:style_errors = l:counts.style_error
        let l:all_non_errors = l:counts.total - l:errors - l:style_errors

        return l:counts.total == 0 ? 'OK' : printf(
        \   '%dW %dS %dE',
        \   all_non_errors,
        \   style_errors,
        \   errors
        \)
    else
        return 'OFF'
    end
endfunction "}}}

function! CheckFTExists() "{{{
    if getbufvar(bufnr('%'), "current_syntax") == "" && &ft != ""
        return "*"
    else
        return " "
    end
endfunction "}}}

let g:do_clock = 0
function! PossiblyClock() "{{{
    if g:do_clock
        return strftime('%H:%M:%S')
    else
        return ""
    endif
endfunction "}}}

function! MagmaStatus() "{{{
    return ""
    let l:kernel_state = MagmaState()
    if l:kernel_state == 0
        return "[idle]"
    elseif l:kernel_state == 1
        return "[nonidle]"
    elseif l:kernel_state == 2
        return "[busy]"
    elseif l:kernel_state == 3
        return ""
    elseif l:kernel_state == 4
        return "[dead]"
    endif
endfunction "}}}

" Configure the statusline
set statusline=
"set statusline+=\ %y
set statusline+=\ %F
set statusline+=\ %m
set statusline+=\ %{fugitive#statusline()}
set statusline+=%=
set statusline+=%{MagmaStatus()}
set statusline+=\ \ %{LinterStatus()}
set statusline+=\ \ %{toupper(&ft)}
set statusline+=%{CheckFTExists()}
set statusline+=\ %{PossiblyClock()}
set statusline+=\ \|
set statusline+=\ %3p
set statusline+=%%
set statusline+=\ %5c
set statusline+=\ %5l
set statusline+=/
set statusline+=%L
" set statusline+=%5{GetTODOCount()}
"}}}

" augroup cursorChange
"     autocmd!
"     autocmd VimEnter * silent !echo -ne "\e[2 q"
" augroup END

" Apply the color scheme
set termguicolors " use truecolor in a terminal
colorscheme csillag

" Make jk and more go display linewise, not file linewise
"{{{
nnoremap <silent><expr> k v:count == 0 ? 'gk' : "\<Esc>".v:count.'k'
vnoremap <silent><expr> k v:count == 0 ? 'gk' : "\<Esc>".v:count.'k'
nnoremap <silent><expr> j v:count == 0 ? 'gj' : "\<Esc>".v:count.'j'
vnoremap <silent><expr> j v:count == 0 ? 'gj' : "\<Esc>".v:count.'j'
nnoremap <silent><expr> - v:count == 0 ? 'gkg^' : "\<Esc>".v:count.'-'
vnoremap <silent><expr> - v:count == 0 ? 'gkg^' : "\<Esc>".v:count.'-'
onoremap <silent><expr> - v:count == 0 ? 'gkg^' : "\<Esc>".v:count.'-'
nnoremap <silent><expr> + v:count == 0 ? 'gjg^' : "\<Esc>".v:count.'+'
vnoremap <silent><expr> + v:count == 0 ? 'gjg^' : "\<Esc>".v:count.'+'
onoremap <silent><expr> + v:count == 0 ? 'gjg^' : "\<Esc>".v:count.'+'
nnoremap <silent> 0 g0
vnoremap <silent> 0 g0
onoremap <silent> 0 g0
nnoremap <silent> $ g$
vnoremap <silent> $ g$
onoremap <silent> $ g$
nnoremap <silent> _ g^
vnoremap <silent> _ g^
onoremap <silent> _ g^
"}}}

" Swap ' and `
nnoremap ' `
nnoremap ` '

" Make Y work like D
nnoremap Y y$

" Abbreviate :w to :up
cabbrev w up

" Unmap Q
nnoremap Q <nop>

" Bind <C-w>n to go back to normal mode from terminal mode
tnoremap <C-w>n <C-\><C-N>

" Add wrapper/helper for using figlet for inserting big text
command! -nargs=1 BigText execute "read! figlet -f standard -S -k -t " . shellescape("<args>", v:true) . " | sed 's/\\s\\+$//' | grep -v '^$'"

" Neater folds
function! NeatFoldText() "{{{
    " let line = ' ' . substitute(getline(v:foldstart), '^\s*"\?\s*\|\s*"\?\s*{{' . '{\d*\s*', '', 'g') . ' '
    let line = getline(v:foldstart) . '  '
    let lines_count = v:foldend - v:foldstart + 1
    let lines_count_text = '| ' . printf("(%d) %10s", v:foldlevel, lines_count . ' lines') . ' |'
    let foldchar = matchstr(&fillchars, 'fold:\zs.')
    " let foldtextstart = strpart(' ' . repeat(foldchar, v:foldlevel*2) . line, 0, (winwidth(0)*2)/3)
    let foldtextstart = strpart('' . line, 0, (winwidth(0)*2)/3)
    let foldtextend = lines_count_text . repeat(foldchar, 8)
    let foldtextlength = strlen(substitute(foldtextstart . foldtextend, '.', 'x', 'g')) + &foldcolumn
    return foldtextstart . repeat(foldchar, winwidth(0)-foldtextlength) . foldtextend
endfunction
set fillchars+=fold:\ ,vert:\│
set foldtext=NeatFoldText() "}}}

" Create and load views automatically
augroup autoview "{{{
    autocmd!
    autocmd BufWinLeave,VimLeave,BufWritePost *
                \ if expand("%") != ""
                \ | silent! mkview
                \ | call lightline#update()
                \ | endif
    autocmd BufWinEnter,BufReadPost *
                \ if expand("%") != ""
                \ | silent! loadview
                \ | call lightline#update()
                \ | endif
augroup END "}}}

" Share data between NeoVim instances (marks, registers, etc.)
augroup SHADA "{{{
    autocmd!
    autocmd FocusGained *
                \ if exists(':rshada') | rshada | endif
    autocmd FocusLost *
                \ if exists(':wshada') | wshada | endif
augroup END "}}}

" Set hotkeys for changing between Magma and Slime
nnoremap <Leader><Leader>s :map <Leader><Leader><Leader> <Plug>SlimeParagraphSend<CR>
nnoremap <Leader><Leader>m :nnoremap <Leader><Leader><Leader> :MagmaEvaluate<lt>CR><CR>

" Set a hotkey for `:set spell!`
nnoremap <Leader>S :set spell!<CR>

" Set a hotkey for `:set spelllang=`
nnoremap <Leader>s :set spelllang=

" Set a hotkey for `gt` and `gT` (including in terminal mode!)
" nnoremap <C-Tab>   gt
" nnoremap <C-S-Tab> gT
" tnoremap <C-Tab>   <C-w>gt
" tnoremap <C-S-Tab> <C-w>gT

" Set a hotkey for `:Beacon`
nnoremap <Leader>; :Beacon<CR>

" Set a hotkey for `:nohl`
nnoremap <Leader>. :nohl<CR>

" Automatically redraw Vim if it is resized
autocmd VimResized * redraw

" " Automatically recompile Pandoc markdown
" autocmd CursorHold *.md
"             \ if exists(':Pandoc')
"             \ |   Pandoc pdf
"             \ | endif

" autocmd BufWritePost *.md Make!
autocmd BufWritePost *.md AsyncStop | sleep 100m | AsyncRun make
autocmd BufWritePost *.mmd AsyncStop | sleep 100m | AsyncRun mmdc -i % -o %.png
autocmd BufWritePost *.uml AsyncStop | sleep 100m | AsyncRun plantuml %

autocmd FileType *.py setlocal fdm=expr
autocmd FileType *.coco setlocal fdm=expr foldexpr=coiledsnake#FoldExpr(v:lnum)
autocmd FileType *.rs setlocal fdm=syntax

" Automatically set the b:git_dir and g:gitgutter_git_executable for dotfiles
let s:dotfiles = split(system('config ls-tree --full-tree -r --name-only HEAD'), '\n')
augroup dotfiles
    autocmd!
    for dotfile in s:dotfiles
        execute 'autocmd BufReadPost ' . getenv('HOME') . '/' . dotfile . ' let b:git_dir="' . getenv('HOME') . '/.dotfiles.git"'
        execute 'autocmd BufReadPost ' . getenv('HOME') . '/' . dotfile . ' let g:gitgutter_git_executable="config"'
    endfor
augroup END

"  VIM: let b:countTODOs_offset=-3
"  vim: fdm=marker
