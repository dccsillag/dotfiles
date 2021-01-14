"  _         _  _            _
" (_) _ __  (_)| |_  __   __(_) _ __ ___
" | || '_ \ | || __| \ \ / /| || '_ ` _ \
" | || | | || || |_  _\ V / | || | | | | |
" |_||_| |_||_| \__|(_)\_/  |_||_| |_| |_|
"
" @author Daniel Csillag (aka. dccsillag)
" @what My NeoVim configuration.

if filereadable(expand("~/.config/nvim/preinit.vim"))
    source ~/.config/nvim/preinit.vim
endif

" Plugins {{{
" A function to plug my own plugins, which if available should be used from
" the local machine.
function! s:PlugOwn(plugin_name) "{{{
    if isdirectory(expand('~/code/' . a:plugin_name))
        execute "Plug '" . '~/code/' . a:plugin_name . "/'"
    else
        execute "Plug 'dccsillag/" . a:plugin_name . "'"
    endif
endfunction "}}}

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


if has("nvim")
    call plug#begin(stdpath('data') . '/plugged')
else
    call plug#begin('~/.vim/plugged')
endif

" Interface {{{
Plug 'junegunn/fzf' " (fuzzy finder)
Plug 'junegunn/fzf.vim' " ('official' fzf addons)
" Plug 'Konfekt/FastFold' " (to accelerate folding with `expr`)
Plug 'machakann/vim-highlightedyank' " (for briefly highlighting yanked regions)
Plug 'danilamihailov/beacon.nvim' " (show large cursor jumps) {{{

let g:beacon_enable = 0
let g:beacon_ignore_filetypes = ['fzf']

"}}}
Plug 'airblade/vim-gitgutter' " (show git diff in the gutter) {{{

let g:gitgutter_map_keys = 0

"}}}
Plug 'junegunn/limelight.vim' " (a spotlight for code, good for presenting bit-by-bit) {{{

let g:limelight_conceal_ctermfg = 242
let g:limelight_conceal_guifg   = '#606060'

"}}}
Plug 'junegunn/goyo.vim' " (make things pretty, for more elegant presentations)
Plug 'dstein64/vim-startuptime' " (profile startup time neatly)
"}}}

" Behaviour {{{
Plug 'tpope/vim-repeat' " (better . command)
Plug 'chaoren/vim-wordmotion' " (improve the `w` key and similar) {{{

let g:wordmotion_spaces = ''

"}}}
Plug 'haya14busa/vim-asterisk' " (improve * and #)
" Plug 'ervandew/supertab' " (for autocomplete with tab)
Plug 'embear/vim-localvimrc' " (for using local [e.g. project-specific] vimrcs) {{{

let g:localvimrc_persistent = 1
let g:localvimrc_persistence_file = expand("~/.local/misc/localvimrc_persistent")

"}}}
Plug 'vim-scripts/let-modeline.vim' " (have a specific modeline for configuring plugins)
Plug 'lambdalisue/suda.vim' " (for editting files which require root permission) {{{

let g:suda_smart_edit = 1

"}}}
Plug 'tpope/vim-vinegar' " (improve NetRW)
Plug 'KabbAmine/vCoolor.vim' " (color selector) {{{

let g:vcoolor_disable_mappings = 1

"}}}
"}}}

" Peripherals {{{
Plug 'tpope/vim-eunuch' " (for adding nice commands for shell commands)
Plug 'skywind3000/asyncrun.vim' " (for running stuff in the background, async)
call s:PlugOwn('debug.vim') " (for debugging)
call s:PlugOwn('vim-runit') " (for playing around with the code in your buffer with ease)
Plug 'itspriddle/vim-shellcheck' " (for running shellcheck from Vim, without using ALE)
call s:PlugOwn('magma.nvim') " (Jupyter client)
Plug 'ludovicchabant/vim-gutentags' " (for automatically running ctags when necessary)
Plug 'tpope/vim-fugitive' " (use git from vim)
"}}}

" Color Schemes {{{
call s:PlugOwn('csillag-color') " (my colorscheme)
Plug 'joshdick/onedark.vim' " (OneDark colorscheme from Atom)
Plug 'pbrisbin/vim-colors-off' " (a plain colorscheme that pretty much disables highlighting)
"}}}

" Editing Help {{{
Plug 'tpope/vim-surround' " (surround text with stuff [parenteses, brackets, and much more])
Plug 'tomtom/tcomment_vim' " (comment/uncomment code)
Plug 'cohama/lexima.vim'
Plug 'alvan/vim-closetag' " (for automatically closing HTML tags)
" Plug 'dkarter/bullets.vim' " (for automatic bullet lists) {{{

let g:bullets_enabled_file_types = [
            \ 'markdown',
            \ 'gitcommit',
            \ 'toq',
            \ 'text',
            \ ]

let g:bullets_renumber_on_change = 0
let g:bullets_enable_in_empty_buffers = 0
let g:bullets_outline_levels = ['ROM', 'ABC', 'num', 'abc', 'rom', 'std-']

"}}}
Plug 'junegunn/vim-easy-align' " (align code)
Plug 'AndrewRadev/splitjoin.vim' " (for changing code between inline and multiline forms)
Plug 'dhruvasagar/vim-table-mode' " (for painlessly editing tables) {{{

let g:table_mode_map_prefix = "<Leader><Bar>"
let g:table_mode_toggle_map = "<Bar>"

"}}}
Plug 'tommcdo/vim-exchange' " (for exchanging text around)
Plug 'svermeulen/vim-subversive' " (for replacing text with current yank)
Plug 'godlygeek/tabular' " (also for aligning text, required by vim-markdown)
"}}}

" Text Objects {{{
Plug 'wellle/targets.vim' " (better text objects)
Plug 'michaeljsmith/vim-indent-object' " (text object for indented text)
Plug 'kana/vim-textobj-entire' " (text object for the entire buffer)
Plug 'kana/vim-textobj-fold' " (text object for a fold)
Plug 'kana/vim-textobj-syntax' " (text object for text in the same highlight group)
Plug 'kana/vim-textobj-user' " (framework for creating text objects [used by other plugins])
"}}}

" Language Support {{{
Plug 'vim-python/python-syntax' " (for better Python syntax highlight) {{{

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
Plug 'Vimjas/vim-python-pep8-indent' " (indent according to PEP8)
Plug 'kalekundert/vim-coiled-snake' " (automatic folding for Python) {{{

let g:coiled_snake_set_foldtext = 0

"}}}
Plug 'bfrg/vim-cpp-modern' " (better C++ syntax highlight) {{{

let g:cpp_no_function_highlight        = 0
let g:cpp_named_requirements_highlight = 1

"}}}
Plug 'plasticboy/vim-markdown' " (better Markdown support) {{{

let g:vim_markdown_folding_style_pythonic = 1
let g:vim_markdown_override_foldtext = 0
let g:vim_markdown_no_default_key_mappings = 1
" let g:vim_markdown_emphasis_multiline = 0
let g:vim_markdown_fenced_languages = ['c++=cpp', 'scm=scheme', 'py=python']

let g:vim_markdown_math = 1
let g:vim_markdown_frontmatter = 1
let g:vim_markdown_strikethrough = 1

"}}}
Plug 'rubik/vim-dg' " (language support for DogeLang [aka. dg])
Plug 'manicmaniac/coconut.vim' " (language support for Coconut)
Plug 'rust-lang/rust.vim' " (for better Rust syntax support)
Plug 'itchyny/vim-haskell-indent' " (proper autoindent for Haskell)
" XXX: Consider using axelf4/vim-haskell?
Plug 'mrk21/yaml-vim' " (better language support for YAML)
Plug 'cespare/vim-toml' " (for TOML syntax highlight)
call s:PlugOwn('toq.vim') " (for my todo management) {{{

augroup toq
    autocmd!
    autocmd FileType toq setl sw=2
augroup END

"}}}
Plug 'MaxMEllon/vim-jsx-pretty' " (language support for JSX)
Plug 'dart-lang/dart-vim-plugin' " (language support for Dart)
Plug 'tikhomirov/vim-glsl' " (language support for GLSL)
Plug 'aklt/plantuml-syntax' " (for PlantUML syntax support)
Plug 'arnoudbuzing/wolfram-vim' " (language support for Wolfram Language) {{{

" Setup wolfram.vim (WL syntax highlight)
autocmd BufNewFile,BufRead *.wl set ft=wl
autocmd BufNewFile,BufRead *.wls set ft=wl

"}}}
Plug 'wlangstroth/vim-racket' " (language support for Racket) {{{

set lispwords-=if

" }}}
Plug 'goerz/jupytext.vim' " (for editing Jupyter notebooks)
"}}}

call plug#end()

"}}}

" If running in GVim {{{
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

" Add mappings for changing the font size
" {{{
" Gently taken/adapted from https://vim.fandom.com/wiki/Change_font_size_quickly

function! AdjustFontSize(amount) "{{{
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
endfunction "}}}

function! LargerFont() "{{{
    call AdjustFontSize(1)
endfunction
command! LargerFont call LargerFont() "}}}

function! SmallerFont() "{{{
    call AdjustFontSize(-1)
endfunction
command! SmallerFont call SmallerFont() "}}}

nnoremap <LocalLeader>+ :LargerFont<CR>
nnoremap <LocalLeader>- :SmallerFont<CR>

" TODO: add default font? Then use <LocalLeader>0 to go back to it.
"}}}
"}}}

" Lots of Vim options {{{
"" Enable the mouse
set mouse=a " enable all features using the mouse

"" Setup statusbar
set noshowmode                                        " don't show the current
                                                      "   mode below the
                                                      "   statusbar
set laststatus=0                                      " only show the
                                                      "   statusline when in
                                                      "   between two windows
set statusline=%{repeat('―',\ nvim_win_get_width(0))} " set the statusline to
                                                      "   a horizontal
                                                      "   separator
set noruler                                           " remove the ruler

"" Use FISH as the shell
" set shell=/usr/bin/fish
set shell=/bin/dash
" set shell=/bin/zsh

"" Setup :grep
set grepprg=grep\ -R\ --exclude-dir=.git\ -n\ $*\ .\ /dev/null

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

"" Show the foldcolumn
" set foldcolumn=1 " have only one column in the fold column
set foldcolumn=auto:5 " automatically manage foldcolumn width (NeoVim only)

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
set viewoptions-=curdir  " remove current directory from mkview views

"" Highlight search results
set hlsearch " highlight search results

"" Preview search/substitute pattern matches
set incsearch          " live preview search results
set inccommand=nosplit " live preview command (:s, :g, etc.) results

"" Show possible tab completions above
set wildmenu " show completion menu in Ex commands

"" Setup conceal
set concealcursor= " don't conceal the current line in any mode
set conceallevel=2 " conceal everything, hiding concealed text completely

"" Show trailing whitespace and tabs
set list                       " show listchars
set listchars=tab:\ \ ,trail:┈ " show tabs as spaces and highlight trailing whitespace

"" Setup autocompletion in a way that is better
set completeopt=          " clear autocompletion options
set completeopt+=menu     " show additional information in a popup menu
set complete=.,i,d,t      " look for completions in the current buffer (.),
                          "                      in the included files (i,d),
                          "                      and in the tags (t)

"" Always keep 2 lines around the cursor
set scrolloff=2 " keep 2 lines above & below the cursor at all times

"" Save tabs in sessions
set sessionoptions+=tabpages,globals " save tabpages and global vars in sessions

"" Accelerate Esc presses
set ttimeout        " Enable timeout
set ttimeoutlen=50  " Timeout for keycodes
set timeoutlen=3000 " Timeout for mappings

"" Setup pseudo-transparency for popup&floating windows
set pumblend=25 " Pseudo-transparency for popup menus
set winblend=25 " Pseudo-transparency for floating windows

"" Enable truecolors
set termguicolors " Use truecolor in a terminal

"}}}

"" Apply the color scheme
colorscheme csillag

" Remappings {{{

"" Make jk and more go display linewise, not file linewise 
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


"" Swap ' and `
nnoremap ' `
nnoremap ` '

"" Make Y work like D
nnoremap Y y$

"" Abbreviate :w to :up
cnoreabbrev w up

"" Unmap Q
nnoremap Q <nop>

"" Bind <C-w>n to go back to normal mode from terminal mode
tnoremap <C-w>n <C-\><C-N>

"" Add Beacon to search jump commands
nnoremap <silent> n n:Beacon<CR>
nnoremap <silent> N N:Beacon<CR>
nnoremap <silent> * *:Beacon<CR>
nnoremap <silent> # #:Beacon<CR>

"" Change S to behave like X
nmap S Xi

"" Change s to ys (normal) / S (visual)
nmap s ys
vmap s S

"}}}

" Misc {{{

"" Add wrapper/helper for using figlet for inserting big text
command! -nargs=1 BigText execute "read! figlet -f standard -S -k -t " . shellescape("<args>", v:true) . " | sed 's/\\s\\+$//' | grep -v '^$'"

"" Neater folds
function! NeatFoldText() "{{{
    let line = getline(v:foldstart)
    let lines_count = v:foldend - v:foldstart + 1
    let lines_count_text = '| ' . printf("(%d) %10s", v:foldlevel, lines_count . ' lines') . ' |'
    let foldchar = matchstr(&fillchars, 'fold:\zs.')
    let foldtextstart = strpart('' . line, 0, (winwidth(0)*2)/3)
    let foldtextend = lines_count_text . repeat(foldchar, 8)
    let foldtextlength = strlen(substitute(foldtextstart . foldtextend, '.', 'x', 'g')) + &foldcolumn
    return foldtextstart . ' ' . repeat(foldchar, winwidth(0)-foldtextlength-1) . foldtextend
endfunction
set fillchars+=fold:―,vert:\│
set foldtext=NeatFoldText() "}}}

" }}}

" Autocommands {{{

"" Create and load views automatically
augroup autoview "{{{
    autocmd!
    autocmd BufWinLeave,VimLeave,BufWritePost *
                \ if expand("%") != ""
                \ | silent! mkview
                \ | endif
    autocmd BufWinEnter,BufReadPost *
                \ if expand("%") != ""
                \ | silent! loadview
                \ | endif
augroup END "}}}

"" Share data between NeoVim instances (marks, registers, etc.)
augroup SHADA "{{{
    autocmd!
    autocmd FocusGained *
                \ if exists(':rshada') | rshada | endif
    autocmd FocusLost *
                \ if exists(':wshada') | wshada | endif
augroup END "}}}

"" Automatically enable the spellchecker on markup documents
augroup SpellCheck "{{{
    autocmd!
    autocmd FileType markdown  set spell
    autocmd FileType tex       set spell
    autocmd FileType html      set spell
    autocmd FileType gitcommit set spell
augroup END "}}}

"" Automatic compilation of markup files
augroup AutoCompile "{{{
    autocmd!
    autocmd BufWritePost *.tex AsyncStop | sleep 100m | AsyncRun cd %:h && latexmk -pdf -f %:t
    autocmd BufWritePost *.md  AsyncStop | sleep 100m | AsyncRun make
    autocmd BufWritePost *.mmd AsyncStop | sleep 100m | AsyncRun mmdc -i % -o %.png
    autocmd BufWritePost *.uml AsyncStop | sleep 100m | AsyncRun plantuml %
augroup END "}}}

" Automatically set the b:git_dir and g:gitgutter_git_executable for dotfiles
let s:dotfiles = split(system('config ls-tree --full-tree -r --name-only HEAD'), '\n') "{{{
augroup dotfiles
    autocmd!
    for dotfile in s:dotfiles
        execute 'autocmd BufReadPost ' . getenv('HOME') . '/' . dotfile . ' let b:git_dir="' . getenv('HOME') . '/.dotfiles.git"'
        execute 'autocmd BufReadPost ' . getenv('HOME') . '/' . dotfile . ' let g:gitgutter_git_executable="config"'
    endfor
    autocmd BufReadPre ~/.dotfiles.git/index let g:fugitive_git_executable = 'config'
augroup END "}}}

function! s:MarkdownIndent() abort "{{{
    if !exists('g:markdown_sourced_indent')
        let g:markdown_sourced_indent = "markdown"
    endif

    " FIXME: don't change cursor position, window position or folding
    "        (probably better done by recovering what they were before)

    let l:highlight_name = synIDattr(synID(line('.'), 1, 0), 'name')
    let l:is_code_block = (l:highlight_name =~ '^mkd\%(Code$\|Snippet\)' || l:highlight_name != '' && l:highlight_name !~ '^\%(mkd\|html\)')

    if strlen(l:highlight_name) == 0
        return
    endif

    if l:is_code_block
        let l:language_to_source = matchstr(l:highlight_name, '\C^[a-z]\+')
        if len(l:language_to_source) == 0
            return
        endif
        if l:language_to_source == 'mkd'
            let l:language_to_source = tolower(matchstr(l:highlight_name, '\C^mkd\(Snippet\|Code\)\zs[A-Z]\+\ze$'))
        endif

        if g:markdown_sourced_indent != l:language_to_source
            let g:markdown_sourced_indent = l:language_to_source
            setlocal indentexpr=
            if exists('b:undo_ftplugin') | execute b:undo_ftplugin | endif
            if exists('b:undo_indent')   | execute b:undo_indent   | endif
            unlet! b:did_indent
            unlet! b:did_ftplugin
            echo l:language_to_source . "..."
            execute 'runtime! indent/' . l:language_to_source . '.vim'
            execute 'runtime! ftplugin/' . l:language_to_source . '.vim'

            let l:changed = 1
        else
            let l:changed = 0
        endif
    else
        if g:markdown_sourced_indent != "markdown"
            let g:markdown_sourced_indent = "markdown"
            setlocal indentexpr=
            if exists('b:undo_ftplugin') | execute b:undo_ftplugin | endif
            if exists('b:undo_indent')   | execute b:undo_indent   | endif
            unlet! b:did_indent
            unlet! b:did_ftplugin
            echo "markdown..."
            runtime! indent/markdown.vim
            runtime! ftplugin/markdown.vim

            let l:changed = 1
        else
            let l:changed = 0
        endif
    endif

    if l:changed
        setl fdl=1
        setl fml=1
        setl fml=1
        setl fen
    endif
endfunction "}}}
augroup MarkdownIndent "{{{
    autocmd!
    " autocmd CursorMoved  *.md call s:MarkdownIndent()
    " autocmd CursorMovedI *.md call s:MarkdownIndent()
augroup END "}}}

"}}}

" Mappings {{{

"" Spellcheck related
nnoremap <Leader>S :set spell!<CR>
nnoremap <Leader>s :set spelllang=

" fzf.vim
nnoremap <Leader>e  :Files<CR>
nnoremap <Leader>T  :BTags<CR>
nnoremap <Leader>t  :Tags<CR>
nnoremap <Leader>b  :Buffers<CR>
nnoremap <Leader>ge :GFiles<CR>
nnoremap <Leader>gc :Commits<CR>
nnoremap <Leader>gC :BCommits<CR>
nnoremap <Leader>H  :Helptags<CR>
nnoremap <Leader>F  :Filetypes<CR>

" Open the compiled PDF for the current [markup] file
function! s:OpenCorrespondingPDF() abort
    if &filetype == "tex"
        let l:filename = expand('%:h') . '/.latex-build/' . expand('%:t:r') . '.pdf'
    else
        let l:filename = expand('%:r') . '.pdf'
    endif

    if !filereadable(l:filename)
        echoerr "No such file: " . l:filename
        return
    endif
    silent exec '!nohup zathura ' . l:filename . ' > /dev/null 2>&1 &'
endfunction
nnoremap <Leader>p :call <SID>OpenCorrespondingPDF()<CR>

" Run shellcheck on the current file
nnoremap <Leader>cs :ShellCheck!<CR>

" Fugitive
nnoremap <Leader>G :G<CR>
augroup FugitiveMappings
    autocmd!
    autocmd FileType fugitive nnoremap <buffer> zp :G pull<CR>
    autocmd FileType fugitive nnoremap <buffer> zP :G push<CR>
augroup END

" Jump along git hunks
nmap [c <Plug>(GitGutterPrevHunk)
nmap ]c <Plug>(GitGutterNextHunk)

" Runit
nmap <Leader>r <Plug>(RunIt)
nmap <Leader>R <Plug>(ReplIt)

" Insert colors (via color picker)
nnoremap <Leader>irgb :VCoolIns r<CR>
nnoremap <Leader>ihsl :VCoolIns h<CR>
nnoremap <Leader>ihex :VCoolor<CR>

" EasyAlign mappings
xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)

"" Beacon
nnoremap <Leader>; :Beacon<CR>

"" :nohl
nnoremap <Leader>. :nohl<CR>

"" vim-asterisk mappings
map *   <Plug>(asterisk-*)
map #   <Plug>(asterisk-#)
map g*  <Plug>(asterisk-g*)
map g#  <Plug>(asterisk-g#)
map z*  <Plug>(asterisk-z*)
map gz* <Plug>(asterisk-gz*)
map z#  <Plug>(asterisk-z#)
map gz# <Plug>(asterisk-gz#)

"" Add empty lines above and below
function! s:BlankUp(count) abort
    put! =repeat(nr2char(10), a:count)
    ']+1
    exec 'norm! ' . a:count . 'k'
    silent! call repeat#set("\<Plug>unimpairedBlankUp", a:count)
endfunction
function! s:BlankDown(count) abort
    put =repeat(nr2char(10), a:count)
    silent! call repeat#set("\<Plug>unimpairedBlankDown", a:count)
endfunction
nnoremap <silent> O :<C-U>call <SID>BlankUp(v:count1)<CR>
nnoremap <silent> o :<C-U>call <SID>BlankDown(v:count1)<CR>

"" vim-subversive mappings
nmap <C-s> <Plug>(SubversiveSubstitute)
nmap <C-s><C-s> <Plug>(SubversiveSubstituteLine)

"" lexima.vim rules

for filetype in ['tex', 'markdown']
    call lexima#add_rule({ 'char': '$', 'input_after': '$', 'filetype': filetype })
    call lexima#add_rule({ 'char': '$', 'at': '\%#\$', 'leave': 1, 'filetype': filetype })
    call lexima#add_rule({ 'char': '<BS>', 'at': '\$\%#\$', 'delete': 1, 'filetype': filetype })
endfor

"" Keymap for opening a LaTeX environment
function! s:OpenLatexEnvironment() abort
    let l:env_name = input("env> ")
    exec 'normal! a\begin{' . l:env_name . '}'
    normal! o
    exec 'normal! a\end{' . l:env_name . '}'
    normal! Oa
    normal! x$
endfunction
inoremap <C-l> <C-\><C-o>:call <SID>OpenLatexEnvironment()<CR>

" }}}

if filereadable(expand("~/.config/nvim/posinit.vim"))
    source ~/.config/nvim/posinit.vim
endif

"  VIM: let b:countTODOs_offset=-3
"  vim: fdm=marker
