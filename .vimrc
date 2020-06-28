"           ██           ▄▄▄▄▄▄     ▄▄▄▄
"           ▀▀           ██▀▀▀▀██ ██▀▀▀▀█
" ██▄  ▄██████   ████▄██▄██    ████▀
"  ██  ██   ██   ██ ██ █████████ ██
"  ▀█▄▄█▀   ██   ██ ██ ████  ▀██▄██▄
"   ████ ▄▄▄██▄▄▄██ ██ ████    ██ ██▄▄▄▄█
"    ▀▀  ▀▀▀▀▀▀▀▀▀▀ ▀▀ ▀▀▀▀    ▀▀▀  ▀▀▀▀
"
" @author Daniel Csillag

" Plugins
"{{{
" Check if vim-plug is installed. If it isn't, than install it (XXX: only
"    works on linux):
if empty(glob('~/.vim/autoload/plug.vim'))
    echom "Installing vim-plug..."
    silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
                \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    autocmd VimEnter * PlugInstall
endif

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

" enable termdebug (for debugging with gdb)
packadd termdebug "{{{}}}

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


call plug#begin('~/.vim/plugged') "{{{

" My colorscheme
call s:PlugOwn('csillag-color')

" ALE (linting / ~~LSP~~)
Plug 'dense-analysis/ale' "{{{

" let g:ale_enabled = 0

let g:ale_cache_executable_check_failures = 1
" \   'haskell': ['stack-ghc'],
" \   'haskell': ['hlint'],
" \   'haskell': ['stack-build'],
" \   'haskell': ['stack-ghc-mod', 'hlint'],
" \   'haskell': ['stack-build', 'stack-ghc', 'hlint'],
" \   'haskell': ['hdevtools', 'hlint'],
" \   'cpp':     ['gcc'],
" \   'python':  ['flake8', 'pyls'],
let g:ale_linters = {
\   'haskell': ['stack-build', 'hlint'],
\   'cs':      ['omnisharp'],
\   'cpp':     ['clangtidy', 'cppcheck'],
\   'python':  ['pylint', 'pyls'],
\   'lua':     ['luacheck'],
\   'yaml':    ['yamllint']
\}

let g:ale_python_flake8_executable = "/usr/bin/flake8"
let g:ale_python_flake8_use_global = 1
let g:ale_python_mypy_executable   = "/home/daniel/.local/bin/mypy"
let g:ale_python_mypy_use_global = 1

" let g:ale_cpp_gcc_options    = '-Wall -std=c++1z'
let g:ale_cpp_clangtidy_checks = [
\   'cppcoreguidelines-*',
\   'clang-analyzer-*',
\   'misc-*',
\   'modernize-*',
\   'performance-*',
\   'readability-*',
\   '-modernize-use-trailing-return-type',
\   '-misc-non-private-member-variables-in-classes',
\   '-cppcoreguidelines-avoid-magic-numbers',
\   '-readability-magic-numbers',
\   '-readability-braces-around-statements',
\   '-readability-else-after-return'
\]
let g:ale_cpp_clangtidy_extra_options = '-extra-arg=-std=c++2a'
let g:ale_completion_enabled          = 1
let g:ale_sign_error                  = "Er"
let g:ale_sign_warning                = "Wa"
let g:ale_change_sign_column_color    = 0
let g:ale_hover_to_preview            = 1

nnoremap <Leader>l :ALEToggle<CR>
nnoremap ]e :ALENext<CR>
nnoremap [e :ALEPrevious<CR>
nnoremap <Leader>D :ALEDetail<CR>
"}}}

" Coc (LSP)
" Plug 'neoclide/coc.nvim', {'branch': 'release'}

" asyncrun.vim (??? required by another plugin, don't remember which)
Plug 'skywind3000/asyncrun.vim' "{{{}}}

" auto-pairs-gentle (automatically close parentheses and much more)
" Plug 'jiangmiao/auto-pairs' "{{{
Plug 'vim-scripts/auto-pairs-gentle'

let g:AutoPairsUseInsertedCount = 1
" }}}

" ayu-vim (color squeme)
"Plug 'ayu-theme/ayu-vim'

" chuck.vim (ChucK integration)
"Plug 'wilsaj/chuck.vim'

" coconut.vim (language support for Coconut)
Plug 'manicmaniac/coconut.vim' "{{{}}}

" ctrlp.vim (fuzzy finder)
Plug 'kien/ctrlp.vim' "{{{

let g:ctrlp_map        = '<Leader>e'
let g:ctrlp_regexp     = 1
let g:ctrlp_showhidden = 1

nnoremap <Leader>t :CtrlPTag<CR>
"}}}

" dart-vim-plugin (language support for Dart)
Plug 'dart-lang/dart-vim-plugin' "{{{}}}

" golden-ratio (automatically manage split window sizes) XXX
" Plug 'roman/golden-ratio' "{{{}}}

" indentLine (show indent guides)
" Plug 'Yggdroot/indentLine' "{{{

" let g:indentLine_char          = '┊'
" " let g:indentLine_color_term    = 249
" let g:indentLine_concealcursor = ''

" nnoremap <Leader>g :IndentLinesToggle<CR>
"}}}

" let-modeline.vim (have a specific modeline for configuring plugins)
Plug 'vim-scripts/let-modeline.vim' "{{{}}}

" limelight.vim (a spotlight for code, good for presenting bit-by-bit)
Plug 'junegunn/limelight.vim' "{{{

let g:limelight_conceal_ctermfg = 242
let g:limelight_conceal_guifg   = '#606060' "}}}

" goyo.vim (make things pretty, useful for when I'm more tired or for more
"           elegant presentations)
Plug 'junegunn/goyo.vim'

" onedark.vim (OneDark colorsqueme from Atom)
Plug 'joshdick/onedark.vim' "{{{}}}

" supertab (do insert completion using TAB)
Plug 'ervandew/supertab' "{{{

let g:SuperTabDefaultCompletionType = "context" "}}}

" targets.vim (better text objects)
Plug 'wellle/targets.vim' "{{{}}}

" vCoolor.vim (color selector)
Plug 'KabbAmine/vCoolor.vim' "{{{

let g:vcoolor_disable_mappings = 1
" let g:vcoolor_map              = '<Leader>cc'

"nnoremap <Leader>crgb :VCoolIns r<CR>
nnoremap <Leader>crgb :VCoolIns r<CR>
nnoremap <Leader>chsl :VCoolIns h<CR>
"}}}

" vim-coloresque (show colors in source code)
Plug 'gko/vim-coloresque' "{{{}}}

" vim-commentary (BAD comment/uncomment code)
"Plug 'tpope/vim-commentary'

" tcomment_vim (comment/uncomment code)
Plug 'tomtom/tcomment_vim'

" vim-cpp-modern (better C++ syntax highlight)
Plug 'bfrg/vim-cpp-modern' "{{{

let g:cpp_no_function_highlight        = 0
let g:cpp_named_requirements_highlight = 1 "}}}

" vim-devicons (icons for files depending on their filetype)
Plug 'ryanoasis/vim-devicons' "{{{
"let g:WebDevIconsOS = 'Linux'
"set conceallevel=3
"let g:WebDevIconsUnicodeDecorateFolderNodes = 1
"let g:WebDevIconsNerdTreeAfterGlyphPadding = ' ' "}}}

" vim-dg (language support for DogeLang [lol])
Plug 'rubik/vim-dg' "{{{}}}

" vim-easy-align (align code)
Plug 'junegunn/vim-easy-align' "{{{
xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)
"}}}

" vim-fugitive (git wrapper)
Plug 'tpope/vim-fugitive' "{{{}}}

" vim-gitgutter (show git diff in the gutter)
Plug 'airblade/vim-gitgutter' "{{{}}}

" vim-glsl (language support for GLSL)
Plug 'tikhomirov/vim-glsl' "{{{}}}

" vim-jsx-pretty (language support for JSX)
Plug 'MaxMEllon/vim-jsx-pretty' "{{{}}}

" nerdtree (file system tree explorer)
" Plug 'preservim/nerdtree'

" vim-nerdtree-tabs (keep NERDTree open across tabs)
" Plug 'jistr/vim-nerdtree-tabs'

" vim-pandoc (lots of tools for Pandoc)
Plug 'vim-pandoc/vim-pandoc' "{{{}}}

" vim-pandoc-syntax (language support for Pandoc Markdown)
Plug 'vim-pandoc/vim-pandoc-syntax' "{{{}}}

" vim-racket (language support for Racket)
Plug 'wlangstroth/vim-racket' "{{{

set lispwords-=if
" }}}

" vim-repeat (better . command)
Plug 'tpope/vim-repeat' "{{{}}}

" vim-slime (send commands to a terminal)
Plug 'jpalardy/vim-slime' "{{{

let g:slime_target = 'vimterminal'
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

" vim-todo-lists (todo list management)
" Plug 'aserebryakov/vim-todo-lists' "{{{}}}

" vim-vinegar (improve NetRW)
Plug 'tpope/vim-vinegar' "{{{}}}

" vim-wordmotion (improve the `w` key and similar)
Plug 'chaoren/vim-wordmotion' "{{{}}}

" wolfram-vim (language support for Wolfram Language)
Plug 'arnoudbuzing/wolfram-vim' "{{{
" Setup wolfram.vim (WL syntax highlight)
autocmd BufNewFile,BufRead *.wl set ft=wl
autocmd BufNewFile,BufRead *.wls set ft=wl
autocmd BufNewFile,BufRead *.m set ft=wl "}}}

" yaml-vim (better language support for YAML)
Plug 'mrk21/yaml-vim' "{{{}}}

" vim-indent-object (text object for indented text)
Plug 'michaeljsmith/vim-indent-object'

" vim-textobj-user (framework for creating text objects [used by other plugins])
Plug 'kana/vim-textobj-user'

" vim-textobj-entire (text object for the entire buffer)
Plug 'kana/vim-textobj-entire'

" vim-textobj-fold (text object for a fold)
Plug 'kana/vim-textobj-fold'

" vim-textobj-function (text object for a function [in a few languages])
Plug 'kana/vim-textobj-function'

" vim-textobj-line (text object for the current line)
Plug 'kana/vim-textobj-line'

" vim-textobj-syntax (text object for text in the same highlight group)
Plug 'kana/vim-textobj-syntax'

" vim-todo-refs (count todo/fixme/bug)
" call s:PlugOwn('vim-todo-refs')

" vim-marp (a better environment for using Marp)
Plug 'dhruvasagar/vim-marp'

" codi.vim (for creating a scratch split buffer that evaluates code automatically)
Plug 'metakirby5/codi.vim'

" vim-eunuch (for adding nice commands for Bash commands)
Plug 'tpope/vim-eunuch'

" magma.vim (for interfacing with Jupyter)
call s:PlugOwn('magma.vim') " {{{

nnoremap <Leader><Leader><Leader> :MagmaEvaluate<CR>
nnoremap <Leader><Leader>o :MagmaShow<CR>
nnoremap <Leader><Leader>O :MagmaShow!<CR>

" nnoremap <Return> :MagmaEvaluate<CR>
nmap <Return><Return> \\\
" }}}


" vim-python-pep8-indent (indent according to PEP8)
Plug 'Vimjas/vim-python-pep8-indent'

" undotree (view vim's undo tree with ease)
Plug 'mbbill/undotree' "{{{

nnoremap <Leader>T :UndotreeToggle<CR>
"}}}

" vim-haskell-indent (proper autoindent for Haskell)
Plug 'itchyny/vim-haskell-indent'

" todo.vim (for todo lists)
call s:PlugOwn('todo.vim')

" vim-orgmode (for syntax support of orgmode files, but comes with some other
" features which I don't plan to use as well)
Plug 'jceb/vim-orgmode'
Plug 'tpope/vim-speeddating'

" vim-fish (syntax support for fish scripts)
Plug 'dag/vim-fish'

" vim-dispatch (for running async build processes)
Plug 'tpope/vim-dispatch'

" vim-coiled-snake (automatic folding for Python)
Plug 'kalekundert/vim-coiled-snake'
Plug 'Konfekt/FastFold'

" scratch.vim (handy scratch buffers)
Plug 'mtth/scratch.vim'

" taboo.vim (for naming tabs and other useful/pretty work with them)
Plug 'gcmt/taboo.vim' "{{{
let g:taboo_tab_format = "▏ %d %f ▕"
let g:taboo_renamed_tab_format = "▏  %d %l ▕"
let g:taboo_close_tab_label = "✗"

nnoremap <Leader>; :TabooRename<Space>
nnoremap <Leader>: :TabooOpen<Space>
"}}}

" vim-localvimrc (for using local [e.g. project-specific] vimrcs)
Plug 'embear/vim-localvimrc'

" notmuch-vim (email client)
Plug 'imain/notmuch-vim'

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
" Enable the mouse
set mouse=a " enable all features using the mouse

"" Use FISH as the shell
set shell=/usr/bin/fish

"" Setup line wrap
set wrap          " wrap lines
set linebreak     " don't break words on wrap
set breakindent   " indent wrapped lines
set showbreak=…\  " prefix for wrapped lines

"" Show line numbers
set number " show absolute line numbers

"" Show the foldcolumn
set foldcolumn=1 " have only one column in the fold column

"" Set the foldmethod
set foldmethod=manual " set folding to manual

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
set undodir=~/.vim/undodir " where to save undos
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
set incsearch " preview search results

"" Show possible tab completions above
set wildmenu " show completion menu in Ex commands

"" Never conceal the current line
set concealcursor= " don't conceal the current line in any mode

"" Show trailing whitespace and tabs
set list                       " show listchars
set listchars=tab:\ \ ,trail:┈ " show tabs as spaces and highlight trailing whitespace

"" Setup autocompletion in a way that is better
set completeopt=          " clear autocompletion options
set completeopt+=menuone  " Show the menu even if there is only one option
set completeopt+=popup    " Show additional information in a popup window
set completeopt+=noinsert " Don't insert text automatically
set completeopt+=noselect " Don't start the menu with an option selected

"" Always keep 2 lines around the cursor
set scrolloff=2 " keep 2 lines above & below the cursor at all times

"" Save tabs in sessions
set sessionoptions+=tabpages,globals " save tabpages and global vars in sessions

"" Always show the tabline
set showtabline=2 " show tab line even if only with one tab open

"" Accelerate Esc presses
set ttimeout        " Enable timeout
set ttimeoutlen=50  " Timeout for keycodes
set timeoutlen=3000 " Timeout for mappings

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
set statusline+=\ %{WebDevIconsGetFileTypeSymbol()}
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

" Change cursor in insert mode
let &t_SI = "\e[6 q"
let &t_SR = "\e[4 q"
let &t_EI = "\e[2 q"

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

" Make gj act like <C-w>j, etc.
"{{{
nnoremap <silent> gh <C-w>h
nnoremap <silent> gj <C-w>j
nnoremap <silent> gk <C-w>k
nnoremap <silent> gl <C-w>l
"}}}

" Swap ' and `
nnoremap ' `
nnoremap ` '

" Abbreviate :w to :up
cabbrev w up

" Unmap Q
nnoremap Q <nop>

" Add wrapper/helper for using figlet-toilet for inserting big text
function! s:BigText(hasbang, text) "{{{
    if a:hasbang == "!"
        execute "read! figlet -f mini -S -t \"" . a:text . "\" | sed \"s/\\s\\+\\$//\""
    else
        execute "read! figlet -f standard -S -k -t \"" . a:text . "\" | sed \"s/\\s\\+\\$//\""
    endif
endfunction

command! -bang -nargs=1 BigText call s:BigText("<bang>", "<args>") "}}}

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
    autocmd BufWinLeave,VimLeave,BufWritePost *.* mkview
    autocmd BufWinEnter,BufReadPost *.* loadview
augroup END "}}}

" Set hotkeys for changing between Magma and Slime
nnoremap <Leader><Leader>s :map <Leader><Leader><Leader> <Plug>SlimeParagraphSend<CR>
nnoremap <Leader><Leader>m :nnoremap <Leader><Leader><Leader> :MagmaEvaluate<lt>CR><CR>

" Set a hotkey for `:set relativenumber!`
nnoremap <Leader>r :set relativenumber!<CR>

" Set a hotkey for `:set spell!`
nnoremap <Leader>S :set spell!<CR>

" Set a hotkey for `:set spelllang=`
nnoremap <Leader>s :set spelllang=

" Set a hotkey for `gt` and `gT` (including in terminal mode!)
nnoremap <C-Tab>   gt
nnoremap <C-S-Tab> gT
tnoremap <C-Tab>   <C-w>gt
tnoremap <C-S-Tab> <C-w>gT

" Set a hotkey for `:nohl`
nnoremap <Leader>. :nohl<CR>

" Set a hotkey for `:CtrlP Notes/`
nnoremap <Leader>n :CtrlP Notes/<CR>

" Automatically redraw Vim if it is resized
autocmd VimResized * redraw

" " Automatically recompile Pandoc markdown
" autocmd CursorHold *.md
"             \ if exists(':Pandoc') 
"             \ |   Pandoc pdf
"             \ | endif

autocmd BufWritePost *.md Make!

autocmd FileType *.py setlocal fdm=expr
autocmd FileType *.coco setlocal fdm=expr foldexpr=coiledsnake#FoldExpr(v:lnum)

" Setup webdevicons
if exists('g:loaded_webdevicons') "{{{
    call webdevicons#refresh()
endif "}}}

"  VIM: let b:countTODOs_offset=-3
"  vim: fdm=marker fdc=5
