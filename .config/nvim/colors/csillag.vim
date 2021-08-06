" CSILLAG syntax
" vim: tw=150 ts=4 sw=4 nowrap
" Vim color file
" Maintainer: Daniel Csillag <dccsillag@gmail.com>

highlight clear
if exists("syntax_on")
  syntax reset
endif
set background=dark
let g:colors_name = "csillag"

" Setup undercurl
let &t_Cs="\e[4:3m"
let &t_Ce="\e[4:0m"

" Core

highlight NormalNC         ctermfg=NONE ctermbg=NONE cterm=NONE guifg=#FFFFFF guibg=NONE gui=NONE
highlight VertSplit        ctermfg=NONE ctermbg=NONE cterm=NONE guifg=#364048 guibg=NONE gui=NONE
highlight StatusLine       ctermfg=NONE ctermbg=NONE cterm=NONE guifg=#364048 guibg=NONE gui=NONE
highlight StatusLineNC     ctermfg=NONE ctermbg=NONE cterm=NONE guifg=#364048 guibg=NONE gui=NONE
highlight StatusLineTerm   ctermfg=NONE ctermbg=NONE cterm=NONE guifg=#364048 guibg=NONE gui=NONE
highlight StatusLineTermNC ctermfg=NONE ctermbg=NONE cterm=NONE guifg=#364048 guibg=NONE gui=NONE
highlight Normal           ctermfg=NONE ctermbg=NONE cterm=NONE guifg=#FFFFFF guibg=NONE gui=NONE

highlight ColorColumn       ctermfg=NONE      ctermbg=darkgray   cterm=NONE               guifg=NONE      guibg=#333333    gui=NONE
highlight Conceal           ctermfg=NONE      ctermbg=NONE       cterm=NONE               guifg=NONE      guibg=NONE       gui=NONE
highlight Cursor            ctermfg=NONE      ctermbg=NONE       cterm=reverse            guifg=NONE      guibg=NONE       gui=reverse
highlight lCursor           ctermfg=NONE      ctermbg=NONE       cterm=reverse            guifg=NONE      guibg=NONE       gui=reverse
highlight CursorIM          ctermfg=NONE      ctermbg=NONE       cterm=reverse            guifg=NONE      guibg=NONE       gui=reverse
highlight CursorColumn      ctermfg=white     ctermbg=darkgray   cterm=NONE               guifg=NONE      guibg=#555555    gui=NONE
highlight CursorLine        ctermfg=white     ctermbg=darkgray   cterm=NONE               guifg=NONE      guibg=#555555    gui=NONE
highlight DiffChange        ctermfg=black     ctermbg=yellow     cterm=NONE               guifg=#0000FF   guibg=NONE       gui=NONE
highlight DiffText          ctermfg=NONE      ctermbg=NONE       cterm=NONE               guifg=NONE      guibg=NONE       gui=NONE
highlight DiffAdd           ctermfg=black     ctermbg=green      cterm=NONE               guifg=#00FF00   guibg=NONE       gui=NONE
highlight DiffDelete        ctermfg=white     ctermbg=red        cterm=NONE               guifg=#FF0000   guibg=NONE       gui=NONE
highlight EndOfBuffer       ctermfg=NONE      ctermbg=NONE       cterm=NONE               guifg=NONE      guibg=NONE       gui=NONE
highlight ErrorMsg          ctermfg=red       ctermbg=NONE       cterm=bold               guifg=#FF0000   guibg=NONE       gui=bold
highlight Folded            ctermfg=lightgray ctermbg=NONE       cterm=bold               guifg=#AAAAAA   guibg=NONE       gui=bold
highlight FoldColumn        ctermfg=white     ctermbg=NONE       cterm=bold               guifg=#FFFFFF   guibg=NONE       gui=bold
highlight SignColumn        ctermfg=white     ctermbg=NONE       cterm=NONE               guifg=#FFFFFF   guibg=NONE       gui=NONE
highlight IncSearch         ctermfg=NONE      ctermbg=NONE       cterm=bold,reverse       guifg=#CCCCCC   guibg=NONE       gui=bold,reverse
highlight Search            ctermfg=NONE      ctermbg=NONE       cterm=bold,reverse       guifg=#CCCCCC   guibg=NONE       gui=bold,reverse
highlight LineNr            ctermfg=darkgray  ctermbg=NONE       cterm=NONE               guifg=#7c7d7f   guibg=NONE       gui=NONE
highlight CursorLineNr      ctermfg=darkgray  ctermbg=NONE       cterm=bold               guifg=#7c7d7f   guibg=NONE       gui=bold
highlight MatchParen        ctermfg=NONE      ctermbg=darkgray   cterm=NONE               guifg=NONE      guibg=#575859    gui=NONE
highlight ModeMsg           ctermfg=NONE      ctermbg=NONE       cterm=bold               guifg=NONE      guibg=NONE       gui=bold
highlight MoreMsg           ctermfg=NONE      ctermbg=NONE       cterm=bold               guifg=NONE      guibg=NONE       gui=bold
highlight NonText           ctermfg=gray      ctermbg=NONE       cterm=NONE               guifg=#777777   guibg=NONE       gui=NONE
"highlight Pmenu
"highlight PmenuSel
"highlight PmenuSbar
"highlight PmenuThumb
highlight Question          ctermfg=NONE      ctermbg=NONE       cterm=bold               guifg=NONE      guibg=NONE       gui=bold
highlight QuickFixLine      ctermfg=NONE      ctermbg=NONE       cterm=bold               guifg=NONE      guibg=NONE       gui=bold
highlight SpecialKey        ctermfg=blue      ctermbg=NONE       cterm=bold               guifg=#00afea   guibg=NONE       gui=bold
" FIXME: TabLine, TabLineFill, TabLineSel
highlight TabLineFill       ctermfg=black     ctermbg=white      cterm=NONE               guifg=#000000   guibg=#AAAAAA    gui=NONE
highlight TabLine           ctermfg=black     ctermbg=white      cterm=NONE               guifg=#000000   guibg=#777777    gui=NONE
highlight TabLineSel        ctermfg=black     ctermbg=white      cterm=bold               guifg=#000000   guibg=#555555    gui=bold
highlight Terminal          ctermfg=NONE      ctermbg=NONE       cterm=NONE               guifg=NONE      guibg=NONE       gui=NONE
highlight Visual            ctermfg=NONE      ctermbg=darkgray   cterm=NONE               guifg=NONE      guibg=#444444    gui=NONE
highlight VisualNOS         ctermfg=NONE      ctermbg=darkgray   cterm=NONE               guifg=NONE      guibg=#444444    gui=NONE
highlight WarningMsg        ctermfg=yellow    ctermbg=NONE       cterm=underline          guifg=#FFFF00   guibg=NONE       gui=underline
highlight WildMenu          ctermfg=black     ctermbg=yellow     cterm=bold               guifg=#000000   guibg=#FFFF00    gui=bold
highlight Title             ctermfg=white     ctermbg=NONE       cterm=bold,underline     guifg=#FFFFFF   guibg=NONE       gui=bold,underline
highlight PopupNotification ctermfg=black     ctermbg=white      cterm=bold               guifg=#FFFFFF   guibg=#000000    gui=bold

highlight link LineNrAbove LineNr
highlight link LineNrBelow LineNr

"highlight ShowMarksHL      ctermfg=black     ctermbg=lightblue  cterm=bold               guifg=black     guibg=lightblue  gui=bold

" Extra

highlight Specs            ctermfg=NONE      ctermbg=white      cterm=NONE               guifg=NONE      guibg=#FFFFFF    gui=NONE

" NetRW

highlight Directory        ctermfg=NONE      ctermbg=NONE       cterm=NONE               guifg=#FFFFFF   guibg=NONE       gui=NONE

" Syntax

highlight Comment          ctermfg=gray       ctermbg=NONE       cterm=italic             guifg=#A5A6A9   guibg=NONE       gui=italic
highlight Constant         ctermfg=white      ctermbg=NONE       cterm=NONE               guifg=#FFFFFF   guibg=NONE       gui=NONE
highlight Special          ctermfg=white      ctermbg=NONE       cterm=NONE               guifg=#FFFFFF   guibg=NONE       gui=NONE
highlight Identifier       ctermfg=white      ctermbg=NONE       cterm=NONE               guifg=#FFFFFF   guibg=NONE       gui=NONE
highlight Statement        ctermfg=white      ctermbg=NONE       cterm=NONE               guifg=#FFFFFF   guibg=NONE       gui=NONE
highlight Preproc          ctermfg=white      ctermbg=NONE       cterm=NONE               guifg=#FFFFFF   guibg=NONE       gui=NONE
highlight Type             ctermfg=white      ctermbg=NONE       cterm=NONE               guifg=#FFFFFF   guibg=NONE       gui=NONE
highlight Underlined       ctermfg=white      ctermbg=NONE       cterm=underline          guifg=#FFFFFF   guibg=NONE       gui=underline
highlight Ignore           ctermfg=white      ctermbg=NONE       cterm=NONE               guifg=#FFFFFF   guibg=NONE       gui=NONE
highlight Error            ctermfg=white      ctermbg=NONE       cterm=underline          guifg=#FFFFFF   guibg=NONE       gui=underline
highlight Todo             ctermfg=black      ctermbg=yellow     cterm=NONE               guifg=#000000   guibg=#FFFF00    gui=NONE
highlight String           ctermfg=white      ctermbg=NONE       cterm=NONE               guifg=#FFFFFF   guibg=NONE       gui=NONE
highlight Character        ctermfg=white      ctermbg=NONE       cterm=NONE               guifg=#FFFFFF   guibg=NONE       gui=NONE
highlight Number           ctermfg=white      ctermbg=NONE       cterm=NONE               guifg=#FFFFFF   guibg=NONE       gui=NONE
highlight Function         ctermfg=white      ctermbg=NONE       cterm=NONE               guifg=#FFFFFF   guibg=NONE       gui=NONE
highlight Label            ctermfg=white      ctermbg=NONE       cterm=NONE               guifg=#FFFFFF   guibg=NONE       gui=NONE
highlight Operator         ctermfg=white      ctermbg=NONE       cterm=NONE               guifg=#FFFFFF   guibg=NONE       gui=NONE
highlight Include          ctermfg=white      ctermbg=NONE       cterm=NONE               guifg=#FFFFFF   guibg=NONE       gui=NONE
highlight Define           ctermfg=white      ctermbg=NONE       cterm=NONE               guifg=#FFFFFF   guibg=NONE       gui=NONE
highlight SpecialChar      ctermfg=white      ctermbg=NONE       cterm=NONE               guifg=#FFFFFF   guibg=NONE       gui=NONE
highlight Delimiter        ctermfg=white      ctermbg=NONE       cterm=NONE               guifg=#FFFFFF   guibg=NONE       gui=NONE
highlight SpecialComment   ctermfg=gray       ctermbg=NONE       cterm=NONE               guifg=#A5A6A9   guibg=NONE       gui=NONE
highlight Debug            ctermfg=white      ctermbg=NONE       cterm=NONE               guifg=#FFFFFF   guibg=NONE       gui=NONE

highlight link Conditional  Statement
highlight link Repeat       Statement
highlight link Keyword      Statement
highlight link Exception    Statement
highlight link Tag          Statement
highlight link Float        Number
highlight link Macro        Define
highlight link Precondit    Preproc
highlight link StorageClass Type
highlight link Structure    Type
highlight link Typedef      Type
highlight link Boolean      Identifier

" ALE
highlight ALEError             ctermfg=red       ctermbg=NONE       cterm=underline          guifg=#FF0000   guibg=NONE       gui=underline
highlight ALEWarning           ctermfg=yellow    ctermbg=NONE       cterm=underline          guifg=#FFFF00   guibg=NONE       gui=underline
highlight ALEStyleError        ctermfg=blue      ctermbg=NONE       cterm=underline          guifg=#A020F0   guibg=NONE       gui=underline
highlight ALEStyleErrorSign    ctermfg=blue      ctermbg=NONE       cterm=underline          guifg=#A020F0   guibg=NONE       gui=underline
highlight ALEStyleWarning      ctermfg=blue      ctermbg=NONE       cterm=underline          guifg=#A020F0   guibg=NONE       gui=underline
highlight ALEStyleWarningSign  ctermfg=darkblue  ctermbg=NONE       cterm=underline          guifg=#A020F0   guibg=NONE       gui=underline
highlight ALEInfo              ctermfg=green     ctermbg=NONE       cterm=underline          guifg=#00FF00   guibg=NONE       gui=underline

" CoC
highlight Pmenu            ctermfg=white     ctermbg=darkgray   cterm=NONE               guifg=#FFFFFF   guibg=#222222    gui=NONE
highlight CocFloat         ctermfg=white     ctermbg=darkgray   cterm=NONE               guifg=#FFFFFF   guibg=#222222    gui=NONE
highlight NormalFloat      ctermfg=white     ctermbg=darkgray   cterm=NONE               guifg=#FFFFFF   guibg=#222222    gui=NONE

" Spell
highlight SpellBad         ctermfg=NONE      ctermbg=NONE       cterm=undercurl          guifg=NONE      guibg=NONE       gui=undercurl
highlight SpellCap         ctermfg=gray      ctermbg=NONE       cterm=undercurl          guifg=#BEBEBE   guibg=NONE       gui=undercurl
highlight SpellRare        ctermfg=gray      ctermbg=NONE       cterm=NONE               guifg=#BEBEBE   guibg=NONE       gui=NONE
highlight SpellLocal       ctermfg=gray      ctermbg=NONE       cterm=NONE               guifg=#BEBEBE   guibg=NONE       gui=NONE

" Termdebug
highlight debugPC          ctermfg=NONE      ctermbg=blue       cterm=NONE               guifg=NONE      guibg=NONE       gui=undercurl
highlight debugBreakpoint  ctermfg=NONE      ctermbg=red        cterm=NONE               guifg=NONE      guibg=#FF0000    gui=NONE

" Magma

" highlight MagmaHoldSign    ctermfg=darkyellow ctermbg=NONE        cterm=bold        guifg=#EBAC00 guibg=NONE    gui=bold
" highlight MagmaRunningSign ctermfg=blue       ctermbg=NONE        cterm=bold,italic guifg=#295EFF guibg=NONE    gui=bold,italic
" highlight MagmaOkSign      ctermfg=green      ctermbg=NONE        cterm=bold        guifg=#03B000 guibg=NONE    gui=bold
" highlight MagmaErrSign     ctermfg=red        ctermbg=NONE        cterm=bold        guifg=#E32A00 guibg=NONE    gui=bold
" highlight MagmaHoldLine    ctermfg=NONE       ctermbg=darkyellow  cterm=NONE        guifg=NONE    guibg=#66543D gui=NONE
" highlight MagmaRunningLine ctermfg=NONE       ctermbg=blue        cterm=NONE        guifg=NONE    guibg=#3D4566 gui=NONE
" highlight MagmaOkLine      ctermfg=NONE       ctermbg=green       cterm=NONE        guifg=NONE    guibg=#3D6643 gui=NONE
" highlight MagmaErrLine     ctermfg=NONE       ctermbg=red         cterm=NONE        guifg=NONE    guibg=#663D3D gui=NONE

" Signature

highlight SignatureMarkText ctermfg=green ctermbg=NONE cterm=bold guifg=#f2b409 guibg=NONE gui=bold

" LSP

highlight LspDiagnosticsFloatingError        guifg=NONE      guibg=Red
highlight LspDiagnosticsFloatingWarning      guifg=NONE      guibg=Orange
highlight LspDiagnosticsFloatingInformation  guifg=NONE      guibg=LightBlue
highlight LspDiagnosticsFloatingHint         guifg=NONE      guibg=LightGray
highlight LspDiagnosticsUnderlineError       guifg=white     guibg=Red
highlight LspDiagnosticsUnderlineWarning     guifg=black     guibg=Orange
highlight LspDiagnosticsUnderlineInformation guifg=black     guibg=LightBlue
highlight LspDiagnosticsUnderlineHint        guifg=black     guibg=LightGray
