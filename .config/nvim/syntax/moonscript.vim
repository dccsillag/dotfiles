" Language:    MoonScript
" Maintainer:  dccsillag
" License:     WTFPL
"
" Based on https://github.com/leafo/moonscript-vim/blob/master/syntax/moon.vim

" Bail if our syntax is already loaded.
if exists('b:current_syntax') && b:current_syntax == 'moon'
  finish
endif

if version < 600
  syn clear
endif

" A non-interpolated string
syn cluster moonBasicString contains=@Spell,moonEscape
" An interpolated string
syn cluster moonInterpString contains=@moonBasicString,moonInterp

" Regular strings
syn region moonString start=/"/ skip=/\\\\\|\\"/ end=/"/
\                       contains=@moonInterpString
syn region moonString start=/'/ skip=/\\\\\|\\'/ end=/'/
\                       contains=@moonBasicString
hi def link moonString String

syn region moonString2 matchgroup=moonString start="\[\z(=*\)\[" end="\]\z1\]" contains=@Spell
hi def link moonString2 String


" Strings used in string assignments, which can't have interpolations
syn region moonAssignString start=/"/ skip=/\\\\\|\\"/ end=/"/ contained
\                             contains=@moonBasicString
syn region moonAssignString start=/'/ skip=/\\\\\|\\'/ end=/'/ contained
\                             contains=@moonBasicString
hi def link moonAssignString String

syn keyword moonTodo TODO FIXME XXX contained
hi def link moonTodo Todo

syn match moonComment "\%^#!.*"
syn match moonComment /--.*/ contains=@Spell,moonTodo
hi def link moonComment Comment

" syn region moonBlockComment start=/####\@!/ end=/###/
" \                             contains=@Spell,moonTodo
" hi def link moonBlockComment moonComment

syn region moonInterp matchgroup=moonInterpDelim start=/#{/ end=/}/ contained
\                       contains=@moonAll
hi def link moonInterpDelim PreProc

" A string escape sequence
syn match moonEscape /\\\d\d\d\|\\x\x\{2\}\|\\u\x\{4\}\|\\./ contained display
hi def link moonEscape SpecialChar

" This is used instead of TOP to keep things moon-specific for good
" embedding. `contained` groups aren't included.
syn cluster moonAll contains=moonStatement,moonRepeat,moonConditional,
\                              moonKeyword,moonOperator,moonFunction,
\                              moonExtendedOp,moonSpecialOp,moonBoolean,
\                              moonGlobal,moonSpecialVar,
\                              moonString,moonComment

if !exists('b:current_syntax')
  let b:current_syntax = 'moonscript'
endif
