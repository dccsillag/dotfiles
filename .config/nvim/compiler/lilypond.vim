if exists(':CompilerSet') != 2
    command -nargs=* CompilerSet setlocal <args>
endif

CompilerSet errorformat&
CompilerSet makeprg=cd\ %:h\ &&\ lilypond\ %:t
