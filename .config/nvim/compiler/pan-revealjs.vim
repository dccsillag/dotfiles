if exists(':CompilerSet') != 2
    command -nargs=* CompilerSet setlocal <args>
endif

CompilerSet errorformat&
CompilerSet makeprg=cd\ %:h\ &&\ pan\ -f\ revealjs\ -o\ \"%:t:r.html\"\ -i\ \"%:t\"
