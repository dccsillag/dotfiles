if exists(':CompilerSet') != 2
    command -nargs=* CompilerSet setlocal <args>
endif

CompilerSet errorformat&
CompilerSet makeprg=cd\ %:h\ &&\ pan\ -f\ latex\ -o\ \"%:t:r.pdf\"\ -i\ \"%:t\"
