#!/bin/zsh

PS3='Please enter your choice (how to configure Git): '

select ans_email in Personal IMPA FGV
do
    case $ans_email in
        Personal)
            email=dccsillag@gmail.com
            name=dccsillag
            break
            ;;
        IMPA)
            email=daniel.csillag@impa.br
            name=Daniel Csillag
            break
            ;;
        FGV)
            email=daniel.csillag@fgv.edu.br
            name=Daniel Csillag
            break
            ;;
    esac
done

git config --global user.email $email
git config --global user.name $name
