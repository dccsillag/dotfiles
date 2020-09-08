#!/bin/sh

set -e

PACKAGEFILE=$HOME/.csillag/packages

if [ "$(id -u)" = 0 ]
then
    if [ -z "$DISTRONAME" ]
    then
        DISTRONAME=$(lsb_release -is)
    fi
elif [ -z "$DISTRONAME" ]
then
    echo "This script has not been run as root. By continuing, miniconda will be installed"
    echo "  (if not already installed) and used as the package manager to install packages."
    echo
    PS3="Press ENTER to continue, Ctrl+C to abort. "
    read

    # Install Miniconda
    if ! which conda
    then
        curl "https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh" | sh
    fi

    # Run this script in a shell which Miniconda supports
    export DISTRONAME=conda-forge
    if which bash
    then
        bash "$(realpath "$0")"
    elif which zsh
    then
        zsh "$(realpath "$0")"
    else
        echo "No shell that is supported was found!"
        echo "Please install bash or zsh."
        exit 1
    fi
    exit 0
fi

STARTLINENUM=$(grep -m 1 -n '^-' "$PACKAGEFILE" | sed 's/^\([0-9]\+\):.\+$/\1/')
DISTROS=$(sed "$((STARTLINENUM-1))q;d" "$PACKAGEFILE")
DISTRONUM=$(echo "$DISTROS" | sed 's/ \+/\n/g' | grep -n "^$DISTRONAME$" | sed 's/^\([0-9]\+\):.\+$/\1/')

if [ -z "$DISTRONUM" ]
then
    echo "Your distro ($DISTRONAME) does not appear in '$PACKAGEFILE'."
    echo "If you want to force it to some particular value, export"
    echo "  \`DISTRONAME=YourDistroName\` on your shell."
    exit 2
fi

PACKAGES=$(tail -n +$((STARTLINENUM+1)) "$PACKAGEFILE" | grep -v '^#' | sed 's/ \+/\t/g' | cut -f "$DISTRONUM" | grep -v _)

# Apply filters
if [ -n "$1" ]
then
    PACKAGES=$(echo $(PACKAGES) | grep "^$1 ")
fi

# Remove duplicate packages
PACKAGES=$(echo $(PACKAGES) | uniq)

echo "Your distro is: $DISTRONAME"

case $DISTRONAME in
    Ubuntu)
        # shellcheck disable=SC2046 # these word splits are intentional
        apt install $(echo "$PACKAGES" | tr '\n' ' ')
        ;;
    ManjaroLinux)
        # shellcheck disable=SC2046 # these word splits are intentional
        yay -Syu $(echo "$PACKAGES" | tr '\n' ' ')
        ;;
    conda-forge)
        if conda env list | grep csillag
        then
            conda env create -n csillag
        fi
        conda activate csillag
        # shellcheck disable=SC2046 # these word splits are intentional
        conda install -c conda-forge $(echo "$PACKAGES" | tr '\n' ' ')
        ;;
    *)
        echo "Unregistered distro: $DISTRONAME"
        exit 2
esac

echo "Running post-install scripts..."
find "$HOME/.csillag/post-package-install/" -exec /bin/sh '{}' \; -name "$DISTRONAME.sh" -or -name all.sh
