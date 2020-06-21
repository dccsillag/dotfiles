#!/bin/sh

set -e

PACKAGEFILE=$HOME/.csillag/packages

if [ -z "$DISTRONAME" ]
then
    DISTRONAME=$(lsb_release -is)
fi

STARTLINENUM=$(grep -m 1 -n '^-' "$PACKAGEFILE" | sed 's/^\([0-9]\+\):.\+$/\1/')
DISTROS=$(sed "$((STARTLINENUM-1))q;d" "$PACKAGEFILE")
DISTRONUM=$(echo "$DISTROS" | sed 's/ \+/\n/g' | grep -n "$DISTRONAME" | sed 's/^\([0-9]\+\):.\+$/\1/')

if [ -z "$DISTRONUM" ]
then
    echo "Your distro ($DISTRONAME) does not appear in '$PACKAGEFILE'."
    echo "If you want to force it to some particular value, export"
    echo "  \`DISTRONAME=YourDistroName\` on your shell."
    exit 2
fi

PACKAGES=$(tail -n +$((STARTLINENUM+1)) "$PACKAGEFILE" | grep -v '^#' | sed 's/ \+/\t/g' | cut -f "$DISTRONUM" | grep -v _)

echo "Your distro is: $DISTRONAME"

set -x

case $DISTRONAME in
    Ubuntu)
        # shellcheck disable=SC2086 # these word splits are intentional
        apt install $PACKAGES
        ;;
    ManjaroLinux)
        # shellcheck disable=SC2086,SC2046 # these word splits are intentional
        yay -Syu $(echo $PACKAGES | tr '\n' ' ')
        ;;
    *)
        echo "Unregistered distro: $DISTRONAME"
        exit 2
esac

set +x

echo "Running post-install scripts..."
find "$HOME/.csillag/post-package-install/" -exec /bin/sh '{}' \; -name "$DISTRONAME.sh" -or -name all.sh
