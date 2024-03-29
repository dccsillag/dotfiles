#!/bin/sh
# initial idea: Florian Bruhin (The-Compiler)
# author: Thore Bödecker (foxxx0)
# source: https://github.com/qutebrowser/qutebrowser/blob/master/scripts/open_url_in_instance.sh

if [ $# -gt 0 ] && [ "$1" = '--private' ]
then
    _target=\"private-window\"
    shift 1
else
    _target=\"window\"
fi

if [ $# -gt 0 ]
then
    _url="$1"
else
    _url="about:blank"
fi
_qb_version='1.0.4'
_proto_version=1
_ipc_socket="${XDG_RUNTIME_DIR}/qutebrowser/ipc-$(printf '%s' "$USER" | md5sum | cut -d' ' -f1)"

printf '{"args": ["%s"], "target_arg": %s, "version": "%s", "protocol_version": %d, "cwd": "%s"}\n' \
       "${_url}" \
       "${_target}" \
       "${_qb_version}" \
       "${_proto_version}" \
       "${PWD}" | socat - UNIX-CONNECT:"${_ipc_socket}" 2>/dev/null || qutebrowser "$@" &
