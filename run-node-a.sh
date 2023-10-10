#!/usr/bin/env bash

set -o errexit
set -o nounset

declare -r script_dir="$(realpath "$(dirname "$BASH_SOURCE")")"

declare -r erl_ssl_path="$(erl -noinput -eval 'io:format("~s~n", [filename:dirname(code:which(inet_tls_dist))])' -s init stop)"

erl -pa "$erl_ssl_path" -proto_dist inet_tls -ssl_dist_optfile "$script_dir/inet-dist-tls.config" -sname a -eval 'net_kernel:verbose(2).'
