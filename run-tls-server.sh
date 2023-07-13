#!/usr/bin/env bash

set -o errexit
set -o nounset

declare -r script_dir="$(realpath "$(dirname "$BASH_SOURCE")")"

# (cd "$script_dir/redbug" && rebar3 clean && rebar3 compile)

erlc +debug "$script_dir/src/tls_server.erl"
erlc +debug "$script_dir/src/custom_ssl_crl_cache.erl"

# erl -pa "$script_dir/redbug/_build/default/lib/redbug/ebin/" -noinput -s tls_server start
erl -noinput -s tls_server start
