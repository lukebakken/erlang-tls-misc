#!/usr/bin/env bash

set -o errexit
set -o nounset

declare -r script_dir="$(realpath "$(dirname "$BASH_SOURCE")")"

erlc +debug "$script_dir/src/tls_server.erl"

erl -noinput -s tls_server start
