#!/usr/bin/env bash

set -o errexit
set -o nounset

declare -r script_dir="$(realpath "$(dirname "$BASH_SOURCE")")"

git submodule update --init

pushd redbug && rebar3 compile && popd
make -C "$script_dir/tls-gen/basic"
make -C "$script_dir/tls-gen/basic" gen-crl

sed -e "s|##PWD##|$script_dir|" -e "s|##HOSTNAME##|$(hostname)|" "$script_dir/inet-dist-tls.config.in" > "$script_dir/inet-dist-tls.config"
