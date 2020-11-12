#!/usr/bin/env bash

set -o errexit
set -o nounset

declare -r script_dir="$(realpath "$(dirname "$BASH_SOURCE")")"

git submodule update --init

make -C "$script_dir/tls-gen/basic"

sed -e "s|##PWD##|$script_dir|" "$script_dir/inet-dist-tls.config.in" > "$script_dir/inet-dist-tls.config"
