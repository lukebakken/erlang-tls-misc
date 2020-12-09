#!/usr/bin/env bash

set -o errexit
set -o nounset

declare -r script_dir="$(realpath "$(dirname "$BASH_SOURCE")")"

git submodule update --init

make -C "$script_dir/tls-gen/basic"

sed -e "s|##PWD##|$script_dir|" "$script_dir/rabbitmq-tls.config.in" > "$script_dir/rabbitmq-tls.config"

virtualenv venv
source ./venv/bin/activate
pip install pika
