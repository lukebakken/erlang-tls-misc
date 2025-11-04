#!/bin/sh

set -e
set -u

if command -v brew
then
    readonly openssl_bin="$(brew --prefix openssl)/bin/openssl"
    readonly openssl_xtra_args="-tls1_3"
else
    readonly openssl_bin='openssl'
    readonly openssl_xtra_args="-tls1_3"
fi

readonly hostname="$(hostname)"

"$openssl_bin" s_client "$openssl_xtra_args" -ign_eof -connect localhost:4433 \
    -verify_depth 4 \
    -CAfile ./certs/ca_certificate.pem \
    -chainCAfile ./certs/chained_ca_certificate.pem \
    -cert "./certs/client_certificate.pem" \
    -key "./certs/client_key.pem" -servername "$hostname"
