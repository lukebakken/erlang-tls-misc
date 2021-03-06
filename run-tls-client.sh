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

echo 'FOO BAR BAZ' | "$openssl_bin" s_client "$openssl_xtra_args" -ign_eof -connect localhost:9999 \
    -CAfile ./tls-gen/basic/result/ca_certificate.pem \
    -cert ./tls-gen/basic/result/client_certificate.pem \
    -key ./tls-gen/basic/result/client_key.pem -servername "$(hostname)"
