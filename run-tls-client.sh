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
    -CAfile ./tls-gen/basic/result/ca_certificate.pem \
    -cert "./tls-gen/basic/result/client_${hostname}_certificate.pem" \
    -key "./tls-gen/basic/result/client_${hostname}_key.pem" -servername "$hostname"
