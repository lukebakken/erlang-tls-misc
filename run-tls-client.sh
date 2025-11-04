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

case "$1" in
    no-intermediate)
        "$openssl_bin" s_client "$openssl_xtra_args" -ign_eof -connect localhost:4433 \
            -CAfile ./certs/ca_certificate.pem \
            -cert "./certs/client_certificate.pem" \
            -key "./certs/client_key.pem" -servername "$hostname"
        ;;
    intermediate)
        "$openssl_bin" s_client "$openssl_xtra_args" -ign_eof -connect localhost:4433 \
            -CAfile ./certs/ca_certificate.pem \
            -cert "./certs/client_with_intermediate.pem" \
            -key "./certs/client_key.pem" -servername "$hostname"
        ;;
    full-chain)
        "$openssl_bin" s_client "$openssl_xtra_args" -ign_eof -connect localhost:4433 \
            -CAfile ./certs/ca_certificate.pem \
            -cert "./certs/client_full_chain.pem" \
            -key "./certs/client_key.pem" -servername "$hostname"
        ;;
    *)
        echo '[ERROR] first arg must be no-intermediate, intermediate, or full-chain' 1>&2
        ;;
esac
