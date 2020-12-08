#!/bin/sh

set -e
set -u

echo 'FOO BAR BAZ' | openssl s_client -connect localhost:9999 \
    -CAfile ./tls-gen/basic/result/ca_certificate.pem \
    -cert ./tls-gen/basic/result/client_certificate.pem \
    -key ./tls-gen/basic/result/client_key.pem -servername "$(hostname)"
