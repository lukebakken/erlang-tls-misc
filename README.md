# erlang-tls-misc

`one-intermediate` branch has example certs with the following chain:

```
Root -> Intermediate -> Server / Client
```

The `certs/` directory has the following files:

| File                              | Description                                   |
| --------------------------------- | --------------------------------------------- |
| `ca_certificate.pem`              | Root CA cert                                  |
| `intermediate_ca_certificate.pem` | Intermediate CA cert                          |
| `chained_ca_certificate.pem`      | Intermediate -> Root (concatenated)           |
| `client_certificate.pem`          | Client X509 cert                              |
| `client_key.pem`                  | Client X509 key                               |
| `client_full_chain.pem`           | Client -> Intermediate -> Root (concatenated) |
| `client_with_intermediate.pem`    | Client -> Intermediate (concatenated)         |
| `server_certificate.pem`          | Server X509 cert                              |
| `server_key.pem`                  | Server X509 key                               |

## Prerequisites

* `bash` version 4 or higher (tested with `5.2.21`)
* `git` (tested with `2.43.0`)
* `sed` (tested with GNU version `4.9`)
* `python` (tested with `3.14.0`)
* `openssl` (tested with `3.0.13`)
* `erl` (tested with `27.3.4.4`)

## Running TLS Server

The `run-tls-server.sh` script's first argument is to the CA certificate to
use. The following _only_ uses the Root CA cert, for instance:

```
./run-tls-server.sh certs/ca_certificate.pem
```

To use the Root CA and Intermediate:

```
./run-tls-server.sh certs/chained_ca_certificate.pem
```

## Running TLS Client

The `run-tls-client.sh` script takes one of these arguments:

* `no-intermediate` - only use Client X509 cert
* `intermediate` - use `client_with_intermediate.pem` file, with client and intermediate concatenated
* `full-chain` - use `client_full_chain.pem` file


## Results

TL;DR you **MUST** have the intermediate X509 cert available to the Erlang VM for _any_ client cert auth scenario to work.
