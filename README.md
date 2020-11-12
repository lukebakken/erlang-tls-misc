# erlang-inet-dist

Example of using TLS for inter-node Erlang communication.

## Resources

https://erlef.github.io/security-wg/secure_coding_and_deployment_hardening/ssl.html

https://elixirforum.com/t/ssl-with-tortoise-lib-not-working/18256/4

https://github.com/vernemq/vernemq/issues/1485

https://github.com/hexpm/hex/pull/798

## Prerequisites

* `bash` version 4 or higher (tested with `5.0.18`)
* `git` (tested with `2.29.2`)
* `sed` (tested with GNU version `4.8`)
* `python` (tested with `3.8.6`)
* `openssl` (tested with `1.1.1h`)
* `erl` (tested with `23.1.2`)

## Usage

* Set up environment and certificates:

```
./setup.sh
```

* Run node a in one terminal:

```
./run-node-a.sh
```

* Run node b in another terminal:

```
./run-node-b.sh
```

## Expected output:

```
$ ./run-node-b.sh 
Erlang/OTP 23 [erts-11.1.2] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [hipe]

Eshell V11.1.2  (abort with ^G)
(b@shostakovich)1> ['a@shostakovich']

(b@shostakovich)1> nodes().
[a@shostakovich]
(b@shostakovich)2> init:stop().
ok
(b@shostakovich)3>
```
