-module(tls_server).

-export([start/0]).

start() ->
    ssl:start(),
    Opts = [
        {cacertfile, "./tls-gen/basic/result/ca_certificate.pem"},
        {certfile, "./tls-gen/basic/result/server_certificate.pem"},
        {keyfile, "./tls-gen/basic/result/server_key.pem"},
        {reuseaddr, true}
    ],
    io:format("[INFO] before ssl:listen(9999, Opts)~n", []),
    {ok, ListenSocket} = ssl:listen(9999, Opts),
    io:format("[INFO] after ssl:listen(9999, Opts)~n", []),
    io:format("[INFO] before ssl:transport_accept~n", []),
    {ok, TLSTransportSocket} = ssl:transport_accept(ListenSocket),
    io:format("[INFO] after ssl:transport_accept~n", []),
    io:format("[INFO] before ssl:handshake~n", []),
    {ok, Socket} = ssl:handshake(TLSTransportSocket),
    io:format("[INFO] after ssl:handshake~n", []),
    loop(Socket).

loop(Socket) ->
    io:format("[INFO] top of loop/2~n", []),
    receive
        Data ->
            io:format("Data: ~p~n", [Data]),
            loop(Socket)
    after 10000 ->
            io:format("DONE!~n", []),
            ssl:close(Socket),
            init:stop()
    end.
