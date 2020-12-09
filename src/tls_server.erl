-module(tls_server).

-export([start/0, sni_fun/1]).

start() ->
    ssl:start(),
    Opts = [
        {cacertfile, "./tls-gen/basic/result/ca_certificate.pem"},
        {certfile, "./tls-gen/basic/result/server_certificate.pem"},
        {keyfile, "./tls-gen/basic/result/server_key.pem"},
        {reuseaddr, true},
        {sni_fun, fun tls_server:sni_fun/1}
    ],
    io:format("[INFO] before ssl:listen(9999, Opts)~n", []),
    {ok, ListenSocket} = ssl:listen(9999, Opts),
    io:format("[INFO] after ssl:listen(9999, Opts)~n", []),
    io:format("[INFO] before ssl:transport_accept~n", []),
    {ok, TLSTransportSocket} = ssl:transport_accept(ListenSocket),
    io:format("[INFO] after ssl:transport_accept~n", []),
    io:format("[INFO] before ssl:handshake~n", []),
    Socket =
        case ssl:handshake(TLSTransportSocket) of
            {ok, S, Ext} ->
                io:format("[INFO] ssl:handshake Ext: ~p~n", [Ext]),
                S;
            {ok, S} ->
                S;
            Error ->
                io:format("[ERROR] ssl:handshake Error: ~p~n", [Error]),
                init:stop()
        end,
    io:format("[INFO] after ssl:handshake~n", []),
    io:format("[INFO] ssl:handshake sni_hostname: ~p~n", [get_sni_hostname(Socket)]),
    loop(Socket).

loop(Socket) ->
    io:format("[INFO] top of loop/2~n", []),
    receive
        Data ->
            io:format("Data: ~p~n", [Data]),
            loop(Socket)
    after 5000 ->
        io:format("DONE!~n", []),
        ssl:close(Socket),
        init:stop()
    end.

sni_fun(ServerName) ->
    io:format("[INFO] sni_fun ServerName: ~p~n", [ServerName]),
    [].

get_sni_hostname(Socket) ->
    case ssl:connection_information(Socket, [sni_hostname]) of
        {ok, []} ->
            undefined;
        {ok, [{sni_hostname, Hostname}]} ->
            Hostname;
        Error ->
            io:format("[ERROR] ssl:connection_information Error: ~p~n", [Error]),
            undefined
    end.
