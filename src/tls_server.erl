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
    ok = io:format("[INFO] before ssl:listen(9999, Opts)~n", []),
    {ok, ListenSocket} = ssl:listen(9999, Opts),
    ok = io:format("[INFO] after ssl:listen(9999, Opts)~n", []),
    ok = io:format("[INFO] before ssl:transport_accept~n", []),
    {ok, TLSTransportSocket} = ssl:transport_accept(ListenSocket),
    ok = io:format("[INFO] after ssl:transport_accept~n", []),
    ok = io:format("[INFO] before ssl:handshake~n", []),
    Socket =
        case ssl:handshake(TLSTransportSocket) of
            {ok, S, Ext} ->
                ok = io:format("[INFO] ssl:handshake Ext: ~p~n", [Ext]),
                S;
            {ok, S} ->
                S;
            Error ->
                ok = io:format("[ERROR] ssl:handshake Error: ~p~n", [Error]),
                ok = init:stop()
        end,
    ok = io:format("[INFO] after ssl:handshake~n", []),
    ok = io:format("[INFO] ssl:handshake sni_hostname: ~p~n", [get_sni_hostname(Socket)]),
    write_keylog(Socket),
    loop(Socket).

loop(Socket) ->
    ok = io:format("[INFO] top of loop/2~n", []),
    receive
        Data ->
            ok = io:format("[INFO] Data: ~p~n", [Data]),
            loop(Socket)
    after 5000 ->
        ok = io:format("[INFO] DONE!~n", []),
        ok = ssl:close(Socket),
        ok = init:stop()
    end.

write_keylog(Socket) ->
    ok = io:format("[INFO] writing keylog data to keylog.bin~n", []),
    KeylogItems1 =
        case ssl:connection_information(Socket, [keylog]) of
            {ok, [{keylog, KeylogItems0}]} ->
                KeylogItems0;
            {ok, []} ->
                ok = io:format("[WARNING] no keylog data!~n", []),
                []
        end,
    ok = file:write_file("keylog.bin", [[KeylogItem, $\n] || KeylogItem <- KeylogItems1]),
    ok.

sni_fun(ServerName) ->
    ok = io:format("[INFO] sni_fun ServerName: ~p~n", [ServerName]),
    [].

get_sni_hostname(Socket) ->
    case ssl:connection_information(Socket, [sni_hostname]) of
        {ok, []} ->
            undefined;
        {ok, [{sni_hostname, Hostname}]} ->
            Hostname;
        Error ->
            ok = io:format("[ERROR] ssl:connection_information Error: ~p~n", [Error]),
            undefined
    end.
