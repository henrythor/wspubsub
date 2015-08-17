-module(ws_pub_handler).
-include("ws_pub_handler.hrl").
-export([init/2
        ,websocket_handle/3
        ,websocket_info/3
        ,terminate/3
        ]).

init(Req, _Opts) ->
    [ApiKey, Domain, Topic] = cowboy_req:path_info(Req),
    Args = [{owner, self()}, {domain, Domain}, {topic, Topic}, {api_key, ApiKey}],
    {ok, Pid} = gen_server:start(wspubsub_srv, Args, []),
    State = #ws_pub{server = Pid, domain = Domain, topic = Topic},
	{cowboy_websocket, Req, State}.

websocket_handle(Data, Req, State) ->
    % Send message received from publisher to the server, with 'pub' atom prefix
    ok = gen_server:call(State#ws_pub.server, {send, Data}),
    {ok, Req, State}.

websocket_info(_Info, Req, State) ->
	{ok, Req, State}.

terminate(_Reason, _Req, State) ->
    try
        gen_server:call(State#ws_pub.server, 'topic going down', 1000)
    catch Error:Reason -> {Error, Reason}
    end,
    ok.
