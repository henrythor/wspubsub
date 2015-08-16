-module(ws_pub_handler).
-include("wspubsub.hrl").
-export([init/2
        ,websocket_handle/3
        ,websocket_info/3]).

init(Req, _Opts) ->
    [Topic] = cowboy_req:path_info(Req),
    Args = [{owner, self()}, {topic, Topic}],
    {ok, Pid} = gen_server:start(wspubsub_srv, Args, []),
    State = #ws_pub{server = Pid, topic = Topic},
	{cowboy_websocket, Req, State}.

websocket_handle(Data, Req, State) ->
    % Send message received from publisher to the server, with 'pub' atom prefix
    State#ws_pub.server ! {pub, Data},
	{ok, Req, State}.

websocket_info(_Info, Req, State) ->
	{ok, Req, State}.
