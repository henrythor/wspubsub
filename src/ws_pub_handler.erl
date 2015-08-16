-module(ws_pub_handler).
-include("wspubsub.hrl").
-export([init/2
        ,websocket_handle/3
        ,websocket_info/3]).

-spec init(Req::cowboy_req:req(), Opts::any()) ->
    {cowboy_websocket, Req::cowboy_req:req(), State::#ws_pub{}}.
init(Req, _Opts) ->
    [Topic] = cowboy_req:path_info(Req),
    Args = [{owner, self()}, {topic, Topic}],
    {ok, Pid} = gen_server:start(wspubsub_srv, Args, []),
    State = #ws_pub{server = Pid, topic = Topic},
	{cowboy_websocket, Req, State}.

-spec websocket_handle(Data::any(), Req::cowboy_req:req(), State::#ws_sub{}) ->
    {ok, Req::cowboy_req:req(), State::#ws_sub{}}.
websocket_handle(Data, Req, State) ->
    % Send message received from publisher to the server, with 'pub' atom prefix
    State#ws_pub.server ! {pub, Data},
	{ok, Req, State}.

-spec websocket_info(Info::any(), Req::cowboy_req:req(), State::#ws_sub{}) ->
    {ok, Req::cowboy_req:req(), State::#ws_sub{}}.
websocket_info(_Info, Req, State) ->
	{ok, Req, State}.
