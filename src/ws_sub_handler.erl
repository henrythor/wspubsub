-module(ws_sub_handler).
-include("wspubsub.hrl").
-export([init/2
        ,websocket_handle/3
        ,websocket_info/3
        ,terminate/3]).

-spec init(Req::cowboy_req:req(), Opts::any()) ->
    {cowboy_websocket, NewReq::cowboy_req:req(), State::#ws_sub{}}.
init(Req, _Opts) ->
    [Topic] = cowboy_req:path_info(Req),
    case pg2:get_members({srv, Topic}) of
        [_Server] ->
            pg2:join({subs, Topic}, self()),
            State = #ws_sub{topic = Topic},
            {cowboy_websocket, Req, State}
    end.

-spec websocket_handle(Data::any, Req::cowboy_req:req(), State::#ws_sub{}) ->
    {ok, Req::cowboy_req:req(), NewState::#ws_sub{}}.
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

-spec websocket_info(Info::any(), Req::cowboy_req:req(), State::#ws_sub{}) ->
    {ok, Req::cowboy_req:req(), NewState::#ws_sub{}}.
websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

-spec terminate(Reason::any(), Req::cowboy_req:req(), State::#ws_sub{}) -> ok.
terminate(_Reason, _Req, _State) ->
    ok.
