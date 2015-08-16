-module(ws_sub_handler).
-include("ws_sub_handler.hrl").
-export([init/2
        ,websocket_handle/3
        ,websocket_info/3
        ,terminate/3]).

init(Req, _Opts) ->
    [Topic] = cowboy_req:path_info(Req),
    case pg2:get_members({srv, Topic}) of
        [_Server] ->
            pg2:join({subs, Topic}, self()),
            State = #ws_sub{topic = Topic},
            {cowboy_websocket, Req, State}
    end.

websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info(Info, Req, State) ->
    {reply, Info, Req, State}.

terminate(_Reason, _Req, _State) ->
    ok.