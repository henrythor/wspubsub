-module(ws_handler).
-include("wspubsub.hrl").
-export([init/2]).
-export([websocket_handle/3]).
-export([websocket_info/3]).

init(Req, _Opts) ->
    State = #ws{hub = undefined},
	{cowboy_websocket, Req, State}.

websocket_handle(_Data, Req, State) ->
	{ok, Req, State}.

websocket_info(_Info, Req, State) ->
	{ok, Req, State}.
