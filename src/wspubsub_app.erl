%%%-------------------------------------------------------------------
%% @doc wspubsub public API
%% @end
%%%-------------------------------------------------------------------

-module('wspubsub_app').

-behaviour(application).

%% Application callbacks
-export([start/2
        ,stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/ws/pub/[...]", ws_pub_handler, []},
            {"/ws/sub/[...]", ws_sub_handler, []}
        ]}
    ]),
    lager:info("wspubsub starting.."),
    {ok, _} = cowboy:start_http(http, 100, [{port, 3000}],
        [{env, [{dispatch, Dispatch}]}]),
    'wspubsub_sup':start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
