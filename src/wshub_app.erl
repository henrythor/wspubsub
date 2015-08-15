%%%-------------------------------------------------------------------
%% @doc wshub public API
%% @end
%%%-------------------------------------------------------------------

-module('wshub_app').

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
            {"/wshub", ws_handler, []}
        ]}
    ]),
    lager:info("wshub starting.."),
    {ok, _} = cowboy:start_http(http, 100, [{port, 3000}],
        [{env, [{dispatch, Dispatch}]}]),
    'wshub_sup':start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
