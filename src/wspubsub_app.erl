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
    TransOpts = application:get_env(wspubsub, cowboy_trans, [{port, 3000}]),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/ws/pub/[...]", ws_pub_handler, []},
            {"/ws/sub/[...]", ws_sub_handler, []}
        ]}
    ]),
    lager:info("wspubsub starting.."),
    lager:debug("transopts: ~p", [TransOpts]),
    {ok, _} = cowboy:start_http(http, 100, TransOpts,
        [{env, [{dispatch, Dispatch}]}]),
    'wspubsub_sup':start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
