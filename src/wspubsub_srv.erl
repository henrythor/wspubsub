-module(wspubsub_srv).
-include("wspubsub.hrl").
-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

init(_Args) ->
    State = #hub{subs = []},
    {ok, State}.

handle_call(sub, From, State) ->
    Subs = State#hub.subs,
    NewState = State#hub{subs = Subs ++ From},
    {noreply, NewState};
handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
