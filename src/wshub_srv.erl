-module(wshub_srv).
-include("wshub.hrl").
-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

init(_Args) ->
    State = #hub{members = []},
    {ok, State}.

handle_call(join, From, State) ->
    Members = State#hub.members,
    NewState = State#hub{members = Members ++ From},
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
