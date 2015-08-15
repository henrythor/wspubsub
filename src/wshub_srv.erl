-module(wshub_srv).
-include("wshub.hrl").
-behavior(gen_server).

init(_Args) ->
    State = #hub{members = []},
    {ok, State}.

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
