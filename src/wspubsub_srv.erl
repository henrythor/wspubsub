-module(wspubsub_srv).
-include("wspubsub_srv.hrl").
-behavior(gen_server).
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

init(Args) ->
    Domain = proplists:get_value(domain, Args),
    Topic = proplists:get_value(topic, Args),
    Owner = proplists:get_value(owner, Args),
    ApiKey = proplists:get_value(api_key, Args),
    % Make sure namespaces are locked by shared secrets (API keys)
    case {allowed, apikeys:verify(ApiKey, Domain)} of
        {allowed, true} ->
            case {reg, global:register_name({srv, Domain, Topic}, self())} of
                {reg, yes} ->
                    State = #srv{owner = Owner
                                ,domain = Domain
                                ,topic = Topic
                                ,subs = []
                                },
                    lager:info("~s: Created new topic ~s/~s",
                        [?MODULE, Domain, Topic]),
                    erlang:monitor(process, Owner),
                    {ok, State};
                {reg, no} ->
                    {stop, 'topic already exists', null}
            end;
        {allowed, false} ->
            {stop, 'permission denied'}
    end.

handle_call(subscribe, {Pid, _T}, State) ->
    case lists:member(Pid, State#srv.subs) of
        true ->
            lager:error("~s/~s/~s: ~p subscribing, but already is subscribed",
                [?MODULE, State#srv.domain, State#srv.topic, Pid]),
            {reply, 'already a sub', State};
        false ->
            lager:debug("~s/~s/~s: ~p subscribing",
                [?MODULE, State#srv.domain, State#srv.topic, Pid]),
            erlang:monitor(process, Pid),
            lager:debug("~s/~s/~s: subs before: ~p",
                [?MODULE, State#srv.domain, State#srv.topic, State#srv.subs]),
            NewSubs = State#srv.subs ++ [Pid],
            NewState = State#srv{subs = NewSubs},
            lager:debug("~s/~s/~s: subs after: ~p",
                [?MODULE, State#srv.domain, State#srv.topic, NewSubs]),
            {reply, ok, NewState}
    end;
handle_call({send, Message}, {Pid, _T}, State) when Pid =:= State#srv.owner ->
    lager:debug("~s/~s/~s: sending ~p to all subs",
        [?MODULE, State#srv.domain, State#srv.topic, Message]),
    send_to_all(State#srv.subs, {'topic message', self(), Message}),
    {reply, ok, State};
handle_call(Request, From, State) ->
    lager:error("~s/~s/~s: Ignored call to ~p from ~p",
        [?MODULE, State#srv.domain, State#srv.topic, Request, From]),
    {reply, ignored, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State)
when Pid =:= State#srv.owner ->
    lager:info("~s/~s/~s: Owner dying, killing topic",
        [?MODULE, State#srv.domain, State#srv.topic]),
    {stop, normal, State};
handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    case lists:member(Pid, State#srv.subs) of
        true ->
            lager:debug("~s/~s/~s: Removing ~p from subs",
                [?MODULE, State#srv.domain, State#srv.topic, Pid]),
            lager:debug("~s/~s/~s: subs before: ~p",
                [?MODULE, State#srv.domain, State#srv.topic, State#srv.subs]),
            NewSubs = State#srv.subs -- [Pid],
            NewState = State#srv{subs = NewSubs},
            lager:debug("~s/~s/~s: subs after: ~p",
                [?MODULE, State#srv.domain, State#srv.topic, NewSubs]),
            {noreply, NewState};
        false ->
            {noreply, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, State) ->
    % From 'global' manual: If a process with a registered name dies, or the
    % node goes down, the name is unregistered on all nodes.
    lager:info("~s/~s/~s: dying, ~p",
        [?MODULE, State#srv.domain, State#srv.topic, Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% Private functions
send_to_all([], _Message) ->
    ok;
send_to_all([Member|Members], Message) when is_pid(Member) ->
    Member ! Message,
    send_to_all(Members, Message).
