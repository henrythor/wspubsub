-module(wspubsub_srv).
-include("wspubsub.hrl").
-behavior(gen_server).
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3]).

-spec init(Args::list()) -> {ok, State::#srv{}}.
init(Args) ->
    Topic = proplists:get_value(topic, Args),
    Owner = proplists:get_value(owner, Args),
    pg2:create({srv, Topic}),
    pg2:join({srv, Topic}, self()),
    pg2:create({subs, Topic}),
    State = #srv{owner = Owner, topic = Topic},
    {ok, State}.

-spec handle_call(Request::tuple(), From::tuple(), State::#srv{}) ->
    {noreply, NewState::#srv{}}.
handle_call({pub, Message}, {Pid, _Tag}, State) ->
    case State#srv.owner of
        Pid ->
            Subs = pg2:get_members({subs, State#srv.topic}),
            send_to_all(Subs, Message)
    end,
    {noreply, State};
handle_call(_Request, _From, State) ->
    {noreply, State}.

-spec handle_cast(Request::any(), State::#srv{}) -> {noreply, NewState::#srv{}}.
handle_cast(_Request, State) ->
    {noreply, State}.

-spec handle_info(Info::any(), State::#srv{}) -> {noreply, NewState::#srv{}}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(Reason::any(), State::#srv{}) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(OldVsn::any(), State::#srv{}, Extra::any()) ->
    {ok, State::#srv{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% Private functions
-spec send_to_all(Members::list(), Message::any()) -> ok.
send_to_all([], _Message) ->
    ok;
send_to_all([Member|Members], Message) when is_pid(Member) ->
    Member ! Message,
    send_to_all(Members, Message).
