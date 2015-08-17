-record(srv, {owner::pid()
             ,domain::binary()
             ,topic::binary()
             ,subs::list()
             }).
-spec wspubsub_srv:init(Args::list()) -> {ok, State::#srv{}}.
-spec wspubsub_srv:handle_call(Request::tuple(), From::tuple(), State::#srv{}) ->
    {reply, Reply::atom(), NewState::#srv{}}.
-spec wspubsub_srv:handle_cast(Request::any(), State::#srv{}) -> {noreply, NewState::#srv{}}.
-spec wspubsub_srv:handle_info(Info::any(), State::#srv{}) -> {noreply, NewState::#srv{}}.
-spec wspubsub_srv:terminate(Reason::any(), State::#srv{}) -> ok.
-spec wspubsub_srv:code_change(OldVsn::any(), State::#srv{}, Extra::any()) ->
    {ok, State::#srv{}}.
-spec wspubsub_srv:send_to_all(Members::list(), Message::any()) -> ok.
