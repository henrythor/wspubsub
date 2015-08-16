-record(srv, {owner :: pid,
              topic :: binary()}).
-spec wspubsub_srv:init(Args::list()) -> {ok, State::#srv{}}.
-spec wspubsub_srv:handle_call(Request::tuple(), From::tuple(), State::#srv{}) ->
    {noreply, NewState::#srv{}}.
-spec wspubsub_srv:handle_cast(Request::any(), State::#srv{}) -> {noreply, NewState::#srv{}}.
-spec wspubsub_srv:handle_info(Info::any(), State::#srv{}) -> {noreply, NewState::#srv{}}.
-spec wspubsub_srv:terminate(Reason::any(), State::#srv{}) -> ok.
-spec wspubsub_srv:code_change(OldVsn::any(), State::#srv{}, Extra::any()) ->
    {ok, State::#srv{}}.
-spec wspubsub_srv:send_to_all(Members::list(), Message::any()) -> ok.
