-record(ws_sub, {server::pid()
                ,topic::binary()
                }).

-spec ws_sub_handler:init(Req::cowboy_req:req(), Opts::any()) ->
    {cowboy_websocket, NewReq::cowboy_req:req(), State::#ws_sub{}}.

-spec ws_sub_handler:websocket_handle(Data::any, Req::cowboy_req:req(), State::#ws_sub{}) ->
    {ok, Req::cowboy_req:req(), NewState::#ws_sub{}}.
-spec ws_sub_handler:websocket_info(Info::any(), Req::cowboy_req:req(), State::#ws_sub{}) ->
    {ok, Req::cowboy_req:req(), NewState::#ws_sub{}}.
-spec ws_sub_handler:terminate(Reason::any(), Req::cowboy_req:req(), State::#ws_sub{}) -> ok.
