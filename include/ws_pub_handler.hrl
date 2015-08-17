-record(ws_pub, {server::pid()
                ,topic::binary()
                }).

-spec ws_pub_handler:init(Req::cowboy_req:req(), Opts::any()) ->
    {cowboy_websocket, Req::cowboy_req:req(), State::#ws_pub{}}.
-spec ws_pub_handler:websocket_handle(Data::any(), Req::cowboy_req:req(), State::#ws_pub{}) ->
    {ok, Req::cowboy_req:req(), State::#ws_pub{}}.
-spec ws_pub_handler:websocket_info(Info::any(), Req::cowboy_req:req(), State::#ws_pub{}) ->
    {ok, Req::cowboy_req:req(), State::#ws_pub{}}.
