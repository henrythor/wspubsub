-module(ws_sub_handler).
-include("ws_sub_handler.hrl").
-export([init/2
        ,websocket_handle/3
        ,websocket_info/3
        ,terminate/3
        ]).

init(Req, _Opts) ->
    [Domain, Topic] = cowboy_req:path_info(Req),
    case global:whereis_name({srv, Domain, Topic}) of
        Server when is_pid(Server) ->
            ok = gen_server:call(Server, 'add subscriber'),
            State = #ws_sub{server = Server, domain = Domain, topic = Topic},
            {cowboy_websocket, Req, State};
        V ->
            lager:error("~s/~s/~s: Got ~p when trying to find server",
                [?MODULE, Domain, Topic, V]),
            Req2 = cowboy_req:reply(404, [], Req),
            {ok, Req2, undefined}
    end.

websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info({'topic going down', Pid}, Req, State) when Pid =:= State#ws_sub.server ->
    {stop, Req, State};
websocket_info({'topic message', Pid, Message}, Req, State) when Pid =:= State#ws_sub.server ->
    {reply, Message, Req, State};
websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

terminate(_Reason, _Req, State) ->
    ok.
