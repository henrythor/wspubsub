-module(ws_pub_handler).
-include("ws_pub_handler.hrl").
-export([init/2
        ,websocket_handle/3
        ,websocket_info/3
        ,terminate/3
        ]).

init(Req, _Opts) ->
    [ApiKey, Topic] = cowboy_req:path_info(Req),
    [Domain] = cowboy_req:host_info(Req),
    ServerPid = case global:whereis_name({srv, Domain, Topic}) of
        Pid when is_pid(Pid) ->
            Args = [{api_key, ApiKey}],
            ok = gen_server:call(Pid, {new_owner, Args}),
            Pid;
        _ ->
            Args = [{owner, self()}, {domain, Domain}, {topic, Topic},
                    {api_key, ApiKey}],
            {ok, Pid} = gen_server:start(wspubsub_srv, Args, []),
            Pid
    end,
    erlang:monitor(process, ServerPid),
    State = #ws_pub{server = ServerPid, domain = Domain, topic = Topic},
	{cowboy_websocket, Req, State}.

websocket_handle(Data, Req, State) ->
    % Send message received from publisher to the server, with 'pub' atom prefix
    ok = gen_server:call(State#ws_pub.server, {send, Data}),
    {ok, Req, State}.

websocket_info({'DOWN', _Ref, process, Pid, _Reason}, Req, State)
when Pid =:= State#ws_pub.server ->
    {stop, Req, State};
websocket_info(_Info, Req, State) ->
	{ok, Req, State}.

terminate(_Reason, _Req, State) when is_record(State, ws_pub) ->
    lager:debug("~s/~s/~s ~p going down",
        [?MODULE, State#ws_pub.domain, State#ws_pub.topic, self()]),
    ok;
terminate(_Reason, _Req, _State) ->
    ok.
