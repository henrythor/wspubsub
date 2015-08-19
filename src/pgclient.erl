-module(pgclient).
-export([get_connection/1, return_connection/2, open/1, close/1]).
-export([query/1, query/2]).

% Taken from Tristan Sloughter's blogpost
% http://blog.erlware.org/erlang-postgres-connection-pool-with-episcina/

-spec get_connection(atom()) -> {pgsql_connection, pid()} | {error, timeout}.
get_connection(Pool) ->
    case episcina:get_connection(Pool) of
        {ok, Pid} ->
            {pgsql_connection, Pid};
        {error, timeout} ->
            {error, timeout}
    end.

-spec return_connection(atom(), {pgsql_connection, pid()}) -> ok.
return_connection(Pool, {pgsql_connection, Pid}) ->
    episcina:return_connection(Pool, Pid).

-spec open(list()) -> {ok, pid()}.
open(DBArgs) ->
    {pgsql_connection, Pid} = pgsql_connection:open(DBArgs),
    {ok, Pid}.

-spec close(pid()) -> ok.
close(Pid) ->
    pgsql_connection:close({pgsql_connection, Pid}).

-spec query(string()) -> tuple().
query(Query) ->
    C = get_connection(primary),
    try
        pgsql_connection:simple_query(Query, [], infinity, C)
    after
        return_connection(primary, C)
    end.

-spec query(string(), list()) -> tuple().
query(Query, Params) ->
    C = get_connection(primary),
    try
        pgsql_connection:extended_query(Query, Params, [], infinity, C)
    after
        return_connection(primary, C)
    end.
