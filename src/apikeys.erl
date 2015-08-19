-module(apikeys).
-export([verify/2]).

verify(ApiKey, Domain) ->
    Q = "SELECT domain FROM apikeys WHERE apikey=$1 AND domain=$2 AND active=true",
    lager:debug("~s: Running query: query(\"~s\", [~p,~p])",
        [?MODULE, Q, ApiKey, Domain]),
    case pgclient:query(Q, [ApiKey, Domain]) of
        {{select,1},[{Domain}]} -> true;
        {{select,0},[]} -> false;
        Result ->
            lager:error("~s: query(\"~s\", [~p,~p]) resulted in error: ~p",
                [?MODULE, Q, ApiKey, Domain, Result]),
            false
    end.
