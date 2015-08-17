-module(apikeys).
-export([verify_new_topic/3]).

verify_new_topic(ApiKey, Domain, _Topic) ->
    if
        ApiKey =:= <<"f47ac10b-58cc-4372-a567-0e02b2c3d479">>,
        Domain =:= <<"test">> ->
            true;
        true -> % else..
            false
    end.
