-module(apikeys_tests).
-include_lib("eunit/include/eunit.hrl").

verify_test() ->
    ?assert(apikeys:verify(<<"f47ac10b-58cc-4372-a567-0e02b2c3d479">>,<<"test">>)),
    ?assertNot(apikeys:verify(<<"f47ac10b-58cc-4372-a567-0e02b2c3d479">>,<<"nontest">>)).
