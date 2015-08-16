-record(ws_pub, {server :: pid(),
                 topic :: binary()}).
-record(ws_sub, {topic :: binary()}).

-record(srv, {owner :: pid,
              topic :: binary()}).
