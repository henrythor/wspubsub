-record(ws, {server :: pid()}).

-record(hub, {pub :: pid(), subs :: list()}).
