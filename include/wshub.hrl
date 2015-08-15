-record(ws, {hub :: pid()}).

-record(hub, {pub :: pid(), subs :: list()}).
