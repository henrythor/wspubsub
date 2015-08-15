-record(ws, {hub :: pid()}).

-record(hub, {owner :: pid(), members :: list()}).
