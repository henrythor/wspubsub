# wspubsub

WebSocket pub/sub hub written in Erlang, based on Cowboy

Basic workings
--------------
* Clients connect via /wspubsub
* Clients start (pub) or join (sub) a channel
* Starting a channel launches a wspubsub_srv process for each channel, referenced
by each connection as 'hub' in their respective states.
* Each wspubsub_srv has a list of clients in their state.
* Each WebSocket connection to /wspubsub may only be a member of 1 channel, but
clients may have multiple Websocket connections if they want.
* When a client sends a message to a channel, the receiving ws_handler forwards
it to its hub, which in turn messages the ws_handler for each member of the
channel.
* ws_handler processes upon receiving such a message will return it as a reply,
resulting in it being passed on via the WebSocket connection.

Build
-----

    $ ./rebar3 release
