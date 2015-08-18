# wspubsub

WebSocket pub/sub hub written in Erlang, based on Cowboy

Basic workings
--------------
* Clients connect via /ws/pub and /ws/sub
* Publishers use /ws/pub/<ApiKey>/<Domain>/<Topic>
* Subscribers use /ws/sub/<Domain>/<Topic>
* Clients start (pub) or join (sub) a topic
* Starting a topic launches a wspubsub_srv process for each topic,
referenced by each connection as 'server' in their respective states.
* Each wspubsub_srv has a list of clients in their state.
* Each WebSocket connection may only be a member of 1 topic, but clients may
have multiple Websocket connections if they want.
* When a publisher sends a message to a topic, the receiving ws_handler
forwards it to its server, which in turn messages the ws_handler for each member
of the topic.
* ws_handler processes receiving such a message will return it as a reply,
resulting in it being passed on via the WebSocket connection.

Build
-----

    $ ./rebar3 release
