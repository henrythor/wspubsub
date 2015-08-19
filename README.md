# wspubsub

WebSocket pub/sub hub written in Erlang, based on Cowboy

Basic workings
--------------
* Clients connect via /ws/pub and /ws/sub
* Publishers use /ws/pub/ApiKey/Domain/Topic
* Subscribers use /ws/sub/Domain/Topic
* Clients start (pub) or join (sub) a topic
* Starting a topic launches a wspubsub_srv process for each topic,
referenced by each connection as 'server' in their respective states.
* Each wspubsub_srv has a list of clients in their state.
* Each WebSocket connection may only be a member of 1 topic, but clients may
have multiple Websocket connections if they want.
* When a publisher sends a message to a topic, the receiving ws_pub_handler
forwards it to its server, which in turn messages the ws_sub_handler for each
member of the topic.
* ws_handler processes receiving such a message will return it as a reply,
resulting in it being passed on via the WebSocket connection.

Build
-----
    `$ ./rebar3 tar`
    Then copy the resulting tarball to server, unpack it, and run
    `./bin/wspubsub start` to start it and `./bin/wspubsub attach` to attach a
    shell.

Database setup
--------------
For the project I've designed this code for we use Postgres to store apikeys.
You'll need to edit the episcina section of sys.config to reflect your own
setup. Here's the DDL:
```
create table "apikeys" (
    "id" integer not null default nextval('apikeys_id_seq'::regclass),
    "domain" varchar(64) not null,
    "username" varchar(254) not null,
    "apikey" varchar(36) not null,
    "insert_ts" timestamp not null default now(),
    "active" boolean not null default false,
    primary key ("id")
);
create unique index "apikeys_pkey" on "apikeys" ("id");
create unique index "unique_id" on "apikeys" ("id");
create unique index "unique_domain" on "apikeys" ("domain");
create unique index "unique_apikey" on "apikeys" ("apikey");
```
To insert an apikey for testing do:
```
insert into apikeys (domain, username, apikey) values('test', 'hello@world.com', 'f47ac10b-58cc-4372-a567-0e02b2c3d479');
```
