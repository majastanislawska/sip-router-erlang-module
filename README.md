Sip-Router Erlang module
========================

This module let sip-router/kamailio connect and send messages to Erlang.

With this module You can:
* use erlang to implement business logic for Your sip-router/kamailio proxy
* use erlang as data processor/aggregator, for traffic statistics, tracing etc.
* use sip-router as sip stack for Your erlang application

Dependencies
------------
* Erlang. I have release R16B01.
  To compile module You need erl_interface library usualy packaged in erlang-dev.

Quickstart
----------

Go to erlang directory and do `make`. This will download rebar, then some addidional
erlang applications to deps folder and then compile sample app.
Start it with `./start.sh`

Start Kamailio with example config in aother console and  use sipsak to send sip messages
and watch log on erlang console.
```sh
sipsak -v -s sip:echo@localhost
sipsak -v -s sip:rpc@localhost
sipsak -v -s sip:rpc2@localhost
sipsak -v -s sip:whatever@localhost
```
Sample erlang application is just simple echoing gen_server.
Logs on console and in log files are done by `lager`,

