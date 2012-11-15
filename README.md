Sip-Router Erlang module
========================

This module let sip-router/kamailio connect and send messages to Erlang.

With this module You can:
* use erlang to implement business logic for Your sip-router/kamailio proxy
* use erlang as data processor/aggregator, for traffic statistics, tracing etc.
* use sip-router as sip stack for Your erlang application

Dependencies
------------
* Erlang. I have release R15B01.
  To compile module You need erl_interface library usualy packaged in erlang-dev.

Quickstart
----------

Start erlang node using `erl -setcookie cookie -sname test`
execute following on erlang console:
```erlang
register(console,self()).
dbg:tracer().
dbg:p(console,m).
dbg:p(rex,m).
```
Start Kamailio with example config,  use sipsak to send sip messages, and watch message trace on erlang console 

