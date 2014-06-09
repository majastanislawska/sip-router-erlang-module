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

Modules
-------

#### erlang ####
This module does all communication with erlang VM, (epmd actually) it forks a separate process
which collects requests from worker process, and dispatch replies to them. This is done using shared memory
and locking (worker proces - the one which is handling sip message, builds a message struct in shared memory
and locks itself (when is expecting a reply) after passing a pointer to elrang interface thread. That one sends 
message to erlang and when reply arrives looks up original request struct, fills it with reply and unlocks worker.
Worker than parses reply and continues script execution.

Spawning erlang interface process is currently done very ugly in mod_init not in child_init, because of db_erlang 
module. Some other modules perform a database setup or checks (usually a select on versions table) in mod_init.
To have this working, erlang process should be already running and connected.
It would be good to have erlang module listed first in config file, so it could be executed first.

This module provides several functions - some are for generic message passing some are interfaces to 
gen_server type process in erlang VM. Curenlty only sending messages to registered processes is implemented.
Please refer to doc for details.

#### db_erlang ####
This module provides database interface for orher modules. You may use it by setting a db_url
modparam for a specific module to 'erlang://connection/process' where connection is a name atribute 
from connection modparam of erlang module and process is an erlang gen_server responible to send back results of
Your queries. Sample erlang application db_mock is in the erlang directory. This app provides permanent storage based on DETS.
(in one table/file, but splitting to mltiple should not be that hard)

Makefile in erlang dir will prepare an erlang module and header files containing database schema, this is produced 
xsltproc and .xsl file int that directory.

Note that this module is a work in progress and it is not intended to compete with pure database modules, but there
are several usecases when it may be handy.


Quickstart
----------

Go to erlang directory and do `make`. This will download rebar, then some addidional
erlang applications to deps folder and then compile sample app.
Start it with `./start.sh`

Start Kamailio with example config in another console and use sipsak to send sip messages
and watch log on erlang console.
```sh
sipsak -v -s sip:echo@localhost
sipsak -v -s sip:rpc@localhost
sipsak -v -s sip:rpc2@localhost
sipsak -v -s sip:whatever@localhost
```
Sample erlang application is just simple echoing gen_server.
Logs on console and in log files are done by `lager`,

Install
-------

To copile and install create symlinks in Your kamailio/sip-router modules directory named 'erlang' pointing to topdir of this repo  and 'db_erlang' pointing to db_erlang subdir. You also need to enable those modules in kamailio Makefile config.
Contents of erlang dir you can put wherever You like but KAMAILIO_HOME variable in erlang/Makefile must be changed to point topdir of kamailio source tree.

TODO
----
On my to to list there are:
* xsl file to produce model files for boss_db (http://github.com/ChicagoBoss/boss_db). Done.
* some sample database app in erlang using boss_db as middleware for backends in mnesia, riak, mysql postgres and, hopefully soon,couchdb. Done.
* some more inteligent startup procedure (currently sleep in main proces is used to ensure erlang process is up and running)
* srdb2 support - needed to general compliance of db_erlang module, afaik, only one module uses this currently.
* better failure handling, like disconnects, reconnection to erlang.
* RPC interface - so one can query sip-router/kamailio status from erlang side.
* some @select or pseudovariables which could parse erlang terms and provide some handy interface in script. Something that could address specific element in tuple, or get a list or proplist 
* some more specialised functions based on erlang_call, something like erlang_call_route but in.ex. adding AVP's or seting scriptvars from some proplist returned from erlang
