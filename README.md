# Riak PBC Pool

Riak PBC Pool is a fork of [emysql] (https://github.com/eonblast/emysql "emysql") for the Riak protocol buffer client.

## Overview
In the case of Emysql, each pool is given a unique identifier by the user. The Riak PBC pool functions differently in that it
each _pool_ consists of connections to one Riak server, so each pool can be referenced by using the Host and Port later.

Once a server has been added to the pool, a request to get a connection will be done in a round-robin fashion, instead of by 
specifying a pool to retrieve a connection from.

## Important Note:
**This code is not tested well; in fact, I just added code along with this README and comments update. Not sure if it all works.** 
**So, use at your own risk and I will up the version after I've tested/fixed. Whatever bugs you find, I would be glad to fix.**

### Connection locking
The Emysql application has a mechanism for locking. Once a connection has been retrieved from the pool, it locks the connection by
making it unavailable when retrieving a new connection. To make the connection available, it must be put back in the pool 
manually.

## Usage

### Starting

To start the pool, issue the command:

    riakpool:start()

This will start crypto, then start the riakpool application. If you have configured connections in the .app or the application
.config file, it will create pools for each server listed in the conns list. The first entry in the tuple is the host, the second
is the port, third is the number of connections to open and the final item in the tuple is a list of options to pass to 
riakc\_pb\_socket:start\_link().


### Adding pools

To add a new pool, call the function riakpool:add\_pool(host(), port()). You can also specify the size and options. Example:

    riakpool:add_pool("127.0.0.1", 8087, 5, [{auto_reconnect, false}]).

### Removing pools

If you would like to remove a pool, you can remove it by calling riakpool:remove\_pool(host(), port()).

    riakpool:remove_pool("127.0.0.1", 8087).

### Adding connections to pools

Because each _pool_ is one server configuration, if we want to add more connections to that pool, we cannot simply add another
named pool with the same connection information. So, we can increment the number of connections contained in one pool.

    riakpool:increment_pool_size("127.0.0.1", 8087, 5).

This will increment the pool for host="127.0.0.1" and port=8087 by five connections.

### Removing connections from pools

It is also possible to remove connections from the pool:

    riakpool:decrement_pool_size("127.0.0.1", 8087, 2).

This will remove two connections.

### Getting a connection

Currently, it is slightly inconvenient to get a connection. To get a connection, call riakpool:get\_conn(). This will return a 
connection record, which has the Riak connection pid. Example:

    Conn = riakpool:get_conn(),
    {ok, Obj} = riakc_pb_socket:get(Conn#connection.pid, <<"bucket">>, <<"key">>).

This is, however, pretty crappy because a record is returned, and not just the Pid. Look for this to change in the future.

### Returning a connection

Because we lock any connection that is being used, it's very important to return the connections back to the pool, or face
getting errors because there are no available connections. To return a connection to the pool:

    riakpool:return_conn(Connection).

### TODO:
(In no particular order)

* Make riakpool:get\_conn() return only the Pid
* Use Lager for logging statements
* ??
* Profit!
