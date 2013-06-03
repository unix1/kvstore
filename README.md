kvstore
=======

kvstore is a key value store implemented as a wrapper around Mnesia. Its distinctive feature is tracking access times.

Summary
-------

kvstore uses Mnesia to store data. It defines the following fields:

* key
* time_created
* time_modified
* time_accessed
* value

Access times are tracked in time_accessed field.

Installation
------------

The obvious preprequisite for kvstore is the Erlang runtime. To install:

* clone this project and cd into its directory
* compile

      `erl -make`

* to install with 2 nodes called kv1 and kv2 start Erlang shell in 2 different shells

      `shell1> erl -pa ebin/ -sname kv1@localhost`

      `shell2> erl -pa ebin/ -sname kv2@localhost`

* link kv2 to kv1 - i.e. in kv2 shell type

      `net_kernel:connect_node('kv1@localhost').`

* install on all linked nodes - from either node run

      `kvstore:install_all_nodes().`

* installation is now complete

Tests
-----

To run tests, change into the project directory and run:
      `ct_run -pa ebin/ -spec kvstore.spec`

Run
---

To run the kvstore server:

* start the Erlang shell on each host or Mnesia instance

      `shell1> erl -pa ebin/ -sname kv1@localhost`

      `shell2> erl -pa ebin/ -sname kv2@localhost`

* connect each instance - i.e. on all instances except the first:

      `net_kernel:connect_node('kv1@localhost').

* start applications on all instances:

      `application:start(mnesia).`

      `application:start(kvstore).`

* to use server from shell, start an agent gen_server first:

      `kvstore:start_server(kv_t1).`

* now you can use this agent to perform any key/value operations

Basic Usage
-----------

After starting the server as above, you can run commands from shell to operate on the data:

* to write data

      `kvstore:write(kv_t1, "key-1", "value-1").`

* to read data by key

      `kvstore:read(kv_t1, "key-1").`

* to delete data by key

      `kvstore:delete(kv_t1).`

Advanced Usage
--------------

Coming soon...