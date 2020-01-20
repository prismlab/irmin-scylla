# Installation
Working with Irmin-scylla requires two installations:
- Installing scylla driver to communicate with OCaml programs
- Installing the scylla backend for the Irmin.

### Installing Scylla driver
Scylla DB is just a C++ implementation of Cassandra DB. So the drivers used for C* works for Scylla too, without any changes. We have used the C++ implementation of Datastax Cassandra driver which can be downloaded from:
https://github.com/datastax/cpp-driver
Instructions to build it can be found at: https://datastax.github.io/cpp-driver/topics/building/

To use the C* driver with OCaml, we can add it to opam libraries as an external stub.
Dune and other files for the same can be found at: <git hub link>
The folder structure in the git repo is designed to accomodate the correct placing of the libraries. 
In my setup, I have named the directory containing the driver and other related files as `libcassandra` . It can be added to opam using:
`opam pin add libcassandra -k path < path to root folder>`

### Installing the Scylla backend
Installing the Scylla backend is quite straight forward, just like any other backend. Since the backend is supposed to interact with the external driver which is written in C++, we have a C stub to convert some of the basic types between OCaml and C/C++. Dune is modified to install this file along with few library dependencies which are required to connect with the driver. These are system based libraries.
Irmin-scylla can be installed using:
`opam pin add irmin-scylla -k path < path to root folder>`

### Setting up the scylla database
Currently I am using the containerized version of the scylla database, for which instructions can be found at: https://docs.scylladb.com/operating-scylla/procedures/tips/best_practices_scylla_on_docker/
Alternatively, it can be installed by compiling the source present at its github repo. 
According to my observation, two containers on the same machine automatically forms the cluster. Further setup would be required to set it up over different machines, whose instructions are also given at above link.

### Examples
This repository also contains a simple example for setting a key-value pair to database in one node and fetching it back from another. 
I have also checked in one piece of code which I run on `utop` for testing purpose, just in case it comes handy.


### Note
- Since the stores are implicit, here I am not asking user to provide any names for the keyspace and tables. The code assumes the following configuration for the datastore:
```
CREATE KEYSPACE irmin_scylla WITH replication = {'class': 'SimpleStrategy', 'replication_factor': '3'};

CREATE TABLE irmin_scylla.atomic_write (
    key text PRIMARY KEY,
    value text);

CREATE TABLE irmin_scylla.append_only (
    key text PRIMARY KEY,
    value blob);
```

- Scylla as of now in its docker version does not support Light weight transactions. The original backend was written keeping Cassandra in mind, hence the implementation of test-and-set function uses Light weight transactions. Hence the second insertion on the same branch would throw an error from the database.  Small modificaition in the query can be fix the issue. 
The trunk version of Scylla has implemented the Light weight transactions, hence this code could come handy if that code is used or if the C* is used instead of Scylla. Hence the modification is not made in the original code. 
