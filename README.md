# clj-crm

generated using Luminus version "3.10.35"

FIXME

## Prerequisites

You will need [Leiningen][1] 2.0 or above installed.

[1]: https://github.com/technomancy/leiningen

## REPL-driven development
To run the REPL with dev profile, run:

```
  lein with-profile dev repl
```

To start the application state in `REPL`, run:

```
(start)
```
so as to invoke the `start` function defined in `env/dev/clj/user.clj`

## Prepare Datomic database through docker
To prepare the datomic-free database for this web application, run:

```
  mkdir datomic-data    # used to hold h2 db files
  docker run -d -p 4334-4336:4334-4336 -v $(pwd)/datomic-data:/data --name datomic-free akiel/datomic-free:0.9.5656
```

To login H2 database (datomic storage backend),
  1. Open web address at `localhost:4336`
  2. Put `jdbc:h2:/data/db/datomic` in JDBC URL
  3. User Name and Password defaults are `datomic`
  4. Login

## Database backup & restore
To backup database, run:

```
bin/datomic -Xmx1g -Xms1g backup-db datomic:free://localhost:4334/clj_crm_dev file:/full/path/to/backup-directory
```
Note: Backup URIs are per database. You can backup the same database at different points in time to a single backup URI.

## Running
To start a web server for the application, run:

```
  lein run
```

## License

Copyright © 2019 FIXME
