# clj-crm

generated using Luminus version "3.10.35"

clj-crm is a Customer relationship management software, which the primary function is to distribute customers to sales and estimate the credit of each sales based on order data and customer-sales binding time.

## Prerequisites
In `infra` directory, there is an Ansible playbook to do the server provisition.


## REPL-driven development
To run the REPL with dev profile, run:

```
  lein repl
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
bin/datomic -Xmx1g -Xms1g backup-db datomic:free://localhost:4334/clj_crm file:/full/path/to/backup-directory
```
Note: Backup URIs are per database. You can backup the same database at different points in time to a single backup URI.

## Run a production server
To start a web server for the application, run:

```
  lein uberjar
  java -Dconf=env/prod/resources/config.edn -jar target/uberjar/clj-crm.jar 
```

## License

Copyright © 2019 Laurence Chen
