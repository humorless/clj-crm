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

## Running
To prepare the datomic-free database for this web application, run:

```
  mkdir datomic-data    # used to hold h2 db files
  docker run -d -p 4334-4336:4334-4336 -v $(pwd)/datomic-data:/data --name datomic-free akiel/datomic-free:0.9.5656
```

To start a web server for the application, run:

```
  lein run
```

## License

Copyright © 2019 FIXME
