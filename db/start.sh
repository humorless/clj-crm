#!/usr/bin/env bash

DATOMIC_VERSION=0.9.5656

cd datomic-free-${DATOMIC_VERSION}

sed "/host=0.0.0.0/a alt-host=${ALT_HOST:-127.0.0.1}" -i transactor.properties

bin/transactor transactor.properties
