#!/usr/bin/env bash

DATOMIC_VERSION=0.9.5656

cd datomic-free-${DATOMIC_VERSION}

bin/transactor transactor.properties
