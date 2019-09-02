#!/usr/bin/env bash

DATOMIC_VERSION=0.9.5656

cd datomic-free-${DATOMIC_VERSION}

bin/transactor -Xmx4g -Xms4g transactor.properties
