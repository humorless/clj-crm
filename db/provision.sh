#!/usr/bin/env bash

DATOMIC_VERSION=0.9.5656

curl -L --progress-bar  -o  /tmp/datomic.zip "https://my.datomic.com/downloads/free/${DATOMIC_VERSION}"

unzip /tmp/datomic.zip && rm /tmp/datomic.zip

cd datomic-free-${DATOMIC_VERSION}

cp config/samples/free-transactor-template.properties transactor.properties

sed "s/host=localhost/host=0.0.0.0/" -i transactor.properties
sed "/host=0.0.0.0/a alt-host=${ALT_HOST:-127.0.0.1}" -i transactor.properties

mkdir ./data
sed "s/# data-dir=data/data-dir=.\/data/" -i transactor.properties

mkdir ./log
sed "s/# log-dir=log/log-dir=.\/log/" -i transactor.properties

