#!/bin/bash

# init the directory
mkdir -p ~/backend/log

# kill the same service which is already running
kill -9 $(ps -aux | grep clj-crm.jar | grep -v grep | awk '{print $2}')

# deploy to the ~/backend dir
cp target/uberjar/clj-crm.jar  ~/backend/clj-crm.jar
cp env/prod/resources/config.edn ~/backend/config.edn

# start the service
tmux new-session -d -s backend 'cd ~/backend && java -Dconf=~/backend/config.edn -jar ~/backend/clj-crm.jar'

exit 0
