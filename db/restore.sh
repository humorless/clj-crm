#!/bin/bash

# setup the environment variable with absulute path
DB_DIR=/home1/irteamsu/db/datomic-free-0.9.5656/
from_db_uri=datomic:free://localhost:4334/clj_crm
to_backup_uri=file:/home1/irteamsu/etl/db_backup

$DB_DIR/bin/datomic -Xmx4g -Xms4g restore-db $to_backup_uri $from_db_uri
