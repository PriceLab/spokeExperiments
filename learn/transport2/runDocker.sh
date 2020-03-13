#!/bin/bash

NAME=transport2
# docker stop $NAME
# docker rm $NAME

# the data directory must contain two neo4j-specific subdirectories: databases and dbms
# when used here, docker apparewntly adds attributes to the $DATA directory
# i had thought this was a problem, but it is not
#
# <neo4j-home>/data: this is where the database datastore is kept
# csv files to import must be in <neo4j-home>/import
# where the docker image has NEO4J_HOME
#   NEO4J_HOME=/var/lib/neo4j
#

docker run --name=$NAME \
    --detach \
    --publish=7476:7474 --publish=7689:7687 \
    --volume=/Users/paul/github/spokeExperiments/learn/transport2/import:/var/lib/neo4j/import \
    --volume=/Users/paul/github/spokeExperiments/learn/transport2/data:/data \
    --volume=/Users/paul/github/spokeExperiments/learn/transport2/logs:/logs \
    --user=neo4j \
    --env NEO4J_AUTH=neo4j/hoopa \
    --env 'NEO4JLABS_PLUGINS=["apoc", "graph-algorithms"]' \
    --env NEO4J_dbms_security_procedures_unrestricted=apoc.\\\*,algo.\\\* \
    neo4j:3.5.12
