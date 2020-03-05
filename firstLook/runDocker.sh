#!/bin/bash
NAME=firstlook
#docker stop $NAME
#docker rm $NAME

# the data directory must contain two neo4j-specific subdirectories: databases and dbms
# when used here, docker apparewntly adds attributes to the $DATA directory
# i had thought this was a problem, but it is not

DATA=/Users/paul/github/spokeExperiments/firstLook/data

docker run --name=$NAME \
    --publish=7474:7474 --publish=7687:7687 \
    --volume=$DATA:/data \
    --volume=/Users/paul/github/spokeExperiments/firstLook/logs:/logs \
    --user=neo4j \
    --env NEO4J_AUTH=neo4j/spoke@isb \
    --env 'NEO4JLABS_PLUGINS=["apoc", "graph-algorithms"]' \
    --env NEO4J_dbms_security_procedures_unrestricted=apoc.\\\*,algo.\\\* \
    neo4j:3.5.12
