#!/bin/bash

docker run --name=transport2 \
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
