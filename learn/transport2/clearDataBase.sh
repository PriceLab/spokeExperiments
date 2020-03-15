docker exec --interactive --tty transport2 \
       /bin/bash -c 'cat /var/lib/neo4j/import/clearDataBase.cypher |  ./bin/cypher-shell -u neo4j -p hoopa'

