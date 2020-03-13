docker exec --interactive --tty transport2 \
       /bin/bash -c 'cat /var/lib/neo4j/import/loadTransport.cypher |  ./bin/cypher-shell -u neo4j -p hoopa'

