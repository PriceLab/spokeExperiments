# data comes from github
# cypher load script found in the docker volume mounted from ../data/import

docker:
	bash runDocker.sh

cypher:
	bash runCypher.sh

bash:
	docker exec --interactive --tty transport2 /bin/bash

load:
	bash ./loadCSV.sh

count:
	bash getNodeCount.sh

test:
	R -f test.R

