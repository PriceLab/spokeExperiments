# data comes from github
# cypher load script found in the docker volume mounted from ../data/import
default:
	@echo targets: start, stop, remove, cypher, bash, clear, load, count

start:
	bash runDocker.sh

cypher:
	bash runCypher.sh

bash:
	docker exec --interactive --tty transport2 /bin/bash

clear:
	bash clearDataBase.sh

load:
	bash ./loadCSV.sh

count:
	bash getNodeCount.sh

stop:
	- docker stop transport2

remove: stop
	- docker rm transport2

test:
	R -f test.R

