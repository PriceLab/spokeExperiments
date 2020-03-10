
docker:
	bash ./runDocker.sh

cypher:
	bash ./runCypher.sh

bash:
	docker exec --interactive --tty spokelearn /bin/bash

load.northwind:
	bash ./runCypherLoadCSV.sh


test: testSimple testNorthwind

testSimple:
	R -f test_simple.R

testNorthwind: load.northwind
	R -f test_northwindGraph.R

