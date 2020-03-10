library(neo4r)
library(RUnit)
#------------------------------------------------------------------------------------------------------------------------
source("learn.R")
#------------------------------------------------------------------------------------------------------------------------
runTests <- function()
{
   clear.db()
   test_query()
   test_runCypherFile()
   test_getNodeLabels()
   test_getEdgeTypes()
   test_getNodeTable()
   test_getEdgeTable()

} # runTests
#------------------------------------------------------------------------------------------------------------------------
test_query <- function()
{
   printf("--- test_query")
   tbl.out <- query("match(n) return count(n)")
   checkTrue(tbl.out$value >= 0)

} # test_query
#------------------------------------------------------------------------------------------------------------------------
test_runCypherFile <- function()
{
  printf("--- test_runCypherFile")

  clear.db()
  checkEquals(nodeCount(), 0)
  checkEquals(edgeCount(), 0)

  runCypherFile("createTwoActors.cypher")
  checkEquals(nodeCount(), 4)
  checkEquals(edgeCount(), 3)

} # test_runCypherFile
#------------------------------------------------------------------------------------------------------------------------
test_exampleGraph_3.3.1 <- function()
{
  printf("--- test_exampleGraph_3.3.1")
  clear.db()
  runCypherFile("exampleGraph-331.cypher")
  nodeCount()
  edgeCount()

} # test_exampleGraph_3.3.1
#------------------------------------------------------------------------------------------------------------------------
test_getNodeLabels <- function()
{
   printf("--- test_getNodeLabels")

   clear.db();
   runCypherFile("createTwoActors.cypher")
   checkEquals(getNodeLabels(), c(":Movie", ":Person:Actor", ":Person:Director"))

} # test_getNodeLabels
#------------------------------------------------------------------------------------------------------------------------
test_getEdgeTypes <- function()
{
   printf("--- test_getEdgeTypes")

   clear.db();
   runCypherFile("createTwoActors.cypher")
   checkEquals(getEdgeTypes(), c("ACTED_IN", "DIRECTED"))

} # test_getEdgeTypes
#------------------------------------------------------------------------------------------------------------------------
test_getNodeTable <- function()
{
   printf("--- test_getNodeTable")

   clear.db()
   checkEquals(nodeCount(), 0)
   checkEquals(edgeCount(), 0)
   runCypherFile("createTwoActors.cypher")

   tbl.nodes <- getNodeTable()
   checkEquals(dim(tbl.nodes), c(4, 6))
   checkEquals(colnames(tbl.nodes), c("id", "label", "born", "name", "released", "title"))

} # test_getNodeTable
#------------------------------------------------------------------------------------------------------------------------
test_getEdgeTable <- function()
{
   printf("--- test_getEdgeTable")

   clear.db()
   checkEquals(nodeCount(), 0)
   checkEquals(edgeCount(), 0)
   runCypherFile("createTwoActors.cypher")

   tbl.edges <- getEdgeTable(directed=TRUE)

   checkEquals(dim(tbl.edges), c(3, 3))
   checkEquals(colnames(tbl.edges), c("a", "b", "type"))

   tbl.edges <- getEdgeTable(directed=FALSE)
   checkEquals(dim(tbl.edges), c(6, 3))
   checkEquals(colnames(tbl.edges), c("a", "b", "type"))

} # test_getEdgeTable
#------------------------------------------------------------------------------------------------------------------------
if(!interactive())
    runTests()
