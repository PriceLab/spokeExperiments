library(neo4r)
library(RUnit)
source("learn.R")
#------------------------------------------------------------------------------------------------------------------------
# make sure the northwind data has been loaded
stopifnot(nodeCount() == 1035)
#------------------------------------------------------------------------------------------------------------------------
runTests <- function()
{
   test_query()
   test_getCounts()
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
test_getCounts <- function()
{
    printf("--- test_getCounts")

    checkEquals(nodeCount(), 1035)
    checkEquals(edgeCount(), 3139)
    checkEquals(edgeCount(directed=FALSE), 6278)

} # test_getCounts
#------------------------------------------------------------------------------------------------------------------------
test_getNodeLabels <- function()
{
   printf("--- test_getNodeLabels")

   checkEquals(getNodeLabels(), c(":Category", ":Customer", ":Order", ":Product", ":Supplier"))

} # test_getNodeLabels
#------------------------------------------------------------------------------------------------------------------------
test_getEdgeTypes <- function()
{
   printf("--- test_getEdgeTypes")

   checkEquals(getEdgeTypes(), c("ORDERS", "PART_OF", "PURCHASED", "SUPPLIES"))

} # test_getEdgeTypes
#------------------------------------------------------------------------------------------------------------------------
test_getNodeTable <- function()
{
   printf("--- test_getNodeTable")

   tbl.nodes <- getNodeTable()
   checkEquals(dim(tbl.nodes), c(1035, 40))

} # test_getNodeTable
#------------------------------------------------------------------------------------------------------------------------
test_getEdgeTable <- function()
{
   printf("--- test_getEdgeTable")

   tbl.edges <- getEdgeTable(directed=TRUE)
   checkEquals(dim(tbl.edges), c(3139, 3))
   checkEquals(colnames(tbl.edges), c("a", "b", "type"))

   tbl.edges <- getEdgeTable(directed=FALSE)
   checkEquals(dim(tbl.edges), c(6278, 3))
   checkEquals(colnames(tbl.edges), c("a", "b", "type"))

} # test_getEdgeTable
#------------------------------------------------------------------------------------------------------------------------
if(!interactive())
    runTests()
