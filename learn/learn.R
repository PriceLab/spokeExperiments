library(neo4r)
library(RUnit)
#------------------------------------------------------------------------------------------------------------------------
if(!exists("con")){
   con <- neo4j_api$new(
     url = "http://localhost:7475",
     user = "neo4j",
     password = "hoopa"
     )
   }
stopifnot("Neo4JAPI" %in% class(con))
#------------------------------------------------------------------------------------------------------------------------
runTests <- function()
{
    test_query()

} # runTests
#------------------------------------------------------------------------------------------------------------------------
query <- function(s)
{
   as.data.frame(call_neo4j(s, con))

} # query
#------------------------------------------------------------------------------------------------------------------------
test_query <- function()
{
   printf("--- test_query")
   tbl.out <- query("match(n) return count(n)")
   checkTrue(tbl.out$value > 13000000)

} # test_query
#------------------------------------------------------------------------------------------------------------------------
loadCSV.northwind <- function()
{
  on_load_query <- 'CREATE (n:Product)
    SET n = row,
    n.unitPrice = toFloat(row.unitPrice),
    n.unitsInStock = toInteger(row.unitsInStock), n.unitsOnOrder = toInteger(row.unitsOnOrder),
    n.reorderLevel = toInteger(row.reorderLevel), n.discontinued = (row.discontinued <> "0");'

  load_csv(url = "http://data.neo4j.com/northwind/products.csv",
           con = con, header = TRUE, periodic_commit = 50,
           as = "row", on_load = on_load_query)

} # loadCSV.northwind
#------------------------------------------------------------------------------------------------------------------------
loadCSV.artists <- function()
{
  #   LOAD CSV FROM 'https://neo4j.com/docs/cypher-manual/3.5/csv/artists.csv' AS line

  load.query <- "CREATE (n:Artist{name: line[1], year: toInteger(line[2])})"

  load_csv(url = "https://neo4j.com/docs/cypher-manual/3.5/csv/artists.csv",
           con = con, header = FALSE, periodic_commit = 50,
           as = "line", on_load=load.query)


} # loadCSV.artists
#------------------------------------------------------------------------------------------------------------------------
clear.db <- function()
{
  inore <- query("match (n) delete n")

} # clear.db
#------------------------------------------------------------------------------------------------------------------------
nodeCount <- function()
{
  return(query("match (n) return count(n)")$value)

} # nodeCount
#------------------------------------------------------------------------------------------------------------------------
