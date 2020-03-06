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
   test_runCypherFile()
   test_getNodeLabels()
   test_getEdgeTypes()
   test_getNodeTable()
   test_getEdgeTable()

} # runTests
#------------------------------------------------------------------------------------------------------------------------
query <- function(s)
{
    suppressMessages(
        x <- call_neo4j(s, con)
        )

    if(length(x) == 0)
       return(NULL)

    tbls <- lapply(x, as.data.frame)
    #return(as.data.frame(tibble[[1]]))
    if(length(tbls) == 1)
      return(tbls[[1]])

    return(tbls)

} # query
#------------------------------------------------------------------------------------------------------------------------
test_query <- function()
{
   printf("--- test_query")
   tbl.out <- query("match(n) return count(n)")
   checkTrue(tbl.out$value >= 0)

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

  query("match (n) set n.id = id(n)")


} # loadCSV.northwind
#------------------------------------------------------------------------------------------------------------------------
loadCSV.artists <- function()
{
  #   LOAD CSV FROM 'https://neo4j.com/docs/cypher-manual/3.5/csv/artists.csv' AS line

  load.query <- "CREATE (n:Artist{name: line[1], year: toInteger(line[2])})"

  load_csv(url = "https://neo4j.com/docs/cypher-manual/3.5/csv/artists.csv",
           con = con, header = FALSE, periodic_commit = 50,
           as = "line", on_load=load.query)

  query("match (n) set n.id = id(n)")

} # loadCSV.artists
#------------------------------------------------------------------------------------------------------------------------
clear.db <- function()
{
  ignore <- query("match (n) detach delete n")

} # clear.db
#------------------------------------------------------------------------------------------------------------------------
nodeCount <- function()
{
  return(query("match (n) return count(n)")$value)

} # nodeCount
#------------------------------------------------------------------------------------------------------------------------
directedEdgeCount <- function()
{
  return(query("match ()-[r]->() return count(r)")$value)

} # nodeCount
#------------------------------------------------------------------------------------------------------------------------
fullGraph <- function()
{
    return(query("match (n)-[r]-(m) return r"))

} # fullGraph
#------------------------------------------------------------------------------------------------------------------------
runCypherFile <- function(filename)
{
  suppressMessages(
     result <- send_cypher(filename, con, type = c("row"), output = "r", include_stats = TRUE, meta = FALSE)
     )

  query("match (n) set n.id = id(n)")
  invisible(result)

} # runCypherFile
#------------------------------------------------------------------------------------------------------------------------
test_runCypherFile <- function()
{
  printf("--- test_runCypherFile")

  clear.db()
  checkEquals(nodeCount(), 0)
  checkEquals(directedEdgeCount(), 0)

  runCypherFile("createTwoActors.cypher")
  checkEquals(nodeCount(), 4)
  checkEquals(directedEdgeCount(), 3)

} # test_runCypherFile
#------------------------------------------------------------------------------------------------------------------------
test_exampleGraph_3.3.1 <- function()
{
  printf("--- test_exampleGraph_3.3.1")
  clear.db()
  runCypherFile("exampleGraph-331.cypher")
  nodeCount()
  directedEdgeCount()

} # test_exampleGraph_3.3.1
#------------------------------------------------------------------------------------------------------------------------
getNodeLabels <- function()
{
   tbl <- query("match (n) return distinct labels(n)")
   labels <- sort(unique(unlist(lapply(seq_len(nrow(tbl)), function(r) paste(tbl[r,], collapse=":")))))
   labels <- gsub(":NA", "", labels, fixed=TRUE)
   labels <- paste(":", labels, sep="")

   return(labels)

} # getNodeLabels
#------------------------------------------------------------------------------------------------------------------------
test_getNodeLabels <- function()
{
   printf("--- test_getNodeLabels")

   clear.db();
   runCypherFile("createTwoActors.cypher")
   checkEquals(getNodeLabels(), c(":Movie", ":Person:Actor", ":Person:Director"))

} # test_getNodeLabels
#------------------------------------------------------------------------------------------------------------------------
getEdgeTypes <- function()
{
   sort(query("MATCH (n)-[r]-(m) RETURN distinct type(r)")$value)

} # getNodeLabels
#------------------------------------------------------------------------------------------------------------------------
test_getEdgeTypes <- function()
{
   printf("--- test_getEdgeTypes")

   clear.db();
   runCypherFile("createTwoActors.cypher")
   checkEquals(getEdgeTypes(), c("ACTED_IN", "DIRECTED"))

} # test_getEdgeTypes
#------------------------------------------------------------------------------------------------------------------------
getNodeTable <- function()
{
   #query("match (n) set n.id = id(n)")
   labels <- getNodeLabels()

   build.label.table <- function(label){
      tbl <- query(sprintf("match (n%s) return n", label))
      tbl$label <- label
      tbl
      }

   x <- lapply(labels, build.label.table)

   column.names <- sort(unique(unlist(lapply(x, colnames))))
   tbl <- setNames(data.frame(matrix(ncol=length(column.names), nrow=0)), column.names)
   for(tbl.sub in x)
      tbl <- merge(tbl, tbl.sub, all.x=TRUE, all.y=TRUE)

   column.names <- colnames(tbl)  # may have changed due to the merge
      # put the table in the right column order, starting with id and label
   id.index <- grep("^id$", column.names)
   label.index <- grep("^label$", column.names)
   other.indices <- seq_len(length(column.names))[-c(id.index, label.index)]
   other.colnames <- sort(column.names[other.indices])
   preferred.colnames <- c("id", "label", other.colnames)
   tbl <- tbl[, preferred.colnames]
   preferred.row.order <- order(tbl$id)
   tbl[preferred.row.order,]

} # getNodeTable
#------------------------------------------------------------------------------------------------------------------------
test_getNodeTable <- function()
{
   printf("--- test_getNodeTable")

   clear.db()
   checkEquals(nodeCount(), 0)
   checkEquals(directedEdgeCount(), 0)
   runCypherFile("createTwoActors.cypher")

   tbl.nodes <- getNodeTable()
   checkEquals(dim(tbl.nodes), c(4, 6))
   checkEquals(colnames(tbl.nodes), c("id", "label", "born", "name", "released", "title"))

} # test_getNodeTable
#------------------------------------------------------------------------------------------------------------------------
getEdgeTable <- function(directed=TRUE)
{
   x <- query("match (m)-[r]-(n) return m, n, type(r)")

   tbl <- data.frame(a=x$m$id, b=x$n$id, type=x$type$value, stringsAsFactors=FALSE)
   sigs <- vector("character", nrow(tbl))

   if(directed){
     for(r in seq_len(nrow(tbl))){
        ordered.nodes <- sort(c(tbl[r, "a"], tbl[r, "b"]))
        sigs[r] <- sprintf("%s:%d:%d", tbl[r, "type"], ordered.nodes[1], ordered.nodes[2])
        } # for r
     deleters <- which(duplicated(sigs))
     if(length(deleters) > 0)
         tbl <- tbl[-deleters,]
     } # if directed

   return(tbl)

} # getEdgeTable
#------------------------------------------------------------------------------------------------------------------------
test_getEdgeTable <- function()
{
   printf("--- test_getEdgeTable")

   clear.db()
   checkEquals(nodeCount(), 0)
   checkEquals(directedEdgeCount(), 0)
   runCypherFile("createTwoActors.cypher")

   tbl.edges <- getEdgeTable(directed=TRUE)

   checkEquals(dim(tbl.edges), c(3, 3))
   checkEquals(colnames(tbl.edges), c("a", "b", "type"))

   tbl.edges <- getEdgeTable(directed=FALSE)
   checkEquals(dim(tbl.edges), c(6, 3))
   checkEquals(colnames(tbl.edges), c("a", "b", "type"))

} # test_getEdgeTable
#------------------------------------------------------------------------------------------------------------------------
