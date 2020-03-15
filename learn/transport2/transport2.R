library(neo4r)
library(RUnit)
library(RCyjs)
source("neo4j-utils.R")
#------------------------------------------------------------------------------------------------------------------------
if(!exists("rcy")){
  title <- "transport2"
  rcy <- RCyjs(title=title, quiet=TRUE)
  setBrowserWindowTitle(rcy, title)
  }
#------------------------------------------------------------------------------------------------------------------------
# make sure the northwind data has been loaded
stopifnot(nodeCount() == 12)
#------------------------------------------------------------------------------------------------------------------------
basicViz <- function()
{
  tbl.nodes <- getNodeTable()
  table(tbl.nodes$label)
    # :Category :Customer    :Order  :Product :Supplier
    #    8        91       830        77        29

  tbl.edges <- getEdgeTable()

  g.json <- toJSON(dataFramesToJSON(tbl.edges, tbl.nodes))
  setGraph(rcy, g.json)
  restoreLayout(rcy, "transport-layout.RData")
  fit(rcy)
  loadStyleFile(rcy, "transport-style.js")

} # viz
#------------------------------------------------------------------------------------------------------------------------
dijkstra.shortestPath <- function(from, to)
{
   s <- paste(sprintf("MATCH (source:Place {id: '%s'}), (destination:Place {id: '%s'})", from, to),
              "CALL algo.shortestPath.stream(source, destination, 'distance') YIELD nodeId, cost",
              "RETURN algo.getNodeById(nodeId).id AS place, cost")
   x <- query(s)
   clearSelection(rcy)
   cities <- x$place$value
   selectNodes(rcy, cities)

   getDistance <- function(from, to){
       stopifnot(from != to)
       from.rows <- c(grep(from, tbl.edges$target), grep(from, tbl.edges$source))
       to.rows   <- c(grep(to, tbl.edges$target), grep(to, tbl.edges$source))
       row <- intersect(from.rows, to.rows)[1]
       return(tbl.edges$distance[row])
       }

   max <- length(cities) - 1
   total.distance <- 0
   for(i in 1:max)
      total.distance <- total.distance + getDistance(cities[i], cities[i+1])

   printf("total distance: %d", total.distance)

} # dijkstra.shortestPath
#------------------------------------------------------------------------------------------------------------------------
test_dijkstra.shortestPath <- function()
{
   printf("--- test_dijkstra.shortestPath")
   dijkstra.shortestPath("Den Haag", "Immingham")

} # test_dijkstra.shortestPath
#------------------------------------------------------------------------------------------------------------------------
aStar.shortestPath <- function(from="Den Haag", to="Colchester")
{
   s <- paste(sprintf("MATCH (source:Place {id: '%s'}), (destination:Place {id: '%s'})", from, to),
              "CALL algo.shortestPath.astar.stream(source,",
              "destination, 'distance', 'latitude', 'longitude')",
              "YIELD nodeId, cost RETURN algo.getNodeById(nodeId).id AS place, cost")

   x <- query(s)
   clearSelection(rcy)
   cities <- x$place$value
   selectNodes(rcy, cities)

   getDistance <- function(from, to){
       stopifnot(from != to)
       from.rows <- c(grep(from, tbl.edges$target), grep(from, tbl.edges$source))
       to.rows   <- c(grep(to, tbl.edges$target), grep(to, tbl.edges$source))
       row <- intersect(from.rows, to.rows)[1]
       return(tbl.edges$distance[row])
       }

   max <- length(cities) - 1
   total.distance <- 0
   for(i in 1:max)
      total.distance <- total.distance + getDistance(cities[i], cities[i+1])

   printf("total distance: %d", total.distance)

} # aStar.shortestPath
#------------------------------------------------------------------------------------------------------------------------
test_aStar.shortestPath <- function()
{
   printf("--- test_aStart.shortestPath")

   clearSelection(rcy)
   aStar.shortestPath("London", "Den Haag")

   clearSelection(rcy)
   aStar.shortestPath("London", "Gouda")

   clearSelection(rcy)
   aStar.shortestPath("London", "Hoek van Holland")

   clearSelection(rcy)
   aStar.shortestPath("Doncaster", "Hoek van Holland")

} # test_dijkstra.shortestPath
#------------------------------------------------------------------------------------------------------------------------
# this fails with a call_neo4j error, apparently in using dplyr
# Error: Column `V5` can't be converted from numeric to character
yens.shortestPaths <- function(from, to)
{
   s <- paste(sprintf("MATCH (start:Place {id:'%s'}), (end:Place {id:'%s'})", from, to),
              "CALL algo.kShortestPaths.stream(start, end, 5, 'distance')",
              "YIELD index, nodeIds, path, costs",
              "RETURN index,",
              "[node in algo.getNodesById(nodeIds[1..-1]) | node.id] AS via,",
              "reduce(acc=0.0, cost in costs | acc + cost) AS totalCost")


   x <- query(s)
   clearSelection(rcy)
   cities <- x$place$value
   selectNodes(rcy, cities)

   getDistance <- function(from, to){
       stopifnot(from != to)
       from.rows <- c(grep(from, tbl.edges$target), grep(from, tbl.edges$source))
       to.rows   <- c(grep(to, tbl.edges$target), grep(to, tbl.edges$source))
       row <- intersect(from.rows, to.rows)[1]
       return(tbl.edges$distance[row])
       }

   max <- length(cities) - 1
   total.distance <- 0
   for(i in 1:max)
      total.distance <- total.distance + getDistance(cities[i], cities[i+1])

   printf("total distance: %d", total.distance)

} # yens.shortestPaths
#------------------------------------------------------------------------------------------------------------------------
apsp <- function()
{
   s <- paste("CALL algo.allShortestPaths.stream(null)",
              "YIELD sourceNodeId, targetNodeId, distance",
              "WHERE sourceNodeId < targetNodeId",
              "RETURN algo.getNodeById(sourceNodeId).id AS source,",
              "     algo.getNodeById(targetNodeId).id AS target,",
              "     distance",
              "ORDER BY distance DESC") #  LIMIT 10")

   x <- query(s)
   tbl.apsp <- data.frame(source=x$source[,1], target=x$target[,1], distance=x$distance[,1],
                          stringsAsFactors=FALSE)

      # books shows that distance is in miles (or whatever units are used in the graph)
      # the result I get is hops.
      # would need to make a separate call to get the actual path, actual distance

   # another approach, not obviously better, fails to return node names
   s <- paste("CALL algo.allShortestPaths.stream(null)",
              "YIELD sourceNodeId, targetNodeId, distance",
              "WHERE sourceNodeId < targetNodeId",
              "RETURN *")

   #x <- query(s)
   #tbl.apsp <- data.frame(source=x$sourceNodeId[,1], target=x$targetNodeId[,1], distance=x$distance[,1],
   #                       stringsAsFactors=FALSE)
   #new.order <- order(tbl.apsp$distance, decreasing=TRUE)
   #tbl.apsp <- tbl.apsp[new.order,]

   return(tbl.apsp)

} # apsp
#------------------------------------------------------------------------------------------------------------------------
test_apsp <- function()
{
   printf("--- test_apsp")
   tbl.apsp <- apsp()
   checkEquals(dim(tbl.apsp), c(66, 3))
   checkEquals(subset(tbl.apsp, source=="Colchester" & target=="London")$distance, 1)
   checkEquals(subset(tbl.apsp, source=="Colchester" & target=="Gouda")$distance, 5)

} # test_apsp
#------------------------------------------------------------------------------------------------------------------------
singleSourceShortestPath <- function(sourceName)
{
   s <- paste(sprintf("MATCH (n:Place {id: '%s'})", sourceName),
              "CALL algo.shortestPath.deltaStepping.stream(n, 'distance', 1.0) YIELD nodeId, distance",
              "WHERE algo.isFinite(distance)",
              "RETURN algo.getNodeById(nodeId).id AS destination, distance ORDER BY distance")
   x <- query(s)
   tbl.apsp <- data.frame(destination=x$destination[,1], distance=x$distance[,1],
                          stringsAsFactors=FALSE)


} # singleSourceShortestPath
#------------------------------------------------------------------------------------------------------------------------
test_singleSourceShortestPath <- function()
{
   printf("--- test_singleSourceShortestPath")
   tbl.sssp <- singleSourceShortestPath(sourceName="Rotterdam")
   checkEquals(dim(tbl.sssp), c(12,2))
   checkEquals(tbl.sssp[1, "distance"], 0)
   checkEquals(tbl.sssp["Doncaster", "distance"], 528)
   checkEquals(as.list(tbl.sssp[12,]), list(destination="Doncaster", distance=528))

} # test_singleSourceShortestPath
#------------------------------------------------------------------------------------------------------------------------
# odd side-effects are used here: a new edge of label :MINST is added to the graph as apparently
# the only way in which the spanning tree is remembered.
# I explicitly delete those edges from the graph before returning
minimalSpanningTree <- function(sourceName)
{
     # this query stores the result in the graph, as (I assume) property MINST
   s <- paste(sprintf("MATCH (n:Place {id:'%s'})", sourceName),
              "CALL algo.spanningTree.minimum('Place', 'EROAD', 'distance', id(n),",
              "{write:true, writeProperty:'MINST'})",
              "YIELD loadMillis, computeMillis, writeMillis, effectiveNodeCount",
              "RETURN loadMillis, computeMillis, writeMillis, effectiveNodeCount")

   x <- query(s)

   s2 <- paste(sprintf("MATCH path = (n:Place {id:'Amsterdam'})-[:MINST*]-()", sourceName),
               "WITH relationships(path) AS rels",
               "UNWIND rels AS rel",
               "WITH DISTINCT rel AS rel",
               "RETURN startNode(rel).id AS source, endNode(rel).id AS destination, rel.distance AS cost")
   x2 <- query(s2)

   tbl.edges <- getEdgeTable()
   tbl.minst <- subset(tbl.edges, interaction=="MINST")
   rownames(tbl.minst) <- NULL

   query("MATCH (m)-[rel:MINST]-(n) delete rel")

   return(tbl.minst)

} # minimalSpanningTree
#------------------------------------------------------------------------------------------------------------------------
test_minimalSpanningTree <- function()
{
   printf("--- test_minimalSpanningTree")

   tbl.minst <- minimalSpanningTree(sourceName="Amsterdam")
   checkEquals(dim(tbl.minst), c(11,4))
   checkEquals(dim(getEdgeTable()), c(15, 4))   # make sure the MINST edges are gone

} # test_minimalSpanningTree
#------------------------------------------------------------------------------------------------------------------------
if(!interactive())
    runTests()
