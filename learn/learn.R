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
query <- function(s)
{
    suppressMessages(
        x <- call_neo4j(s, con)
        )

    if(length(x) == 0)
       return(data.frame())

    tbls <- lapply(x, as.data.frame)
    #return(as.data.frame(tibble[[1]]))
    if(length(tbls) == 1)
      return(tbls[[1]])

    return(tbls)

} # query
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
edgeCount <- function(directed=TRUE)
{
  queryString <- "match ()-[r]->() return count(r)"

  if(!directed)
     queryString <- "match ()-[r]-() return count(r)"

  return(query(queryString)$value)

} # edgeCount
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
getNodeLabels <- function()
{
   tbl.raw <- query("match (n) return distinct labels(n)")
   if(nrow(tbl.raw) == 0)
       return(c())

   labels <- sort(unique(unlist(lapply(seq_len(nrow(tbl.raw)), function(r) paste(tbl.raw[r,], collapse=":")))))
   labels <- gsub(":NA", "", labels, fixed=TRUE)
   labels <- paste(":", labels, sep="")

   return(labels)

} # getNodeLabels
#------------------------------------------------------------------------------------------------------------------------
getEdgeTypes <- function()
{
   tbl.raw <- query("MATCH (n)-[r]-(m) RETURN distinct type(r)")
   if(nrow(tbl.raw) == 0)
       return(c())

   sort(tbl.raw$value)

} # getNodeLabels
#------------------------------------------------------------------------------------------------------------------------
getNodeTable <- function()
{

   # add id property to every node if not already present
   if(nrow(query("MATCH (n) return(n.id)")) == 0){
      query("match (n) set n.id = id(n)")
      }

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
