## #' @rdname addToDataStore
## #' @param img.save.dir Directory in which Solr/Blacklight will look for the full-size images. For now, use absolute path to rails app's assets/images/ directory for best results. Set to NULL by default (no images saved).
## #' @param img.ext "png" is currently the only implemented method and therefore is the only viable option for this parameter.
## #' @param verbose An optional boolean indicating whether diagnostic messages should be printed. False by default.
## #' @export
## #' @examples
## #' library(ggplot2)
## #' pg <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
## #' pfs <- makePlotFeatureSet(pg)
## #' #solr.uri <- httr::modify_url("http://whatever",
## #' #    hostname = "reswebdev301.gene.com", port = 19971, path = "solr/viztrackrdb")
## #' ## same as:
## #' solr.uri <- "http://reswebdev301.gene.com:19971/solr/viztrackrdb"
## #'\dontrun{
## #' library(rsolr)
## #' sl <- SolrList(solr.uri, requestHandler = "search")
## #' sl <- addToDataStore(pfs, sl, img.save.dir = "/gne/research/web/dev/ruby-on-rails/viztrackr/app/assets/images")
## #' ## equivalently, add the ggplot object directly:
## #' # sl <- addToDataStore(pg, sl, img.save.dir = "/gne/research/web/dev/ruby-on-rails/viztrackr/app/assets/images")
## #'}
## setMethod(f = "addToDataStore",
##     signature = signature(object = "PlotFeatureSet", data.store = "SolrList"),
##     definition = function(object, data.store,
##         img.save.dir = NULL, img.ext = "png", verbose = FALSE) {

##         # UTC required for Solr dates
##         object@regdate <- .POSIXct(object@regdate, tz="UTC")
##         existing.record <- searchDataStore(uniqueID(object), data.store, query.field = "id", verbose = verbose)

##         if (nrow(existing.record)>0) {
##             warning("Overwriting previous version with ID ", uniqueID(object), ".")
##         }

## #        doc <- c(id = uniqueID(object), flatten4(as(object, "list")))
##     doc <- c(id = uniqueID(object), flatten5(as(object, "list")))

##         if(!is.null(img.save.dir)) {
##             invisible(saveBasicPlot(object, 
##                 file.path(img.save.dir, paste0(uniqueID(object), "_thumb.", img.ext)),
##                 width = 5, height = 5, dpi = 50))
##             invisible(saveBasicPlot(object, 
##                 file.path(img.save.dir, paste0(uniqueID(object), "_feed.", img.ext)),
##                 width = 5, height = 5, dpi = 100))
##             # main image with R object embedded as metadata
##             saveEnrichedPlot(object, 
##                 file.path(img.save.dir, paste(uniqueID(object), img.ext, sep=".")))

##             doc$preview.path = paste0(uniqueID(object), "_thumb.", img.ext)
##             doc$image.path = paste(uniqueID(object), img.ext, sep=".")
##         }

##         data.store[insert=TRUE] <- list(doc)

##         if (verbose) {
##             cat("ID ", uniqueID(object), "\n")
##         }
##         invisible(data.store)
##     }
## )

## #' @describeIn addToDataStore Directly add an object of class ggplot to the data store by silently constructing a PlotFeatureSet.
## setMethod(f = "addToDataStore", 
##     signature = signature(object = "ggplot"),
##     definition = function(object, data.store,
##         img.save.dir = NULL, img.ext = "png", verbose = FALSE) {
        
##         pfs <- makePlotFeatureSet(object)
##         if(verbose) {
##             message("Adding ggplot object with ID ", uniqueID(pfs), "...")
##         }
##         addToDataStore(pfs, data.store,
##             img.save.dir = img.save.dir, img.ext = img.ext, verbose = verbose)
##     }
## )

## #' @describeIn addToDataStore Directly add an object of class trellis to the data store by silently constructing a PlotFeatureSet.
## setMethod(f = "addToDataStore", 
##     signature = signature(object = "trellis"),
##     definition = function(object, data.store,
##         img.save.dir = NULL, img.ext = "png", verbose = FALSE) {
        
##         pfs <- makePlotFeatureSet(object)
##         # gtree.version <- grid::grid.grabExpr(print(object))
##         if(verbose) {
##             message("Adding trellis object with ID ", uniqueID(pfs), "...")
##         }
##         addToDataStore(pfs, data.store,
##             img.save.dir = img.save.dir, img.ext = img.ext, verbose = verbose)
##     }
## )

## #' @rdname searchDataStore
## #' @param query.field An optional character vector indicating the field(s) to be queried. Multiple fields are combined with an OR operator. NULL by default, leading to use of query fields specified as defaults for the search handler (for solr) or all fields (for other data stores not yet implemented).
## #' @param verbose An optional boolean indicating whether diagnostic messages should be printed. False by default.
## #' @export
## setMethod(f = "searchDataStore",
##     signature = signature(q = "character", data.store = "SolrList"),
##     definition = function(q, data.store, query.field = NULL, verbose = FALSE) {

##         final.query <- paste(q, collapse=" OR ")
##         if(!is.null(query.field)) {
##             final.query <- paste(
##                 apply(expand.grid(query.field, q), 1, paste, collapse=":"), 
##                 collapse=" OR ")
##         }
        
##         sr <- rsolr::searchDocs(data.store, final.query)

##         # sf <- rsolr::SolrFrame(solr.uri, requestHandler = "search", q = final.query, fill = FALSE)
##         if (verbose) {
##             message("Found ", rsolr::ndoc(sr), " documents matching query '", final.query, "'.")
##         }

##         return(as.data.frame(sr))
##     }
## )

## #' @rdname removeFromDataStore
## #' @param img.save.dir Directory in which Solr/Blacklight will look for the full-size images. For now, use absolute path to rails app's assets/images/ directory for best results. Set to NULL by default (no images removed).
## #' @param verbose A boolean indicating whether diagnostic messages should be printed. False by default.
## #' @export
## setMethod(f = "removeFromDataStore",
##     signature = signature(object = "character", data.store = "SolrList"),
##     definition = function(object, data.store, img.save.dir = NULL, verbose = FALSE) {

##         obj.to.rm <- searchDataStore(object, data.store, query.field = "id", verbose = verbose)
##         if (nrow(obj.to.rm)==1) {
##             data.store[obj.to.rm[,"id"]] <- NULL

##             if(!is.na(obj.to.rm[,"preview.path"])) {
##                 file.remove(file.path(img.save.dir, obj.to.rm[,"preview.path"]))
##             }
##             if(!is.na(obj.to.rm[,"image.path"])) {
##                 file.remove(file.path(img.save.dir, obj.to.rm[,"image.path"]))
##             }
##             if (verbose) {
##                 message("Document with ID ", object, " was removed from the data store.")
##             }

##         } else if(nrow(obj.to.rm)==0) {
##             warning("No documents found matching id ", object, ". Nothing was done.")
##         } else {
##             warning("More than one document found matching id ", object, ", indicating that the id field is not unique. Please check your data store configuration. Nothing was done.")
##         }
##         invisible(data.store)
##     }
## )

## #' @rdname removeFromDataStore
## setMethod(f = "removeFromDataStore",
##     signature = signature(object = "PlotFeatureSet", data.store = "SolrList"),
##     definition = function(object, data.store, img.save.dir = NULL, verbose = FALSE) {
##         removeFromDataStore(uniqueID(object), data.store, 
##             img.save.dir = img.save.dir, verbose = verbose)
##     }
## )

## #' @rdname removeFromDataStore
## setMethod(f = "removeFromDataStore",
##     signature = signature(object = "ggplot", data.store = "SolrList"),
##     definition = function(object, data.store, img.save.dir = NULL, verbose = FALSE) {
##         pfs <- makePlotFeatureSet(object)
##         removeFromDataStore(pfs, data.store, 
##             img.save.dir = img.save.dir, verbose = verbose)
##     }
## )

## #' @rdname removeFromDataStore
## setMethod(f = "removeFromDataStore",
##     signature = signature(object = "trellis", data.store = "SolrList"),
##     definition = function(object, data.store, img.save.dir = NULL, verbose = FALSE) {
##         pfs <- makePlotFeatureSet(object)
##         removeFromDataStore(pfs, data.store, 
##             img.save.dir = img.save.dir, verbose = verbose)
##     }
## )

## #' @rdname removeFromDataStore
## setMethod(f = "removeFromDataStore",
##     signature = signature(object = "gTree", data.store = "SolrList"),
##     definition = function(object, data.store, img.save.dir = NULL, verbose = FALSE) {
##         pfs <- makePlotFeatureSet(object)
##         removeFromDataStore(pfs, data.store, 
##             img.save.dir = img.save.dir, verbose = verbose)
##     }
## )

## #' @rdname removeFromDataStore
## setMethod(f = "removeFromDataStore",
##     signature = signature(object = "recordedplot", data.store = "SolrList"),
##     definition = function(object, data.store, img.save.dir = NULL, verbose = FALSE) {
##         gridGraphics::grid.echo(plot)
##         plot2 <- grid::grid.grab()
##         removeFromDataStore(plot2, data.store, 
##             img.save.dir = img.save.dir, verbose = verbose)
##     }
## )
