## storage for mutable things that live in the namespace
## most notably, the default TrackrDB and default
## history trackr
##' @title trackr options environment
##' @description An environment where the default trackrdb and history tracker are stored.
##' Users should not modify the contents of this environment directly.
##'
##' @export
trackropts = new.env()
trackropts$trackrdb = NULL
trackropts$autotracking = FALSE
trackropts$tracker = NULL


##' @title default TrackrDB
##' @description Get or set the default TrackrDB in use (ie the one that is
##' used when `record` is used with no trackrdb specified.
##' @param trackrdb TrackrDB. The TrackrDB to which objects will be recorded
##' by default. If missing, the current default is returned.
##' @export

defaultTDB = function(trackrdb) {
    if(missing(trackrdb)) {
        if(is.null(trackropts$trackrdb))
            trackropts$trackrdb = jsonTDB()
        trackropts$trackrdb
    } else {
        trackropts$trackrdb = trackrdb
        trackrdb
    }
}


## ## trackrdb=NULL turns off tracking, like Rprof
## ##' @title Automatically record plots
## ##' Toggle automatic recording of plots to the
## ##' current default TrackrDB.
## ##' @param trackrdb TrackrDB. The TrackrDB to record
## ##' plots to automatically. If specified, will become
## ##' the default.
## ##' @return NULL, invisibly.
## ##' @export
## autotrackPlots = function(trackrdb = trackropts$trackrdb) {
##     wastracking = trackropts$autotracking
##     if(wastracking) {
##         message("Turning off plot autotracking.")
##         trackropts$autotracking = FALSE
##         try(untrace(print.trellis, where = asNamespace("lattice")))
##         try(untrace(print.ggplot, where = asNamespace("ggplot2")))
##     }
##     ## if it was called as autotrackPlots() we're done
##     if(wastracking && missing(trackrdb)) {
##         return(invisible(TRUE))
##     } 
##     if (!is.null(trackrdb)) {
##         message("Enabling plot autotracking.")
##         trackropts$trackrdb = trackrdb
##         trackropts$autotracking = TRUE
##     }

##     if(is.null(trackrdb))
##         stop("No TrackrDB specified or currently active")
##     if(!trackingHistory())
##         stop("cannot autotrack plots without automatic history tracking")

##     qexpr = quote(addTaskCallback(function(expr, value, success, printed, tracker) { record(value, db = trackropts$trackrdb, symorpos = length(histry::histry())); FALSE}))
##     trace(lattice:::print.trellis, qexpr,
##           where = asNamespace("lattice"))
##     trace(ggplot2:::print.ggplot, qexpr, 
##           where = asNamespace("ggplot2"))
##     invisible(NULL)
## }
