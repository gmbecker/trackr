## storage for mutable things that live in the namespace
## most notably, the default ViztrackrDB and default
## history trackr
##' @title viztrackr options environment
##' An environment where the default vtdb and history tracker are stored.
##' Users should not modify the contents of this environment directly.
##'
##' @export
vtopts = new.env()
vtopts$vtdb = NULL
vtopts$autotracking = FALSE
vtopts$tracker = NULL

##' @importFrom histry histropts trackingHistory

##' @title default VTDB
##' @param vtdb ViztrackrDB. The ViztrackrDB to which objects will be recorded
##' by default. If missing, the current default is returned.
##' @export

defaultVTDB = function(vtdb) {
    if(missing(vtdb))
        vtopts$vtdb
    else {
        vtopts$vtdb = vtdb
        vtdb
    }
}


## vtdb=NULL turns off tracking, like Rprof
##' @title Automatically record plots
##' Toggle automatic recording of plots to the
##' current default ViztrackrDB.
##' @param vtdb ViztrackrDB. The ViztrackrDB to record
##' plots to automatically. If specified, will become
##' the default.
##' @return NULL, invisibly.
##' @export
autotrackPlots = function(vtdb = vtopts$vtdb) {
    wastracking = vtopts$autotracking
    if(wastracking) {
        message("Turning off plot autotracking.")
        vtopts$autotracking = FALSE
        try(untrace(print.trellis, where = asNamespace("lattice")))
        try(untrace(print.ggplot, where = asNamespace("ggplot2")))
    }
    ## if it was called as autotrackPlots() we're done
    if(wastracking && missing(vtdb)) {
        return(invisible(TRUE))
    } 
    if (!is.null(vtdb)) {
        message("Enabling plot autotracking.")
        vtopts$vtdb = vtdb
        vtopts$autotracking = TRUE
    }

    if(is.null(vtdb))
        stop("No ViztrackrDB specified or currently active")
    if(!trackingHistory())
        stop("cannot autotrack plots without automatic history tracking")

    qexpr = quote(addTaskCallback(function(expr, value, success, printed, tracker) { record(value, db = vtopts$vtdb, symorpos = length(histry:::histropts$history$exprs)); FALSE}))
    trace(lattice:::print.trellis, qexpr,
          where = asNamespace("lattice"))
    trace(ggplot2:::print.ggplot, qexpr, 
          where = asNamespace("ggplot2"))
    invisible(NULL)
}
