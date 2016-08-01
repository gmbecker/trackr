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
vtopts$history = NULL
vtopts$tracker = NULL

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
    if(!is(vtopts$history, "VirtHistoryTracker"))
        stop("cannot autotrack plots without automatic history tracking")

    qexpr = quote(addTaskCallback(function(expr, value, success, printed, tracker) { record(value, db = vtopts$vtdb, symorpos = length(viztrackr:::vtopts$history$exprs)); FALSE}))
    trace(lattice:::print.trellis, qexpr,
          where = asNamespace("lattice"))
    trace(ggplot2:::print.ggplot, qexpr, 
          where = asNamespace("ggplot2"))
    invisible(NULL)
}

##' is history tracking on
##' @export
trackingHistory = function() {
    is(vtopts$history, "VirtHistoryTracker") && vtopts$history$tracking
}

##' @title Automatically track history within an R session
##' @param tracker VirtHistoryTracker subclass or NULL. For NULL, if
##' a default tracker is set, toggle tracking with default tracker, otherwise
##' set a default tracker. For a VirtHistoryTracker (subclass) set as
##' the default tracker and turn it on.
##' @export
trackHistory = function( tracker = NULL) {
    if(!missing(tracker) && is(tracker, "VirtHistoryTracker")) {
        vtopts$history = tracker
        if(!trackingHistory())
            tracker$toggleTracking()
        message("Tracking session history. To turn off tracking call trackHistory().")
    } else if (is.null(tracker) && is.null(vtopts$history)) {
        vtopts$history = history_tracker("auto_history")
    } else {

        if(trackingHistory()) {
            message("Suspending automatic history tracking. To turn it back on call trackHistory() again.")
        } else {
            message("Reinstating history tracking with existing default tracker.")
        }
        vtopts$history$toggleTracking()
    }
    invisible(NULL)

}
