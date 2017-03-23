recplothook = function(x, opts, ...) {
    if(is.null(defaultTDB())) {
        warning("cannot record plot, default db not set. Use defaultTDB() in the first chunk to do this")
    } else if(!is.null(ggplot2:::last_plot())){
        
        len = length(histry::histropts$history$exprs)
        record(ggplot2:::last_plot(), symorpos = len)
    }
    paste0("![", x, "]")

}



.handlerender = function(rfun) {
    if(is.null(rfun))
        ## lifted from knitr:::knit_handlers (knitr:::utils.R) 
        rfun = function(x, ...) {
            res = withVisible(knit_print(x, ...))
                                        # indicate the htmlwidget result with a special class so we can attach
                                        # the figure caption to it later in wrap.knit_asis
            if (inherits(x, 'htmlwidget'))
                class(res$value) = c(class(res$value), 'knit_asis_htmlwidget')
            if (res$visible) res$value else invisible(res$value)
        }

    function(x, options) {
        record(x)
        rfun(x, options = options)
    }
}


##' @title Knit and record an Rmd, Rnw, etc file
##'
##' @description This function wraps knitr's \code{knit} function in
##' a way that captures and records all the outputs printed in the final report,
##' associates them with the report, and then records the report as a whole.
##'
##' This means that many records will generally be added to the trackr db for
##' a single call to this function.
##'
##' @param input The input argument exactly as knitr's \code{knit} function
##' accepts it
##' @param ... Passed directly to \code{knit}
##' @param verbose passed to (multiple) \code{record} calls for report and its
##' outputs
##' @note as with all knitr support in the histry and trackr packages, manually
##' tracing certain functions within the knitr and evaluate packages will break
##' this function. 
##' @export
knit_and_record = function(input, ..., verbose = FALSE) {
    tmptdb = TrackrDB(backend = listbackend())
    oldtdb = defaultTDB()
    on.exit(defaultTDB(oldtdb))
    defaultTDB(tmptdb)
    knitrtracer(FALSE) ## probably unnecessary
    evaltracer(TRUE, TRUE)

    superstupidenv$chunks = NULL
    suppressMessages(trace(knitr:::split_file, exit = quote(assign("chunks", returnValue(), envir = trackr:::superstupidenv)),
          print = FALSE))
    on.exit(untrace(knitr:::split_file))


    resfile = knit(input = input, ...)
    suppressMessages(untrace(knitr:::split_file))
    on.exit(NULL)
    chunks = superstupidenv$chunks
    superstupidenv$chunks = NULL

    evaltracer(FALSE)
    knitrtracer(TRUE)
    rmdfs = RmdFeatureSet(rmdfile = input, objtdb = tmptdb,
                          outputfile = resfile, chunks = chunks)
    objfsets = lapply(docs(trackr_backend(tmptdb)), function(x) {
        fs = listRecToFeatureSet(x)
        fs@generatedin = uniqueID(rmdfs)
        fs})
    defaultTDB(oldtdb)
    on.exit(NULL)
    record(rmdfs, code =NULL, verbose = verbose)
    lapply(objfsets, function(x) record(x, code = objCode(x),
                                        symorpos = length(code),
                                        verbose = verbose))
    oldtdb
        
}
