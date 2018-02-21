recplothook = function(x, opts, ...) {
    if(is.null(defaultTDB())) {
        warning("cannot record plot, default db not set. Use defaultTDB() in the first chunk to do this")
    } else if(!is.null(ggplot2::last_plot())){
        
        len = length(histry::histry())
        record(ggplot2::last_plot(), symorpos = len)
    }
    paste0("![", x, "]")

}



.handlerender = function(rfun) {
    if(is.null(rfun))
        ## lifted from knitr:::knit_handlers (knitr:::utils.R) 
        rfun = function(x, ...) {
            res = withVisible(knitr::knit_print(x, ...))
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
##' @param tmptdb A TrackrDB in which to temporarily record results which are printed within the dynamic document. Generally this should not need to be changed, as it is only used to collect the records so they can be associated with the result for the whole document (in the defaultTDB).
##' @note as with all knitr support in the histry and trackr packages, manually
##' tracing certain functions within the knitr and evaluate packages will break
##' this function.
##' @import rmarkdown
##' @export
knit_and_record = function(input, ..., verbose = FALSE,
                           tmptdb = TrackrDB(backend= ListBackend(), img_dir = img_dir(defaultTDB()))) {
    oldtdb = defaultTDB()
    on.exit(defaultTDB(oldtdb))
    defaultTDB(tmptdb)
    ## knitrtracer(FALSE) ## probably unnecessary
    evaltracer(FALSE)
    evaltracer(TRUE, TRUE)
    trackr_knit_env$chunks = NULL
    suppressMessages(trace("split_file", where = asNamespace("knitr"),
                           exit = quote(assign('chunks', returnValue(), envir = trackr_knit_env)),
                           print = FALSE))
    on.exit(suppressMessages(untrace("split_file", where = asNamespace("knitr"))), add=TRUE)

      
    if("output" %in% names(list(...)))
        odir = dirname(list(...)$output)
    else
        odir = dirname(input) #getwd()

    starttime = Sys.time()
    if(grepl("[Rr][Mm][Dd]$", input))
        resfile = render(input = input,output_format = html_document(self_contained = FALSE, mathjax = NULL),
                         run_pandoc = TRUE, ...)
    else if (grepl("[Rr][Nn][Ww]$", input)) {
        resfile = render(input = input,  output_format = "pdf_document",
                         run_pandoc=TRUE, ...)
    }
    endtime = Sys.time()
    filenamestub = gsub("(.*)\\.R..$", "\\1", basename(input))
    figpath = file.path(odir, paste0(filenamestub, "_files"))
    figs = character()

    if(file.exists(file.path(odir, "figure"))) {
        dir.create(file.path(figpath, "figure-html"))
        file.copy(list.files(file.path(odir, "figure"), full.names=TRUE), file.path(figpath, "figure-html"))
    }
    if(file.exists(figpath))
        figs = list.files(figpath, recursive=TRUE)

    figsfull = file.path(figpath, figs)

   
   
    if(length(figs) > 0 && grepl("html", resfile, ignore.case=TRUE)) {
        alllines = readLines(resfile)
        figs = figs[sapply(basename(figs), function(x) any(grepl(x, alllines)))]
        ## ## these are integers. ugh. thisisfine.jpg
        ## figmtimes = sapply(figsfull, function(x) file.info(x)$mtime)
        ## figs = figs[figmtimes > starttime & figmtimes <= endtime]
    }

    figmd5 = character()
    if(length(figs) > 0) {
        figmd5 = tools::md5sum(figs)
    }

    uniqueid = gen_hash_id(c(readLines(resfile), figmd5))
        
    
  
    chunks = unlist(lapply( trackr_knit_env$chunks, function(x) x$input))
    trackr_knit_env$chunks = NULL

    evaltracer(FALSE)
    evaltracer(TRUE)

    defaultTDB(oldtdb)
    suppressMessages(untrace("split_file", where = asNamespace("knitr")))
    on.exit(NULL)

    rmdfs = RmdFeatureSet(rmdfile = input, objtdb = tmptdb,
                          uniqueid = uniqueid,
                          outputfile = resfile, chunks = chunks,
                          figurefiles = figs)
    imgpat = paste0("(", paste(rmdfs@outputids, collapse="|"), ")")
    imgfiles = list.files(img_dir(tmptdb), pattern = imgpat, full.names = TRUE)
    print(imgfiles)
    stopifnot(length(imgfiles) == 3 * length(rmdfs@outputids))
    file.copy(imgfiles, file.path(img_dir(oldtdb), basename(imgfiles)))
    objfsets = lapply(docs(trackr_backend(tmptdb)), function(x) {
        fs = listRecToFeatureSet(x)
        fs@generatedin = uniqueID(rmdfs)
        fs@regdate = rmdfs@regdate
        fs})
    record(rmdfs, code =NULL, verbose = verbose)
    lapply(objfsets, function(x) record(x, code = objCode(x),
                                        symorpos = length(code),
                                        verbose = verbose))
    oldtdb
        
}

    
