##' @importFrom tools md5sum
fileHash = function(fil) {
    paste0("MD5:",md5sum(fil))
}
    

##' @rdname main-api
##' @param ingestfun function or NULL. A function which must accept
##'     only the paths argument which will take \code{object} and
##'     return an R object to be recorded and linked to the raw files
##'     indicated by \code{object}
##' @details When more than one file is passed to
##'     \code{recordFiles},either via a vector of paths or the path to
##'     a directory, the files will be zipped up into a single file
##'     which will be recorded.
##' @export
recordFiles = function(object, ingestfun = NULL, db = defaultTDB(), resultURI = "",
                      code = histry_tracker(), force = FALSE,
                      verbose = FALSE, symorpos = NULL,
                      dryrun = FALSE) {
    ##this will error if any of the file(s) don't exist
    object = normalizePath(object, mustWork = TRUE)
    paths = object

    tmpfil = file.path(tempdir(), "rawfiles.zip")
    zip(tmpfil, object)
    object = tmpfil
    if (length(paths) == 1 && file.info(paths)$isdir) {
        paths = list.files(object, recursive = TRUE, full.names = TRUE)
    }
    tmpdb= TrackrDB(backend= ListBackend(),
                    img_dir = img_dir(defaultTDB()))
    stopifnot(identical(names(formals(ingestfun)), "paths"))
    funname = deparse(substitute(ingestfun))
    rawfilefs = RawFilesFeatureSet(object, origfiles = paths)
    if(!is.null(ingestfun)) {
        newcode = substitute(.objinmem <- f(a), list(f = as.symbol(funname), a = object))
        env = parent.frame()
        resobj = eval(newcode, envir = env)
        ## newcode is an expression, we need a call object to match what
        ## histry normally captures, thus the [[1]]
        code$addInfo(newcode, class(resobj), hash = fastdigest(resobj))
        record(resobj, code = code, db = tmpdb,
               resultURI = if(nzchar(resultURI)) paste0(resultURI, "/robject") else "",
               symorpos = ".objinmem")
        ## only one thing in here since it's a new empty list backend
        resfs = listRecToFeatureSet(findRecords("*:*", db = tmpdb)[[1]])
        resfs@derivedFromFileID = uniqueID(rawfilefs)
        resfs@derivedFromFilePath = object
        res1 = record(resfs, code = code, symorpos = ".objinmem", verbose = verbose,
               dryrun = dryrun)
    }
    res2 = record(rawfilefs, code = "", verbose = verbose, dryrun = dryrun)
    if(dryrun)
        list(res1, res2)
    else
        res2 ## this will be the trackr if dryrun is false...
  
}
