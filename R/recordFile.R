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
recordFiles = function(object, ingestfun = NULL, db = defaultTDB, resultURI = "",
                      code = histry_tracker(), force = FALSE,
                      verbose = FALSE, symorpos = NULL) {
    ##this will error if any of the file(s) don't exist
    object = normalizePath(object, mustWork = TRUE)
    paths = object
    if(length(object) > 1) {
        tmpfil = tempfile(fileext = ".zip")
        zip(tmpfil, object)
        object = tmpfil
    } else if (file.info(object)$isdir) {
        paths = list.files(object, recursive = TRUE, full.names = TRUE)
        tmpfil = tempfile(fileext = ".zip")
        zip(tmpfil, paths)
        object = tmpfil
    }
    tmpdb= TrackrDB(backend= ListBackend(),
                    img_dir = img_dir(defaultTDB()))
    stopifnot(identical(names(formals(ingestfun)), "paths"))
    funname = deparse(substitute(ingestfun))
    rawfilefs = RawFilesFeatureSet(object, origfiles = paths)
    if(!is.null(ingestfun)) {
        newcode = parse(text = paste0(".objinmem = ", funname, "( '", object, "' )"))
        env = parent.frame()
        resobj = eval(newcode, envir = env)
        ## newcode is an expression, we need a call object to match what
        ## histry normally captures, thus the [[1]]
        code$addInfo(newcode[[1]], class(resobj), hash = fastdigest(resobj))
        record(resobj, code = code, db = tmpdb,
               resultURI = if(nzchar(resultURI)) paste0(resultURI, "/robject") else "",
               symorpos = ".objinmem")
        ## only one thing in here since it's a new empty list backend
        resfs = listRecToFeatureSet(findRecords("*:*", db = tmpdb)[[1]])
        resfs@derivedFromFileID = uniqueID(rawfilefs)
        resfs@derivedFromFilePath = object
        record(resfs, code = code, symorpos = ".objinmem")
    }
    record(rawfilefs, code = "")
  
}
