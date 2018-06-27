##' @importFrom tools md5sum
fileHash = function(fil) {
    paste0("MD5:",md5sum(fil))
}
    


recordFile = function(object, ingestfun = NULL, db = defaultTDB, resultURI = "",
                      code = histry_tracker(), force = FALSE,
                      verbose = FALSE, symorpos = NULL) {
    object = normalizePath(object)
    tmpdb= TrackrDB(backend= ListBackend(),
                    img_dir = img_dir(defaultTDB()))
    stopifnot(identical(names(formals(ingestfun)), "path"))
    funname = deparse(substitute(ingestfun))
    rawfilefs = RawFileFeatureSet(object)
    if(!is.null(ingestfun)) {
        newcode = paste0(".objinmem = ", funname, "( '", object, "' )")
        env = parent.frame()
        resobj = eval(parse(text = newcode), envir = env)
        code$addInfo(newcode, class(resobj), hash = fastdigest(resobj))
        record(resobj, code = code, db = tmpdb, resultURI = if(nzchar(resultURI)) paste0(resultURI, "/robject") else "", symorpos = ".objinmem")
        ## only one thing in here since it's a new empty list backend
        resfs = listRecToFeatureSet(findRecords("*:*", db = tmpdb)[[1]])
        resfs@derivedFromFileID = uniqueID(rawfilefs)
        resfs@derivedFromFilePath = object
        record(resfs, code = code, symorpos = ".objinmem")
    }
    record(rawfilefs, code = "")
  
}
