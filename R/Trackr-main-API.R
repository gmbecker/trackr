#' @import roprov

#' @name record
#' @description These functions define the high-level, user-facing
#'     trackr API.
#' @details These functions allow end-users to interact with trackr
#'     databases. Each function does what its name suggests.
#' @title Primary high-level API functions for Trackr Databases
#' @param object ANY. An object to record. Or (for recordFiles) a
#'     vector of one or more files to record or the path of a single
#'     directory whose contents will be recorded. Or (for rmPlot) the
#'     unique ID of an object in the database to remove.
#' @param db TrackrDB. The database
#' @param resultURI A URI which can be used to define a
#'     grouping/hierarchy of results recorded via trackr. Currently
#'     unused by trackr itself except as additional metadata to search
#'     across. Defaults to an empty string.
#' @param code ANY. Code/evaluation history to be associated with
#'     \code{object}
#' @param force logical. Overwrite any existing entry matching
#'     \code{object}.  (default: FALSE)
#' @param verbose logical. Should extra informative messages be
#'     displayed ( default: FALSE)
#' @param symorpos The symbol or position corresponding to
#'     \code{object} in \code{code}. For normal usage this will not be
#'     required.
#' @param dryrun logical. Should a "dry run" be performed. If true,
#'     the \code{insert_record} and \code{trackr_write} steps of the
#'     record process are \emph{not} run, and the result of
#'     \code{prep_for_backend} is immediately returned.
#' @rdname main-api
#' @examples
#' prevtdb = defaultTDB()
#' defaultTDB(listTDB())
#' mt = datasets::mtcars
#' record(mt)
#'
#' res = findRecords("mtcars")
#' stopifnot(length(res) ==1)
#' rmRecord(mt)
#'
#' res = findRecords("mtcars")
#' stopifnot(length(res) == 0)
#' 
#' \dontrun{
#' f = function(paths) readLines(paths)
#' fil = system.file("test_docs", "knitr_test.Rmd", package = "trackr")
#' recordFiles(fil, ingestfun = f)
#'
#' res= findRecords("test_docs")
#' }
#' defaultTDB(prevtdb)
#' @export

record = function(object, db = defaultTDB(), resultURI = "", 
                  code = histry::histry_tracker(), force = FALSE,
                  verbose = FALSE, symorpos = NULL,
                  dryrun = FALSE) {
    
    if(verbose && dryrun)
        message("Performing dry run for submission. No records will be written to the backend.")
    
    provtable = roprov::ProvStoreDF()
    if(!is.null(code)) {
        if(is.null(symorpos))
            symorpos = substitute(object)
        
        if(is(code, "VirtHistoryTracker")) {
            provtable = histryProvDF(code)
            code = exprs(code)
        }
        if(is.code(code))
            code = deparse(code, control = NULL)
        else if (is(code, "list"))
            code = vapply(code, function(x) {
                if(is.code(x))
                    paste(deparse(x, control=NULL), collapse="\n")
                else
                    paste(x, collapse="\n")
            }, character(1))
        
        if(!is.null(code) && length(code) > 0 && any(nzchar(code))) {
            codescr = readScript(txt = code)
            codeinfo = getInputs(codescr, inputCollector(checkLibrarySymbols = TRUE))
            if(is.code(symorpos))
                symorpos = deparse(symorpos)
            codethread = getDependsThread(symorpos, codeinfo)
            ## do I Want to go back to code here, or keep as codeinfo?
            code = code[codethread] 
        } else {
            code = ""
        }
    } else {
        code = ""
    }
    
    pfs = makeFeatureSet(object, code = code, resultURI = resultURI, provtable = provtable)
    exst = trackr_lookup(pfs, target = db, exist = TRUE) # generic
    if(verbose) {
        if(exst)
            message("Existing record found for this object, id: ", uniqueID(pfs))
        else
            message("No existing record found for id ", uniqueID(pfs))
    }
    if(force || !exst || dryrun) {
        id = uniqueID(pfs)
        doc = prep_for_backend(pfs, target = db, verbose = verbose) #generic
        if(dryrun)
            return(doc)
        db = insert_record( object = doc, id = id, target = db, #generic
                           verbose = verbose)
        db = trackr_write(target = db) #generic
    }
    invisible(db)
}


#' @rdname main-api
#' @export
rmRecord = function(object, db = defaultTDB(), verbose = FALSE) {
    if(!is(object, "character")) {
        object = makeFeatureSet(object)
        id = uniqueID(object)
    } else {
        id = object
    }
    exst = trackr_lookup(id, target = db, exist = TRUE)

    if(!exst)
        warning("Entry specified for removal does not appear to exist in the database. Skipping.")
    else {
        db = remove_record(id, target = db, verbose = verbose)
        db = trackr_write(target = db, verbose = verbose)
    }
    invisible(db)
}


## findRecords is a direct call-down, but we still want to conceptually
## separate the user-facing and developer-facing interfaces.

#' @rdname main-api
#' @param pattern character. A regular expression to match against the text in \code{fields}
#' @param fields character or NULL. The fiends in which to match, or NULL to
#' include all fields.
#' @param ret_type character. Format in which to return the response. Options are:
#' "id" - id of matching documents (default), "list" - A list containing the
#' matching documents represnted as R lists, and "backend" - a backend specific
#' representation of the set of matching documents 
#' @export
findRecords = function(pattern, db = defaultTDB(), fields = NULL,  ret_type = c("doclist", "id", "backend"),
                    verbose = FALSE) {
    ret_type = match.arg(ret_type)
    trackr_search(pattern = pattern, fields = fields, target = db,
            ret_type = ret_type, verbose = verbose)
}




