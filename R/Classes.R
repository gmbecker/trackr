##' @import lattice  methods histry CodeDepends rsolr fastdigest htmltools roprov
##' @importFrom graphics par plot.new plot.window text
##' @importFrom grDevices dev.off png pdf
##' @importFrom stats getCall nobs setNames
##' @importFrom utils capture.output compareVersion packageVersion sessionInfo str zip
NULL

## define necessary class unions

setClassUnion("characterOrNULL", c("character", "NULL"))
# setClassUnion("GGplotOrTrellis", c("ggplot", "trellis"))
setOldClass("sessionInfo")
## define S4 classes

setClassUnion("sinfoOrList", c("sessionInfo", "list"))

#' @title FeatureSet (and Sub)-Classes
#' @description Metadata inferred about R objects or dynamic documents is stored in
#' FeatureSet objects specific. Specific types of featuresets have specific additional
#' metadata they contain, beyond the standard metadata inferred about all results, and
#' represented by the core FeatureSet class.
#' @slot user character The user who submitted the result
#' @slot regdate POSIXct The date/time the result was recorded
#' @slot analysispkg list The R package associated with the result (because the
#' working directory was within the package's directory structure).
#' @slot uniqueid character The uniqueid of the result
#' @slot tags character Additional tags associated with the result
#' @slot analysisfile character The .R file active when there result was recorded (RStudio IDE only)
#' @slot rstudioproject character The RStudio project active when the result was recorded (RStudio IDE only)
#' @slot generatedin character The uniqueid of the Rmd file the result was generated in, if applicable
#' @slot code character The code used to generate the result (by default, as captured by histry)
#' @slot codeinfo ScriptInfo the ScriptInfo for the code
#' @slot sessioninfo sinfoOrList The session info at the time the result was recorded
#' @slot isplot logical Whether the result is a plot
#' @slot fsetklass character The FeatureSet subclass for the result
#' @slot trackrversion character The exact version of the trackr package used to record the result.
#' @slot clineargs character The commandline arguments passed to R when starting the session the result was recorded from
#' @slot resultURI character The URI associated with the result, see featureset constructor documentation.
#' @slot extrametadata list Any extra metadata associated with the result.
#' @slot titles Title and subtitle of the plot object; a named list of the form list(main = "My title", sub = "My subtitle").
#' @rdname featureset-classes
#' @exportClass FeatureSet
#' @aliases FeatureSet-class show,FeatureSet-method
.FeatureSet <- setClass("FeatureSet", contains="VIRTUAL",
                        slots = c(user = "character",
                                  regdate = "POSIXct",
                                  uniqueid = "character",
                                  analysispkg = "list",
                                  tags = "character",
                                  analysisfile = "character",
                                  rstudioproject = "character",
                                  generatedin = "character",
                                  code = "character",
                                  codeinfo = "ScriptInfo",
                                  sessioninfo = "sinfoOrList", 
                                  isplot = "logical",
                                  fsetklass = "character",
                                  trackrversion = "character",
                                  clineargs = "character",
                                  resultURI = "character",
                                  extramdata = "list",
                                  provtable = "ProvStoreDF",
                                  titles = "characterOrNULL")
                        )

#' @rdname featureset-classes
#' @exportClass ObjFeatureSet
#' @slot klass character The R object class of the result
#' @slot object ANY The object itself, or NULL if the object is not available.
#' @aliases ObjFeatureSet-class
#' @docType methods
.ObjFeatureSet <- setClass("ObjFeatureSet",
                           slots = c(klass = "character",
                                     object = "ANY",
                                     derivedFromFileID = "character",
                                     derivedFromFilePath = "character"),
                           contains = "FeatureSet")



#' @rdname featureset-classes
#' @exportClass DFFeatureSet
#' @slot vars character The variable names for a data.frame result
#' @slot varclasses character the variable classes for a data.frame result
#' @slot varsummaries list summaries for
#' @slot object ANY The object itself, or NULL if the object is not available.
#' @aliases DFFeatureSet-class
#' @docType methods
.DFFeatureSet <- setClass("DFFeatureSet",
                          slots = c(vars = "character",
                                    varclasses = "character",
                                    varsummaries = "list",
                                    varsummarydputs = "ANY",
                                    nobs = "numeric"),
                          contains = "ObjFeatureSet")

.RawFilesFeatureSet <-  setClass("RawFilesFeatureSet",
                                contains = "FeatureSet",
                                slots = c(path = "character",
                                          origfiles = "character"))

#' @slot data A list of data.frames containing the variables and observations used in the plot.
#' @slot varlabels Variable labels of the plot object; a named list of the form list(x = "X axis label", y = "Y axis label", groups = list(...)). Note that non-empty labels are character vectors and may contain more than one entry.
#' @slot annotation.text Annotation text of the plot object.
#' @slot vartypes Variable types of the plot object; a named list of the form list(x = "numeric", y = "factor", ...).
#' @slot grouping Grouping information on the plot object; a named list.
#' @slot coordsys A character vector describing the coordinate system employed in the plot.
#' @slot nobs An integer representing the number of observations in the plotted data.
#' @slot haslegend A boolean indicating whether or not a legend is displayed in the plot.
#' @slot tags A character vector of user-defined tags.
#' @slot code R code to reproduce the plot, as a CodeDepends::Script object. May be empty.
#' @slot codeinfo Information about the R code to reproduce the plot, as a CodeDepends::ScriptInfo object. May be empty.
#' @rdname featureset-classes
#' @exportClass PlotFeatureSet
#' @aliases PlotFeatureSet-class show,PlotFeatureSet-method
.PlotFeatureSet <- setClass("PlotFeatureSet",
                            slots = c(data = "list",
        varlabels = "list",
        annotationtext = "characterOrNULL",
        vartypes = "list", 
        grouping = "list",
        coordsys = "characterOrNULL", 
        nobs = "numeric",
        haslegend = "logical",
        package = "character"
        ),
    contains = "ObjFeatureSet",
    # eventually put something in here
    validity = function(object) {
        TRUE
    }
)

#' @slot geom A named list of parameters for geometric objects in each layer of the ggplot.
#' @slot stat A named list of parameters for statistical transforms in each layer of the ggplot.
#' @slot position A named list of positioning information in each layer of the ggplot.
#' @slot num.layers An integer representing the number of layers in the plot.
#' @rdname featureset-classes
#' @exportClass GGplotFeatureSet
#' @aliases GGplotFeatureSet-class show,GGplotFeatureSet-method
.GGplotFeatureSet <- setClass("GGplotFeatureSet",
    slots = c(geom = "list", stat = "list", position = "list", num.layers = "numeric"),
    contains = "PlotFeatureSet")

#' @rdname featureset-classes
#' @exportClass TrellisFeatureSet
#' @aliases TrellisFeatureSet-class
.TrellisFeatureSet <- setClass("TrellisFeatureSet",
    # slots = c(),
    contains = "PlotFeatureSet")

#' @rdname featureset-classes
#' @exportClass GraphicsFeatureSet
#' @aliases GraphicsFeatureSet-class
.GraphicsFeatureSet <- setClass("GraphicsFeatureSet",
    # slots = c(),
    contains = "PlotFeatureSet")

#' @rdname featureset-classes
#' @exportClass TrellisFeatureSet
#' @aliases RmdFeatureSet-class show,RmdFeatureSet-method
.RmdFeatureSet <- setClass("RmdFeatureSet",
                           contains = "FeatureSet",
                           slots = c(chunks = "character",
                                     numouts = "numeric",
                                     numplots = "numeric",
                                     title = "character",
                                     author = "character",
                                     textkeywords = "character",
                                     codekeywords = "character",
                                     inputfiles = "character",
                                     outputids = "character",
                                     fullcode = "character",
                                     ##its ScriptNodeInfo because we force it into single node and grab that info directly
                                     fullcodeinfo = "ScriptNodeInfo",
                                     rmdfile = "character",
                                     outfile = "character",
                                     rmdfileid = "character",
                                     figurefiles = "character"))



#' @name TrackrOptions-class
#' @description TrackrOptions objects dictate various behavior
#' by trackr when interacting with or preparing to interact with backends. See
#' individual parameters for behaviors controlled in this manner.
#' @title Trackr configuration options
#' @exportClass TrackrOptions

setClass("TrackrOptions",
         slots = list(insert_delay = "numeric",
             img_dir = "character",
             img_ext = "character",
             backend_opts = "list"))

#' @rdname TrackrOptions-class
#' @param insert_delay numeric. delay in seconds between insertions.
#' @param img_dir character. Directory to save image files in. This will be normalized via normalizePath
#' @param img_ext character. extension to give image files.
#' @param backend_opts list. list of options specific to the backend. Currently ignored
#' by trackr machinery.
#' @param \dots additional arguments, which are collected into a list for the default \code{backend_opts} value.
#' @export
TrackrOptions = function(insert_delay = 0,
                            img_dir = "./images",
                            img_ext = "png",
                            backend_opts = list(...),
                         ...) {
    new("TrackrOptions",
        insert_delay = insert_delay,
        img_dir = normalizePath(img_dir, mustWork=FALSE),
        img_ext = img_ext,
        backend_opts = backend_opts)
}


## We have the TrackrDB class, and thus a seemingly unnecessary additional
## level of abstraction because the Backends are sometimes Ref classes
## and we don't want to have users operating directly on non-idiotmatic
## objects. We could probably do away with this but I will leave it
## as is for now.

#'@name TrackrDB-class
#' @title Trackr database
#' @exportClass "TrackrDB"
#' @rdname TrackrDB-class
setClass("TrackrDB",
         slots = list(opts = "TrackrOptions",
             backend = "ANY")
         )

#' @rdname TrackrDB-class
#' @description A TrackrDB object is a combination of a backend and a
#' TrackrOptions object controlling trackr's behavior.
#' @param opts TrackrOptions object.
#' @param backend ANY. The backend to use.
#' @param ... ignored.
#' @export
TrackrDB = function(opts = TrackrOptions(...), backend = JSONBackend(), ...)
    new("TrackrDB", opts = opts, backend = backend)




#' @name DocCollectionRef
#' @title Reference to a DocCollection
#'
#' @description A reference class which carries around a
#'     DocCollection, suitable for use as a trackr backend.
#' @docType methods
#' @rdname DocCollection-refclasses
#' @exportClass DocCollectionRef
#' @aliases DocCollectionRef-class names<-,DocCollectionRef,ANY-method
#'     ndoc,DocCollectionRef-method
.docrefclass = setRefClass("DocCollectionRef",
            fields = list(docs = "DocCollection"))




#' @name JSONBackend
#' @description A JSON-file based backend. This is the default backend in trackr.
#' @title JSON backend for trackr
#' @slot data list. An in-memory list representation of the data in the db
#' @slot file character. The file containing the db ( to read from and write to)
#' @slot last_load POSIXct. The last time \code{data} was updated from disk.
#' @note This is a reference class, which does NOT have standard copy-on-write
#' semantics
#' @docType methods
#' @exportClass JSONBackend
#' @aliases JSONBackend-class
#' @rdname JSONBackend

jsonbackend = setRefClass("JSONBackend",
                            contains = "DocCollectionRef",
                            fields = list(
                                .file = "character",
                                file =  function(val) {
                                if(missing(val))
                                    return(.self$.file)
                                else
                                    .self$.file = val
                            }))


#' @name JSONBackend
#' @title JSON backend contstructor
#' @param file character. The json "database" to use as a trackr backend
#' @param data A list of records to pre-populate the backend with.
#' @return A JSONBackend object, for use in creating a TrackrDB object.
#' @note This function should generally not be called directly by end-users.
#' See instead \code{\link{jsonTDB}}
#' @export
#' @importFrom RJSONIO fromJSON
#' @rdname JSONBackend

JSONBackend = function(file = normalizePath("./trackr_db_data.json"), data = list()) {
    if(file.exists(file)) {
        data = fromJSON(file)
        ll = Sys.time()
        if (is.null(names(data)))
            names(data) = sapply(data, function(x) x$id)
    } else if (length(data) > 0 ) {
        if(!dir.exists(dirname(file)))
            dir.create(dirname(file), recursive=TRUE)
        toJSON(data, file = file)
    }
    jsonbackend(docs = as(data, "DocList"), file = file)
}


#' @name RStudioExtras
#' @title RStudioExtras class
#'
#' @description A class to contain information about the current session when
#' working inside the RStudio IDE. 
#' @docType methods
#' @exportClass RStudioExtras
#' @aliases RStudioExtras-class
setClass("RStudioExtras",
         representation(file = "character",
                        project = "character"),
         prototype = list(file = NA_character_,
                          project = NA_character_)
         )




#' @rdname DocCollection-refclasses
#' @exportClass ListBackend
#' @aliases ListBackend-class
listbackend = setRefClass("ListBackend", contains="DocCollectionRef")
#' @rdname DocCollection-refclasses
#' @param lst List of documents to populate the list backend with.
#' @export
#' @aliases ListBackend
ListBackend = function(lst = list()) {
    listbackend(docs = new("DocList", lst))
}


