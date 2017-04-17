##' @import rdocdb
NULL

## define necessary class unions

setClassUnion("characterOrNULL", c("character", "NULL"))
# setClassUnion("GGplotOrTrellis", c("ggplot", "trellis"))
setOldClass("sessionInfo")
## define S4 classes

setClassUnion("sinfoOrList", c("sessionInfo", "list"))

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
                                  trackrversion = "character")
                        )


.ObjFeatureSet <- setClass("ObjFeatureSet",
                           slots = c(klass = "character",
                                     object = "ANY"##,
                                     ##            user = "character",
                                     ##regdate = "POSIXct",
                                     ##uniqueid = "character",
                                     ##analysispkg = "list",
                                     ##tags = "character",
                                     ## currently only supported within RStudio
                                     ##analysisfile = "character",
                                     ## should be NA_character if not in RStudio
                                     ##rstudioproject = "character"
                                     ),
                           contains = "FeatureSet")



.DFFeatureSet <- setClass("DFFeatureSet",
                          slots = c(vars = "character",
                                    varclasses = "character",
                                    varsummaries = "list",
                                    varsummarydputs = "ANY",
                                    nobs = "numeric"),
                          contains = "ObjFeatureSet")


#' @name PlotFeatureSet-class
#' @title An S4 class called PlotFeatureSet
#' @slot data A list of data.frames containing the variables and observations used in the plot.
#' @slot titles Title and subtitle of the plot object; a named list of the form list(main = "My title", sub = "My subtitle").
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
#' @rdname PlotFeatureSet-class
#' @exportClass PlotFeatureSet
.PlotFeatureSet <- setClass("PlotFeatureSet",
    slots = c(titles = "characterOrNULL", data = "list",
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

#' @name GGplotFeatureSet-class
#' @title An S4 subclass of PlotFeatureSet with additional slots for objects of class ggplot.
#' @slot geom A named list of parameters for geometric objects in each layer of the plot.
#' @slot stat A named list of parameters for statistical transforms in each layer of the plot.
#' @slot position A named list of positioning information in each layer of the plot.
#' @slot num.layers An integer representing the number of layers in the plot.
#' @rdname GGplotFeatureSet-class
#' @exportClass GGplotFeatureSet
.GGplotFeatureSet <- setClass("GGplotFeatureSet",
    slots = c(geom = "list", stat = "list", position = "list", num.layers = "numeric"),
    contains = "PlotFeatureSet")

#' @name TrellisFeatureSet-class
#' @title An S4 subclass of PlotFeatureSet with additional slots for objects of class trellis.
#' @rdname TrellisFeatureSet-class
#' @exportClass TrellisFeatureSet
.TrellisFeatureSet <- setClass("TrellisFeatureSet",
    # slots = c(),
    contains = "PlotFeatureSet")

#' @name GraphicsFeatureSet-class
#' @title An S4 subclass of PlotFeatureSet with additional slots for objects of class recordedplot.
#' @rdname GraphicsFeatureSet-class
#' @exportClass GraphicsFeatureSet
.GraphicsFeatureSet <- setClass("GraphicsFeatureSet",
    # slots = c(),
    contains = "PlotFeatureSet")


.RmdFeatureSet <- setClass("RmdFeatureSet",
                           contains = "FeatureSet",
                           slots = c(chunks = "list",
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
#' @title Trackr configuration optionso
#' @exportClass TrackrOptions

setClass("TrackrOptions",
         slots = list(insert_delay = "numeric",
             img_dir = "character",
             img_ext = "character",
             backend_opts = "list"))

#' @rdname TrackrOptions-class
#' @param insert_delay numeric. delay in seconds between insertions.
#' @param img_dir character. Directory to save image files in.
#' @param img_ext character. extension to give image files.
#' @param backend_opts list. list of options specific to the backend. Currently ignored
#' by trackr machinery.
#' @export
TrackrOptions = function(insert_delay = 0,
                            img_dir = "./images",
                            img_ext = "png",
                            backend_opts = list(...),
                            ...) {
    new("TrackrOptions",
        insert_delay = insert_delay,
        img_dir = img_dir,
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
#' @param opts TrackrOptions object.
#' @param backend ANY. The backend to use.
#' @param ... ignored.
#' @export
TrackrDB = function(opts = TrackrOptions(...), backend = JSONBackend(), ...)
    new("TrackrDB", opts = opts, backend = backend)


#' @name JSONBackend-class
#' @title JSON backend for trackr
#' @slot data list. An in-memory list representation of the data in the db
#' @slot file character. The file containing the db ( to read from and write to)
#' @slot last_load POSIXct. The last time \code{data} was updated from disk.
#' @note This is a reference class, which does NOT have standard copy-on-write
#' semantics
#' @docType methods
#' @exportClass JSONBackend


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
#' @return A JSONBackend object, for use in creating a TrackrDB object.
#' @note This function should generally not be called directly by end-users.
#' See instead \code{\link{jsonTDB}}
#' @export
#' @importFrom RJSONIO fromJSON

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
setClass("RStudioExtras",
         representation(file = "character",
                        project = "character"),
         prototype = list(file = NA_character_,
                          project = NA_character_)
         )

listbackend = setRefClass("ListBackend", contains="DocCollectionRef")
ListBackend = function(lst = list()) {
    listbackend(docs = new("DocList", lst))
}
