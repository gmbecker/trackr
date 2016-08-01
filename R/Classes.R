##' @import rdocdb
NULL

## define necessary class unions

setClassUnion("characterOrNULL", c("character", "NULL"))
# setClassUnion("GGplotOrTrellis", c("ggplot", "trellis"))

## define S4 classes

.ObjFeatureSet <- setClass("ObjFeatureSet",
                           slots = c(klass = "character",
                                     object = "ANY",
                                     user = "character",
                                     regdate = "POSIXct",
                                     uniqueid = "character",
                                     analysispkg = "list",
                                     code = "Script",
                                     code.info = "ScriptInfo",
                                     isplot = "logical",
                                     tags = "character"))



.DFFeatureSet <- setClass("DFFeatureSet",
                          slots = c(vars = "character",
                                    varclasses = "character",
                                    varsummaries = "list",
                                    nobs = "integer"),
                          contains = "ObjFeatureSet")


#' @name PlotFeatureSet-class
#' @title An S4 class called PlotFeatureSet
#' @slot data A list of data.frames containing the variables and observations used in the plot.
#' @slot titles Title and subtitle of the plot object; a named list of the form list(main = "My title", sub = "My subtitle").
#' @slot var.labels Variable labels of the plot object; a named list of the form list(x = "X axis label", y = "Y axis label", groups = list(...)). Note that non-empty labels are character vectors and may contain more than one entry.
#' @slot annotation.text Annotation text of the plot object.
#' @slot var.types Variable types of the plot object; a named list of the form list(x = "numeric", y = "factor", ...).
#' @slot grouping Grouping information on the plot object; a named list.
#' @slot coord.sys A character vector describing the coordinate system employed in the plot.
#' @slot num.obs An integer representing the number of observations in the plotted data.
#' @slot has.legend A boolean indicating whether or not a legend is displayed in the plot.
#' @slot tags A character vector of user-defined tags.
#' @slot code R code to reproduce the plot, as a CodeDepends::Script object. May be empty.
#' @slot code.info Information about the R code to reproduce the plot, as a CodeDepends::ScriptInfo object. May be empty.
#' @rdname PlotFeatureSet-class
#' @exportClass PlotFeatureSet
.PlotFeatureSet <- setClass("PlotFeatureSet",
    slots = c(titles = "characterOrNULL", data = "list",
        var.labels = "list",
        annotation.text = "characterOrNULL", var.types = "list", 
        grouping = "list", coord.sys = "characterOrNULL", 
        num.obs = "numeric", has.legend = "logical",
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


#' @name ViztrackrOptions-class
#' @title Viztrackr configuration optionso
#' @exportClass ViztrackrOptions

setClass("ViztrackrOptions",
         slots = list(insert_delay = "numeric",
             img_dir = "character",
             img_ext = "character",
             backend_opts = "list"))

#' @rdname ViztrackrOptions-class
#' @param insert_delay numeric. delay in seconds between insertions.
#' @param img_dir character. Directory to save image files in.
#' @param img_ext character. extension to give image files.
#' @param backend_opts list. list of options specific to the backend. Currently ignored
#' by viztrackr machinery.
#' @export
ViztrackrOptions = function(insert_delay = 0,
                            img_dir = "./images",
                            img_ext = "png",
                            backend_opts = list(...),
                            ...) {
    new("ViztrackrOptions",
        insert_delay = insert_delay,
        img_dir = img_dir,
        img_ext = img_ext,
        backend_opts = backend_opts)
}

#'@name ViztrackrDB-class
#' @title Viztrackr database
#' @exportClass "ViztrackrDB"
#' @rdname ViztrackrDB-class


setClass("ViztrackrDB",
         slots = list(opts = "ViztrackrOptions",
             backend = "ANY")
         )

#' @rdname ViztrackrDB-class
#' @param opts ViztrackrOptions object.
#' @param backend ANY. The backend to use.
#' @param ... ignored.
#' @export
ViztrackrDB = function(opts = ViztrackrOptions(...), backend = VTJSONBackend(), ...)
    new("ViztrackrDB", opts = opts, backend = backend)


#' @name VTJSONBackend-class
#' @title JSON backend for viztrackr
#' @slot data list. An in-memory list representation of the data in the db
#' @slot file character. The file containing the db ( to read from and write to)
#' @slot last_load POSIXct. The last time \code{data} was updated from disk.
#' @note This is a reference class, which does NOT have standard copy-on-write
#' semantics
#' @docType methods
#' @exportClass VTJSONBackend

## vtjsonbackend = setRefClass("VTJSONBackend",
##             fields = list(.data = "list",
##                           data = function(val) {
##                 if(missing(val))
##                     return(.data)
##                 else
##                     .self$.data = val
##             },
##             .file = "character",
##             file =  function(val) {
##                 if(missing(val))
##                     return(.self$.file)
##                 else
##                     .self$.file = val
##             }),
##             contains)

vtjsonbackend = setRefClass("VTJSONBackend",
                            contains = "DocCollectionRef",
                            fields = list(
                                .file = "character",
                                file =  function(val) {
                                if(missing(val))
                                    return(.self$.file)
                                else
                                    .self$.file = val
                            }))

#' @name VTJSONBackend
#' @title JSON backend contstructor
#' @param file character. The json "database" to use as a viztrackr backend
#' @return A VTJSONBackend object, for use in creating a ViztrackrDB object.
#' @export
#' @importFrom RJSONIO fromJSON

VTJSONBackend = function(file = normalizePath("./viztrackr_db_data.json"), data = list()) {
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
    vtjsonbackend(docs = as(data, "DocList"), file = file)
}

