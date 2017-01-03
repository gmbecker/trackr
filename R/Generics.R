#' @include Classes.R
NULL

## Generics of constructor methods --------------------------------------------

#' @name makeFeatureSet
#' @title Construct an ObjFeatureSet.
#' @param object The plot to summarize via metadata in an ObjFeatureSet (or subclass)
#' @param ... Other named arguments that become slots in the new PlotFeatureSet.
#' @return PlotFeatureSet S4 object containing a ObjFeatureSet object and extracted plot features
#' @export
#' @examples
#' library(ggplot2)
#' pg <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' pfs <- makeFeatureSet(pg)
#'
#' library(lattice)
#' pl <- barchart(Class ~ Freq | Sex + Age, data = as.data.frame(Titanic), groups = Survived, stack = TRUE, layout = c(4, 1), auto.key = list(title = "Survived", columns = 2))
#' pfs <- makeFeatureSet(pl)
#'
#' dffs <- makeFeatureSet(as.data.frame(Titanic))
#'
#'\dontrun{
#' plot(1:10, 1:10)
#' pb <- recordPlot()
#' pfs <- makeFeatureSet(pb)
#'}
setGeneric(name = "makeFeatureSet", 
    def = function(object, ...) {
        standardGeneric("makeFeatureSet")
    }
)

## Generics of utility methods for ObjFeatureSet and PlotFeatureSet -------------

# if (!isGeneric("as.data.frame")) {
#     setGeneric(name = "as.data.frame", 
#         def = function(object, row.names = NULL, optional = FALSE, ...) {
#             standardGeneric("as.data.frame")
#         }
#     )
# }

#' @name saveEnrichedPlot
#' @title Save the plot object owned by an object of class/superclass PlotFeatureSet as an image with metadata.
#' @param object An object of (super)class PlotFeatureSet.
#' @param filename The full filename, including path, where the plot should be saved.
#' @param ... Other named arguments passed on to the class-specific basic image save method.
#' @return NULL
#' @rdname enrichedPlot-methods
#' @export
setGeneric(name = "saveEnrichedPlot",
    def = function(object, filename, ...) {
        standardGeneric("saveEnrichedPlot")
    }
)

#' @name saveBasicPlot
#' @title Save the plot object owned by an object of class/superclass PlotFeatureSet as an image.
#' @param object An object of (super)class PlotFeatureSet or plot object of class ggplot or trellis.
#' @param filename The full filename, including path, where the plot should be saved.
#' @param type The type of image to save. Default is "png"; should be one of "jpeg", "jpg", "tiff", "tif", "png", or "bmp". In future versions, this should be automatically determined from the filename.
#' @param width Width of output, in inches
#' @param height Height of output, in inches
#' @param dpi Resolution of output, in pixels per inch
#' @param ... Other named arguments passed on to the class-specific save method.
#' @return A boolean indicating success of the save operation.
#' @export
setGeneric(name = "saveBasicPlot",
    def = function(object, filename, ...) {
        standardGeneric("saveBasicPlot")
    }
)

#' @name selectPlot
#' @title Search the features of a PlotFeatureSet.
#' @param object A PlotFeatureSet.
#' @param terms The terms for which the algorithm should search.
#' @param ... Other named arguments.
#' @return A boolean indicating whether the terms were found.
#' @rdname selectPlot-methods
#' @export
setGeneric(name = "selectPlot",
    def = function(object, terms, ...) {
        standardGeneric("selectPlot")
    }
)

## #' @name addToDataStore
## #' @title Add a PlotFeatureSet to a data store like a Solr core/index.
## #' @param object An object of class PlotFeatureSet, ggplot, or trellis.
## #' @param data.store An object representing the data store to which the object should be added.  Currently implemented options: rsolr::SolrList.
## #' @param ... Other named arguments.
## #' @return An updated data store object.
## #' @rdname addToDataStore
## #' @export
## setGeneric(name = "addToDataStore",
##     def = function(object, data.store, ...) {
##         standardGeneric("addToDataStore")
##     }
## )

## #' @name searchDataStore
## #' @title Search a database of plot objects.
## #' @param q A character vector representing a search query. Multiple queries are combined with an OR operator.
## #' @param data.store An object representing the data store that should be searched.  Currently implemented options: rsolr::SolrList.
## #' @param ... Other named arguments.
## #' @return A data.frame (or data-store-specific analogue) of results
## #' @rdname searchDataStore
## #' @export
## setGeneric(name = "searchDataStore",
##     def = function(q, data.store, ...) {
##         standardGeneric("searchDataStore")
##     }
## )

## #' @name removeFromDataStore
## #' @title Remove an object from a data store like a Solr core/index.
## #' @param object An object of class PlotFeatureSet or a character vector corresponding to a unique ID of an object in the data store.
## #' @param data.store An object representing the data store from which the object should be removed.  Currently implemented options: rsolr::SolrList.
## #' @param ... Other named arguments.
## #' @return An updated data store object.
## #' @rdname removeFromDataStore
## #' @export
## setGeneric(name = "removeFromDataStore",
##     def = function(object, data.store, ...) {
##         standardGeneric("removeFromDataStore")
##     }
## )

#' @name parseCode
#' @title Parse plot creation code into a CodeDepends::Script.
#' @param code An expression, call or character vector, or a PlotFeatureSet or plot object. If missing, this method will attempt to extract code out of the plot object.
#' @return A CodeDepends::Script object.
#' @rdname parseCode
setGeneric(name = "parseCode",
    def = function(code) {
        standardGeneric("parseCode")
    }
)

## Generics of mutator methods for ObjFeatureSet and PlotFeatureSet -------------

#' @name uniqueID<-
#' @title Mutator method for "uniqueid" slot of ObjFeatureSet (or one of its subclasses).
#' @param object An object of (super)class ObjFeatureSet.
#' @param value A character vector serving as the unique ID of the plot object in the database.
#' @return A modified object of (super)class ObjFeatureSet.
#' @rdname uniqueID-methods
#' @export
setGeneric(name = "uniqueID<-",
    def = function(object, value) {
        standardGeneric("uniqueID<-")
    }
)

#' @name user<-
#' @title Mutator method for "user" slot of ObjFeatureSet (or one of its subclasses).
#' @param object An object of (super)class ObjFeatureSet.
#' @param value A character vector serving as the username associated with the plot object in the database.
#' @return A modified object of (super)class ObjFeatureSet.
#' @rdname user-methods
#' @export
setGeneric(name = "user<-",
    def = function(object, value) {
        standardGeneric("user<-")
    }
)

#' @name regDateTime<-
#' @title Mutator method for "regdate" slot of ObjFeatureSet (or one of its subclasses).
#' @param object An object of (super)class ObjFeatureSet.
#' @param value  A POSIXct timestamp representing the moment when the plot was registered.
#' @return A modified object of (super)class ObjFeatureSet.
#' @rdname regDateTime-methods
#' @export
setGeneric(name = "regDateTime<-",
    def = function(object, value) {
        standardGeneric("regDateTime<-")
    }
)

# setGeneric(name = "graphSys<-",
#     def = function(object, value) {
#         standardGeneric("graphSys<-")
#     }
# )

#' @name titles<-
#' @title Mutator method for "titles" slot of PlotFeatureSet (or one of its subclasses).
#' @param object An object of (super)class PlotFeatureSet. For accession, also plot objects of class ggplot or trellis.
#' @param value A named list of titles of the form list(main = "My title", sub = "My subtitle").
#' @return A modified object of (super)class PlotFeatureSet.
#' @rdname titles-methods
#' @export
setGeneric(name = "titles<-",
    # signature = "object",
    def = function(object, value) {
        standardGeneric("titles<-")
    }
)

#' @name objCode<-
#' @title Mutator method for "code" slot of PlotFeatureSet (or one of its subclasses).
#' @param object An object of (super)class PlotFeatureSet. For accession, also plot objects of class ggplot or trellis.
#' @param value A CodeDepends::Script object.
#' @return A modified object of (super)class PlotFeatureSet.
#' @rdname objCode-methods
#' @export
setGeneric(name = "objCode<-",
    # signature = "object",
    def = function(object, value) {
        standardGeneric("objCode<-")
    }
)

#' @name codeInfo<-
#' @title Mutator method for "codeinfo" slot of PlotFeatureSet (or one of its subclasses).
#' @param object An object of (super)class PlotFeatureSet. For accession, also plot objects of class ggplot or trellis.
#' @param value A CodeDepends::ScriptInfo object.
#' @return A modified object of (super)class PlotFeatureSet.
#' @rdname codeInfo-methods
#' @export
setGeneric(name = "codeInfo<-",
    # signature = "object",
    def = function(object, value) {
        standardGeneric("codeInfo<-")
    }
)

#' @name dataLabels<-
#' @title Mutator method for PlotFeatureSet "varlabels" slot.
#' @param object An object of (super)class PlotFeatureSet. For accession, also plot objects of class ggplot or trellis.
#' @param value A named list of variable labels of the form list(x = "X axis label", y = "Y axis label", ...).
#' @return A modified object of (super)class PlotFeatureSet.
#' @rdname dataLabels-methods
#' @export
setGeneric(name = "dataLabels<-",
    def = function(object, value) {
        standardGeneric("dataLabels<-")
    }
)

# setGeneric(name = "dataTypes<-",
#     def = function(object, value) {
#         standardGeneric("dataTypes<-")
#     }
# )

# setGeneric(name = "annotationText<-",
#     def = function(object, value) {
#         standardGeneric("annotationText<-")
#     }
# )

#' @name geomObject<-
#' @title Mutator method for GGplotFeatureSet "geom" slot.
#' @param object An object of class GGplotFeatureSet. For accession, also plot objects of class ggplot.
#' @param value A named list of parameters for geometric objects in each layer of the plot.
#' @return A modified object of class GGplotFeatureSet.
#' @rdname geomObject-methods
#' @export
setGeneric(name = "geomObject<-",
    def = function(object, value) {
        standardGeneric("geomObject<-")
    }
)

#' @name statTransform<-
#' @title Mutator method for GGplotFeatureSet "stat" slot.
#' @param object An object of class GGplotFeatureSet. For accession, also plot objects of class ggplot.
#' @param value A named list of parameters for statistical transforms in each layer of the plot.
#' @return A modified object of class GGplotFeatureSet.
#' @rdname statTransform-methods
#' @export
setGeneric(name = "statTransform<-",
    def = function(object, value) {
        standardGeneric("statTransform<-")
    }
)

# setGeneric(name = "panelInfo<-",
#     def = function(object, value) {
#         standardGeneric("panelInfo<-")
#     }
# )

# setGeneric(name = "nObs<-",
#     def = function(object, value) {
#         standardGeneric("nObs<-")
#     }
# )

#' @name nLayers<-
#' @title Mutator method for GGplotFeatureSet "num.layers" slot.
#' @param object An object of class GGplotFeatureSet. For accession, also plot objects of class ggplot.
#' @param value An integer representing the number of layers in the plot.
#' @return A modified object of class GGplotFeatureSet.
#' @rdname nLayers-methods
#' @export
setGeneric(name = "nLayers<-",
    def = function(object, value) {
        standardGeneric("nLayers<-")
    }
)

#' @name position<-
#' @title Mutator method for GGplotFeatureSet "position" slot.
#' @param object An object of class GGplotFeatureSet. For accession, also plot objects of class ggplot.
#' @param value A named list of positioning information in each layer of the plot.
#' @return A modified object of class GGplotFeatureSet.
#' @rdname position-methods
#' @export
setGeneric(name = "position<-",
    def = function(object, value) {
        standardGeneric("position<-")
    }
)

# setGeneric(name = "coordSystem<-",
#     def = function(object, value) {
#         standardGeneric("coordSystem<-")
#     }
# )

#' @name tags<-
#' @title Mutator method for PlotFeatureSet "tags" slot.
#' @param object An object of (super)class PlotFeatureSet or a plot object of class ggplot or trellis.
#' @param value A character vector of tags.
#' @param ... Additional named arguments.
#' @return A modified object of (super)class PlotFeatureSet.
#' @rdname tags-methods
#' @export
setGeneric(name = "tags<-",
    def = function(object, value) {
        standardGeneric("tags<-")
    }
)

#' @name editTags
#' @title Mutator method for PlotFeatureSet "tags" slot.
#' @param option One of c("replace", "add", "remove"), with default "replace" (equivalent to "tags<-" with an additional uniqueness check), describing the action to be taken with the tag vector provided.
#' @return A modified object of (super)class PlotFeatureSet.
#' @rdname tags-methods
#' @export
setGeneric(name = "editTags",
    def = function(object, value, option) {
        standardGeneric("editTags")
    }
)

## Generics of accessor methods for ObjFeatureSet and PlotFeatureSet ------------

#' @name uniqueID
#' @title Accessor method for ObjFeatureSet "uniqueid" slot.
#' @return A character vector serving as the unique ID of the plot object in the database.
#' @rdname uniqueID-methods
#' @export
setGeneric(name = "uniqueID", valueClass = "character", 
    def = function(object) {
        standardGeneric("uniqueID")
    }
)

#' @name user
#' @title Accessor method for ObjFeatureSet "user" slot.
#' @return A character vector serving as the username associated with the plot object in the database.
#' @rdname user-methods
#' @export
setGeneric(name = "user", valueClass = "character", 
    def = function(object) {
        standardGeneric("user")
    }
)

#' @name regDateTime
#' @title Accessor method for ObjFeatureSet "regdate" slot.
#' @return A POSIXct timestamp representing the moment when the plot was registered.
#' @rdname regDateTime-methods
#' @export
setGeneric(name = "regDateTime",
    def = function(object) {
        standardGeneric("regDateTime")
    }
)

#' @name graphSys
#' @title Accessor method for PlotFeatureSet "package" slot.
#' @param object An object of (super)class PlotFeatureSet.
#' @return A character vector representing the originating R package of the plot object.
#' @rdname graphSys-methods
#' @export
setGeneric(name = "graphSys",
    def = function(object) {
        standardGeneric("graphSys")
    }
)

#' @name describePackage
#' @title Accessor method for ObjFeatureSet "analysispkg" slot.
#' @param object An object of (super)class ObjFeatureSet.
#' @return A list holding the name, title, and description of any packages with description files on the current path.
#' @rdname describePackage-methods
#' @export
setGeneric(name = "describePackage",
    def = function(object) {
        standardGeneric("describePackage")
    }
)

#' @name titles
#' @title Accessor method for the title and subtitle (slot "titles") of the plot object.
#' @return A named list of titles of the form list(main = "My title", sub = "My subtitle").
#' @rdname titles-methods
#' @export
setGeneric(name = "titles",
    def = function(object) {
        standardGeneric("titles")
    }
)

#' @name objCode
#' @title Accessor method for the Script (slot "code") of the plot object.
#' @return A CodeDepends::Script object
#' @rdname objCode-methods
#' @export
setGeneric(name = "objCode",
    def = function(object) {
        standardGeneric("objCode")
    }
)

#' @name codeInfo
#' @title Accessor method for the Script (slot "codeinfo") of the plot object.
#' @return A CodeDepends::ScriptInfo object
#' @rdname codeInfo-methods
#' @export
setGeneric(name = "codeInfo",
    def = function(object) {
        standardGeneric("codeInfo")
    }
)

#' @name fullData
#' @title Accessor method for the data (slot "data") that is used in plotting.
#' @param object An object of (super)class PlotFeatureSet. For accession, also plot objects of class ggplot or trellis.
#' @param quiet Should warnings about plotted data be supressed?
#' @return A list of data.frame containing the variables used in plotting.
#' @rdname fullData-methods
#' @export
setGeneric(name = "fullData",
    def = function(object, quiet = FALSE) {
        standardGeneric("fullData")
    }
)

#' @name dataLabels
#' @title Accessor method for variable labels (slot "varlabels") of the plot object.
#' @return A named list of variable labels of the form list(x = "X axis label", y = "Y axis label", ...).
#' @rdname dataLabels-methods
#' @export
setGeneric(name = "dataLabels",
    def = function(object) {
        standardGeneric("dataLabels")
    }
)

#' @name dataNames
#' @title Accessor method for the variable names of the plot object. Not an exported method.
#' @param object An object of (super)class PlotFeatureSet. For accession, also plot objects of class ggplot or trellis.
#' @return A named list of variable names of the form list(x = "x.name", y = "y.name", groups = list(...)).
setGeneric(name = "dataNames",
    def = function(object) {
        standardGeneric("dataNames")
    }
)

#' @name dataTypes
#' @title Accessor method for the variable types (slot "vartypes") of the plot object.
#' @param object An object of class PlotFeatureSet, GGplotFeatureSet, TrellisFeatureSet, ggplot, or trellis.
#' @return A named list of variable types of the form list(x = "numeric", y = "factor", ...).
#' @rdname dataTypes-methods
#' @export
setGeneric(name = "dataTypes",
    def = function(object) {
        standardGeneric("dataTypes")
    }
)

#' @name annotationText
#' @title Accessor method for the annotation text (slot "annotationtext") of the plot object.
#' @param object An object of class  PlotFeatureSet, GGplotFeatureSet, TrellisFeatureSet, ggplot, or trellis.
#' @return A character vector of text annotations appearing on the plot.
#' @rdname annotationText-methods
#' @export
setGeneric(name = "annotationText",
    def = function(object) {
        standardGeneric("annotationText")
    }
)

#' @name geomObject
#' @title Accessor method for the geometric object(s) (slot "geom") of the plot object.
#' @return A named list of parameters for geometric objects in each layer of the plot.
#' @rdname geomObject-methods
#' @export
setGeneric(name = "geomObject",
    def = function(object) {
        standardGeneric("geomObject")
    }
)

#' @name statTransform
#' @title Accessor method for the statistical transform(s) (slot "stat") of the plot object.
#' @return A named list of parameters for statistical transforms in each layer of the plot.
#' @rdname statTransform-methods
#' @export
setGeneric(name = "statTransform",
    def = function(object) {
        standardGeneric("statTransform")
    }
)

#' @name groupInfo
#' @title Accessor method for the panel and other grouping information (slot "grouping") of the plot object.
#' @param object An object of class  PlotFeatureSet, GGplotFeatureSet, TrellisFeatureSet, ggplot, or trellis.
#' @return A named list of grouping information parameters.
#' @rdname groupInfo-methods
#' @export
setGeneric(name = "groupInfo",
    def = function(object) {
        standardGeneric("groupInfo")
    }
)

#' @name nLayers
#' @title Accessor method for the number of layers (slot "num.layers") of the plot object.
#' @return An integer representing the number of layers in the plot.
#' @rdname nLayers-methods
#' @export
setGeneric(name = "nLayers",
    def = function(object) {
        standardGeneric("nLayers")
    }
)


#' @name position
#' @title Accessor method for the positioning information (slot "position") of the plot object.
#' @return A named list of positioning information in each layer of the plot.
#' @rdname position-methods
#' @export
setGeneric(name = "position",
    def = function(object) {
        standardGeneric("position")
    }
)

#' @name coordSystem
#' @title Accessor method for the coordinate system (slot "coordsys") of the plot object. 
#' @param object An object of class  PlotFeatureSet, GGplotFeatureSet, TrellisFeatureSet, ggplot, or trellis.
#' @return A character vector describing the coordinate system employed in the plot.
#' @rdname coordSystem-methods
#' @export
setGeneric(name = "coordSystem",
    def = function(object) {
        standardGeneric("coordSystem")
    }
)

#' @name hasLegend
#' @title Accessor method for the legend indicator (slot "haslegend") of the plot object. 
#' @param object An object of class PlotFeatureSet, GGplotFeatureSet, TrellisFeatureSet, ggplot, or trellis.
#' @return A boolean indicating whether or not a legend is displayed in the plot.
#' @rdname hasLegend-methods
#' @export
setGeneric(name = "hasLegend",
    def = function(object) {
        standardGeneric("hasLegend")
    }
)

#' @name nObs
#' @title Accessor method for the number of observations (slot "nobs") of the plot object. 
#' @param object An object of class PlotFeatureSet, GGplotFeatureSet, TrellisFeatureSet, ggplot, or trellis.
#' @return An integer representing the number of observations in the plotted data.
#' @rdname nObs-methods
#' @export
setGeneric(name = "nObs",
    def = function(object) {
        standardGeneric("nObs")
    }
)

#' @name tags
#' @title Accessor method for the tags (slot "tags") of the plot object.
#' @return A character vector of user-defined tags.
#' @rdname tags-methods
#' @export
setGeneric(name = "tags",
    def = function(object) {
        standardGeneric("tags")
    }
)


## Low-level backend interface API

#' @name trackr_lookup
#' @title Backend-interface API
#'
#' This page describes the API which must be implimented by all Trackr backends
#' Any class with methods for these generic can be used in the \code{backend}
#' slot of a \code{TrackrDB} object.
#'
#' Methods should be written to be dispatched \code{backend}, and, where appropriate
#' on code{object}/code{doc} 
#' 
#' @param object ANY. The object to lookup, add, remove, etc.
#' @param target ANY. The backend of the TrackrDB instance.
#' @param opts TrackrOptions. The trackr-level options. Typically
#' extracted from \code{target} in a \code{TrackrDB} method and passed down.
#' @param exist logical. Return TRUE/FALSE rather than the looked-up object.
#' (default: FALSE)
#'
#' @return for \code{trackr_lookup}: If \code{exist} is TRUE, a logical indicating whether \code{object} was
#' found in \code{db}. Otherwise, the object stored in the database (or NULL if it
#' was not found).
#' @rdname interface-api
#' @docType methods
#' @export
setGeneric("trackr_lookup", function(object, target, opts, 
                                 exist = FALSE) standardGeneric("trackr_lookup"))

#' @name insert_record
#'
#' @param verbose logical. Should extra informative messages be displayed (
#' default: FALSE)
#' @rdname interface-api
#' @return for \code{insert_record} and \code{remove_record}: The \code{TrackrDB}
#' (\code{db} parameter) after the plot has been added or removed.
#' @details \code{insert_record} may or may not involve writing to
#' disk, which can alternatively occur during \code{trackr_write}. Writing,
#' if any is desired, must occur within at one and only one of these methods. If
#' \code{insert_record} performs the writing, \code{trackr_write} should be a no-op.
#' @export
setGeneric("insert_record", function(object, id, target, opts, verbose = FALSE)
    standardGeneric("insert_record"), signature = "target")

#' @name prep_for_backend
#' @rdname interface-api
#' @return For \code{prep_for_backend}, \code{object}, representend in the form
#' that the \code{insert_record} method for \code{backend} expects.
#' @export
setGeneric("prep_for_backend", function(object, target, opts,
                                     verbose = FALSE)
    standardGeneric("prep_for_backend"))

#' @name remove_record
#' @note \code{remove_record} should have the same writing behavior
#' as \code{insert_record}
#' @rdname interface-api
#' @export
setGeneric("remove_record", function(object, target, opts, verbose = FALSE)
    standardGeneric("remove_record"))


#' @name remove_record
#' @note \code{remove_record} should have the same writing behavior
#' as \code{insert_record}
#' @rdname interface-api
#' @export
setGeneric("trackr_write", function(target, opts, verbose = FALSE) standardGeneric("trackr_write"))




#' @name trackr_search
#' @rdname interface-api
#' @param pattern character. A regular expression to match against the text in \code{fields}
#' @param fields character or NULL. The fiends in which to match, or NULL to
#' include all fields.
#' @param ret_type character. Format in which to return the response. Options are:
#' "id" - id of matching documents (default), "doclist" - A list containing the
#' matching documents represnted as R lists, and "backend" - a backend specific
#' representation of the set of matching documents (generally the same class
#' as \code{backend}.
#' @export
setGeneric("trackr_search", function(pattern, target, opts, 
                               fields = NULL,
                               ret_type = c("doclist", "id", "backend"),
                               verbose = TRUE)
    standardGeneric("trackr_search"))



setGeneric("generateTags", function(object) standardGeneric("generateTags"))


