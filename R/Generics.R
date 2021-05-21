#' @include Classes.R
NULL

## Generics of constructor methods --------------------------------------------

#' @name makeFeatureSet
#' @description The generic function for transforming an R object into the
#' appropriate subclass of FeatureSet. This includes most introspection-based
#' metadata extraction from the object. Metadata extraction can be customized
#' at the package/R sesion level for specific classes of objects by
#' defining methods for this generic.
#'
#' @title Construct an ObjFeatureSet.
#' @param object The plot to summarize via metadata in an
#'     ObjFeatureSet (or subclass)
#' @param ... Other named arguments that become slots in the new
#'     PlotFeatureSet.
#' @return PlotFeatureSet S4 object containing a ObjFeatureSet object
#'     and extracted plot features
#' @docType methods
#' @export
#' @examples
#' library(ggplot2)
#' mt = datasets::mtcars
#' pg <- ggplot(mt, aes(wt, mpg)) + geom_point()
#' pfs <- makeFeatureSet(pg)
#'
#' library(lattice)
#' titan <- datasets::Titanic
#' pl <- barchart(Class ~ Freq | Sex + Age, data = as.data.frame(titan),
#'                groups = Survived, stack = TRUE, layout = c(4, 1),
#'                auto.key = list(title = "Survived", columns = 2))
#' pfs <- makeFeatureSet(pl)
#'
#' dffs <- makeFeatureSet(as.data.frame(titan))
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

#' @name saveBasicPlot
#' @description Save a plot to an image file as part of the record
#'     process. This generally shouldn't need to be overwritten.
#' @title Save the plot object owned by an object of class/superclass
#'     PlotFeatureSet as an image.
#' @param object An object of (super)class PlotFeatureSet or plot
#'     object of class ggplot or trellis.
#' @param filename The full filename, including path, where the plot
#'     should be saved.
#' @param type The type of image to save. Default is "png"; should be
#'     one of "jpeg", "jpg", "tiff", "tif", "png", or "bmp". In future
#'     versions, this should be automatically determined from the
#'     filename.
#' @param width Width of output, in inches
#' @param height Height of output, in inches
#' @param dpi Resolution of output, in pixels per inch
#' @param ... Other named arguments passed on to the class-specific
#'     save method.
#' @return A boolean indicating success of the save operation.
setGeneric(name = "saveBasicPlot",
    def = function(object, filename, ...) {
        standardGeneric("saveBasicPlot")
    }
)


#' @name parseCode
#' @title Parse result creation code
#' @description parse code into a CodeDepends::Script
#' @param code An expression, call or character vector, or a PlotFeatureSet or plot object. If missing, this method will attempt to extract code out of the plot object.
#' @return A CodeDepends::Script object.
#' @rdname parseCode
#' @export
setGeneric(name = "parseCode",
    def = function(code) {
        standardGeneric("parseCode")
    }
)

## Generics of mutator methods for ObjFeatureSet and PlotFeatureSet -------------

#' @name uniqueID<-
#' @description Get or set the uniqueID field of a FeatureSet. This should not be called directly by end users.
#' @title Accessor methods for "uniqueid" of FeatureSet objects
#' @param object An object of (super)class ObjFeatureSet.
#' @param value A character vector serving as the unique ID of the plot object in the database.
#' @return A modified object of (super)class ObjFeatureSet.
#' @rdname uniqueID-methods
#' @docType methods
#' @export
setGeneric(name = "uniqueID<-",
    def = function(object, value) {
        standardGeneric("uniqueID<-")
    }
)

#' @name user<-
#' @title Accessors for the "user" of FeatureSet objects
#' @description Get or set the user associated with a FeatureSet object. This should not be called directly.
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
#' @title Accessors for the registration Date/Time of FeatureSet objects
#' @description Get or set the registration time associated with a FeatureSet object. This should not be called directly.
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

#' @name titles<-
#' @title Accessors for the title(s) of FeatureSet objects
#' @description Get or set the titles associated with a FeatureSet object. This should not be called directly.
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
#' @title Accessors for the code of FeatureSet objects
#' @description Get or set the code associated with a FeatureSet object. This should not be called directly.
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
#' @title Accessors for the code analysis information of FeatureSet objects
#' @description Get or set the analysis information for the code associated with a FeatureSet object. This should not be called directly.
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
#' @title Accessors for the axis labels PlotFeatureSet objects
#' @description Get or set the axis label information associated with a PlotFeatureSet object. This should not be called directly.
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
#' @title Accessors for the geom of GGplotFeatureSet objects
#' @description Get or set the geom associated with a GGplotFeatureSet object. This should not be called directly.
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
#' @title Accessors for the stat(s) of GGplotFeatureSet objects
#' @description Get or set the stat transform(s)  associated with a GGplotFeatureSet object. This should not be called directly.
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
#' @title Accessors for the number of layers of GGplotFeatureSet objects
#' @description Get or set the number of layers associated with a GGplotFeatureSet object. This should not be called directly.
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
#' @title Accessors for the position of GGplotFeatureSet objects
#' @description Get or set the position associated with a GGplotFeatureSet object. This should not be called directly.
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
#' @title Accessors for the tags of FeatureSet objects
#' @description Get or set the tags associated with a FeatureSet object. This should not be called directly.
#' @title Mutator method for PlotFeatureSet "tags" slot.
#' @param object An object of (super)class FeatureSet
#' @param value A character vector of tags.
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
#' @rdname uniqueID-methods
#' @export
#' @aliases uniqueID,ANY-method
setGeneric(name = "uniqueID", valueClass = "character",
    def = function(object) {
        standardGeneric("uniqueID")
    }
)

#' @name user
#' @rdname user-methods
#' @examples
#' mt = datasets::mtcars
#' fs = makeFeatureSet(mt)
#' user(fs)
#' @export
setGeneric(name = "user", valueClass = "character",
    def = function(object) {
        standardGeneric("user")
    }
)

#' @name regDateTime
#' @rdname regDateTime-methods
#' @examples
#' mt = datasets::mtcars
#' fs = makeFeatureSet(mt)
#' regDateTime(fs)
#' @export
setGeneric(name = "regDateTime",
    def = function(object) {
        standardGeneric("regDateTime")
    }
)

#' @name graphSys
#' @description Retrieve the graphics system from a PlotFeatureSet object
#' @title Accessor method for PlotFeatureSet "package" slot.
#' @param object An object of (super)class PlotFeatureSet.
#' @return A character vector representing the originating R package of the plot object, i.e. base, ggplot, or lattice.
#' @rdname graphSys-methods
#' @examples
#' library(ggplot2)
#' mt = datasets::mtcars
#' plt = qplot(mt$mpg, mt$cyl)
#' fs = makeFeatureSet(plt)
#' graphSys(fs)
#' @export
setGeneric(name = "graphSys",
    def = function(object) {
        standardGeneric("graphSys")
    }
)

#' @name describePackage
#' @description The analysis package, if any, that was 'in use' (i.e.,
#'     that contained the working directory that was active) when the
#'     result was recorded.
#' @title Accessor method for ObjFeatureSet "analysispkg" slot.
#' @param object An object of (super)class ObjFeatureSet.
#' @return A list holding the name, title, and description of any
#'     packages with description files on the current path.
#' @rdname describePackage-methods
#' @examples
#' mt = datasets::mtcars
#' fs = makeFeatureSet(mt)
#' describePackage(fs)
#' @export
setGeneric(name = "describePackage",
    def = function(object) {
        standardGeneric("describePackage")
    }
)

#' @name titles
#' @rdname titles-methods
#' @examples
#' library(ggplot2)
#' mt = datasets::mtcars
#' plt = qplot(mt$mpg, mt$cyl)
#' fs = makeFeatureSet(plt)
#' titles(fs)
#' @export
setGeneric(name = "titles",
    def = function(object) {
        standardGeneric("titles")
    }
)

#' @name objCode
#' @rdname objCode-methods
#' @examples
#' library(ggplot2)
#' mt = datasets::mtcars
#' plt = qplot(mt$mpg, mt$cyl)
#' fs = makeFeatureSet(plt)
#' objCode(fs)
#' @export
setGeneric(name = "objCode",
    def = function(object) {
        standardGeneric("objCode")
    }
)

#' @name codeInfo
#' @rdname codeInfo-methods
#' @examples
#' mt = datasets::mtcars
#' fs = makeFeatureSet(mt)
#' codeInfo(fs)
#' @export
setGeneric(name = "codeInfo",
    def = function(object) {
        standardGeneric("codeInfo")
    }
)

#' @name fullData
#' @description Get full data associated with a plot or model fit, if possible.
#' @title Accessor method for the data (slot "data") that is used in plotting.
#' @param object An object of (super)class PlotFeatureSet. For accession, also plot objects of class ggplot or trellis.
#' @param quiet Should warnings about plotted data be supressed?
#' @return A list of data.frame containing the variables used in plotting.
#' @rdname fullData-methods
#' @examples
#' library(ggplot2)
#' mt = datasets::mtcars
#' plt = qplot(mt$mpg, mt$cyl)
#' fs = makeFeatureSet(plt)
#' fullData(fs)
#' @export
setGeneric(name = "fullData",
    def = function(object, quiet = FALSE) {
        standardGeneric("fullData")
    }
)

#' @name dataLabels
#' @rdname dataLabels-methods
#' @examples
#' library(ggplot2)
#' mt = datasets::mtcars
#' plt = qplot(mt$mpg, mt$cyl)
#' fs = makeFeatureSet(plt)
#' dataLabels(fs)
#' @export
setGeneric(name = "dataLabels",
    def = function(object) {
        standardGeneric("dataLabels")
    }
)

#' @name dataNames
#' @title Accessor method for the variable names of the plot object. Not an exported method.
#' @description Get the variable names from a PlotFeatureSet
#' @param object An object of (super)class PlotFeatureSet. For accession, also plot objects of class ggplot or trellis.
#' @return A named list of variable names of the form list(x = "x.name", y = "y.name", groups = list(...)).
setGeneric(name = "dataNames",
    def = function(object) {
        standardGeneric("dataNames")
    }
)

#' @name dataTypes
#' @description Get the variable types from a plot feature set
#' @title Accessor method for the variable types (slot "vartypes") of
#'     the plot object.
#' @param object An object of class PlotFeatureSet, GGplotFeatureSet,
#'     TrellisFeatureSet, ggplot, or trellis.
#' @return A named list of variable types of the form list(x =
#'     "numeric", y = "factor", ...).
#' @rdname dataTypes-methods
#' @examples
#' library(ggplot2)
#' mt = datasets::mtcars
#' plt = qplot(mt$mpg, mt$cyl)
#' fs = makeFeatureSet(plt)
#' dataTypes(fs)
#'
#' @export
setGeneric(name = "dataTypes",
    def = function(object) {
        standardGeneric("dataTypes")
    }
)

#' @name annotationText
#' @title Accessor method for the annotation text (slot "annotationtext") of the plot object.
#' @description Get the text annotations for a plot object.
#' @param object An object of class  PlotFeatureSet, GGplotFeatureSet, TrellisFeatureSet, ggplot, or trellis.
#' @return A character vector of text annotations appearing on the plot.
#' @rdname annotationText-methods
#' @examples
#' library(ggplot2)
#' mt = datasets::mtcars
#' plt = qplot(mt$mpg, mt$cyl)
#' fs = makeFeatureSet(plt)
#' annotationText(fs)
#' @export
setGeneric(name = "annotationText",
    def = function(object) {
        standardGeneric("annotationText")
    }
)

#' @name geomObject
#' @rdname geomObject-methods
#' @examples
#' library(ggplot2)
#' mt = datasets::mtcars
#' plt = qplot(mt$mpg, mt$cyl)
#' fs = makeFeatureSet(plt)
#' geomObject(fs)
#' @export
setGeneric(name = "geomObject",
    def = function(object) {
        standardGeneric("geomObject")
    }
)

#' @name statTransform
#' @rdname statTransform-methods
#' @examples
#' library(ggplot2)
#' mt = datasets::mtcars
#' plt = qplot(mt$mpg, mt$cyl)
#' fs = makeFeatureSet(plt)
#' statTransform(fs)
#' @export
setGeneric(name = "statTransform",
    def = function(object) {
        standardGeneric("statTransform")
    }
)

#' @name groupInfo
#' @title Accessor method for the panel and other grouping information (slot "grouping") of the plot object.
#' @description Get the grouping info for a plot or PlotFeatureSet.
#' @param object An object of class  PlotFeatureSet, GGplotFeatureSet, TrellisFeatureSet, ggplot, or trellis.
#' @return A named list of grouping information parameters.
#' @rdname groupInfo-methods
#' @examples
#' library(ggplot2)
#' mt = datasets::mtcars
#' plt = qplot(mt$mpg, mt$cyl)
#' fs = makeFeatureSet(plt)
#' groupInfo(fs)
#' @export
setGeneric(name = "groupInfo",
    def = function(object) {
        standardGeneric("groupInfo")
    }
)

#' @name nLayers
#' @rdname nLayers-methods
#' @examples
#' library(ggplot2)
#' mt = datasets::mtcars
#' plt = qplot(mt$mpg, mt$cyl)
#' fs = makeFeatureSet(plt)
#' nLayers(fs)
#' @export
setGeneric(name = "nLayers",
    def = function(object) {
        standardGeneric("nLayers")
    }
)


#' @name position
#' @rdname position-methods
#' @examples
#' library(ggplot2)
#' mt = datasets::mtcars
#' plt = qplot(mt$mpg, mt$cyl)
#' fs = makeFeatureSet(plt)
#' position(fs)
#' @export
setGeneric(name = "position",
    def = function(object) {
        standardGeneric("position")
    }
)

#' @name coordSystem
#' @title Accessor method for the coordinate system (slot "coordsys") of the plot object.
#' @description Get the coordinate system for a plot or PlotFeatureSet.
#' @param object An object of class  PlotFeatureSet, GGplotFeatureSet, TrellisFeatureSet, ggplot, or trellis.
#' @return A character vector describing the coordinate system employed in the plot.
#' @rdname coordSystem-methods
#' @examples
#' library(ggplot2)
#' mt = datasets::mtcars
#' plt = qplot(mt$mpg, mt$cyl)
#' fs = makeFeatureSet(plt)
#' coordSystem(fs)
#' @export
setGeneric(name = "coordSystem",
    def = function(object) {
        standardGeneric("coordSystem")
    }
)

#' @name hasLegend
#' @title Accessor method for the legend indicator (slot "haslegend") of the plot object.
#' @description Get whether or not a plot or PlotFeatureSet.
#' @param object An object of class PlotFeatureSet, GGplotFeatureSet, TrellisFeatureSet, ggplot, or trellis.
#' @return A boolean indicating whether or not a legend is displayed in the plot.
#' @rdname hasLegend-methods
#' @examples
#' library(ggplot2)
#' mt = datasets::mtcars
#' plt = qplot(mt$mpg, mt$cyl)
#' fs = makeFeatureSet(plt)
#' hasLegend(fs)
#' @export
setGeneric(name = "hasLegend",
    def = function(object) {
        standardGeneric("hasLegend")
    }
)

#' @name nObs
#' @title Accessor method for the number of observations (slot "nobs") of the plot object.
#' @description Number of observations plotted in a plot or associated PlotFeatureSet.
#' @param object An object of class PlotFeatureSet, GGplotFeatureSet, TrellisFeatureSet, ggplot, or trellis.
#' @return An integer representing the number of observations in the plotted data.
#' @rdname nObs-methods
#' @examples
#' library(ggplot2)
#' mt = datasets::mtcars
#' plt = qplot(mt$mpg, mt$cyl)
#' fs = makeFeatureSet(plt)
#' nObs(fs)
#' @export
setGeneric(name = "nObs",
    def = function(object) {
        standardGeneric("nObs")
    }
)

#' @name tags
#' @rdname tags-methods
#' @examples
#' library(ggplot2)
#' mt = datasets::mtcars
#' plt = qplot(mt$mpg, mt$cyl)
#' fs = makeFeatureSet(plt)
#' tags(fs)
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
#' @description This page describes the API which must be implimented by all Trackr backends
#' Any class with methods for these generic can be used in the \code{backend}
#' slot of a \code{TrackrDB} object. These functions should not be called directly by an end-user.
#'
#' @details Methods should be written to be dispatched on \code{target}, and, where appropriate
#' on code{object}/code{doc}
#'
#' These methods should be endomorphic with respect to the
#' \code{target} argument. That is, they should return an object of
#' the same class as was passed to \code{target}, which reflect the
#' requested change to the backend state.
#'
#' @param object ANY. The object to lookup, add, remove, etc.
#' @param target ANY. The backend of the TrackrDB instance.
#' @param opts TrackrOptions. The trackr-level options. Typically
#' extracted from \code{target} in a \code{TrackrDB} method and passed down.
#' @param exist logical. Return TRUE/FALSE rather than the looked-up object.
#' (default: FALSE)
#'
#' @return for \code{trackr_lookup}: If \code{exist} is TRUE, a
#'     logical indicating whether \code{object} was found in
#'     \code{db}. Otherwise, the object stored in the database (or
#'     NULL if it was not found).
#' @rdname interface-api
#' @docType methods
#' @export
#' @aliases trackr_lookup,ANY,ANY-method
#'     trackr_lookup,character,ANY-method
#'     trackr_lookup,character,DocCollectionRef-method
#'     trackr_lookup,character,TrackrDB-method
setGeneric("trackr_lookup", function(object, target, opts,
                                 exist = FALSE) standardGeneric("trackr_lookup"))

#' @name insert_record
#' @param id character. The id to assign to the recorded result within
#'     the target backend.
#' @param verbose logical. Should extra informative messages be
#'     displayed ( default: FALSE)
#' @rdname interface-api
#' @return for \code{insert_record} and \code{remove_record}: The
#'     \code{TrackrDB} (\code{db} parameter) after the plot has been
#'     added or removed.
#' @details \code{insert_record} may or may not involve writing to
#'     disk, which can alternatively occur during
#'     \code{trackr_write}. Writing, if any is desired, must occur
#'     within at one and only one of these methods. If
#'     \code{insert_record} performs the writing, \code{trackr_write}
#'     should be a no-op.
#' @export
#' @aliases insert_record,ANY-method
#'     insert_record,DocCollection-method
#'     insert_record,DocCollectionRef-method
#'     insert_record,TrackrDB-method
setGeneric("insert_record", function(object, id, target, opts, verbose = FALSE)
    standardGeneric("insert_record"), signature = "target")

#' @name prep_for_backend
#' @rdname interface-api
#' @return For \code{prep_for_backend}, \code{object}, representend in
#'     the form that the \code{insert_record} method for
#'     \code{backend} expects.
#' @export
#' @aliases prep_for_backend,FeatureSet,ANY-method
#'     prep_for_backend,ObjFeatureSet,ANY-method
#'     prep_for_backend,FeatureSet,TrackrDB-method
setGeneric("prep_for_backend", function(object, target, opts,
                                     verbose = FALSE)
    standardGeneric("prep_for_backend"))

#' @name remove_record
#' @note \code{remove_record} should have the same writing behavior
#' as \code{insert_record}
#' @rdname interface-api
#' @export
#' @aliases remove_record,ANY,TrackrDB-method
#'     remove_record,character,ANY-method
#'     remove_record,character,DocCollectionRef-method
#'     remove_record,character,TrackrDB-method
setGeneric("remove_record", function(object, target, opts, verbose = FALSE)
    standardGeneric("remove_record"))


#' @name trackr_write
#' @note \code{remove_record} should have the same writing behavior as
#'     \code{insert_record}
#' @rdname interface-api
#' @export
#' @aliases trackr_write,ANY-method trackr_write,JSONBackend-method
#'     trackr_write,TrackrDB-method
setGeneric("trackr_write", function(target, opts, verbose = FALSE) standardGeneric("trackr_write"))




#' @name trackr_search
#' @rdname interface-api
#' @param pattern character. A regular expression to match against the
#'     text in \code{fields}
#' @param fields character or NULL. The fiends in which to match, or
#'     NULL to include all fields.
#' @param ret_type character. Format in which to return the
#'     response. Options are: "id" - id of matching documents
#'     (default), "doclist" - A list containing the matching documents
#'     represnted as R lists, and "backend" - a backend specific
#'     representation of the set of matching documents (generally the
#'     same class as \code{backend}.
#' @export
#' @aliases trackr_search,character,DocCollection-method
#'     trackr_search,character,DocCollectionRef-method
#'     trackr_search,character,SolrList-method
#'     trackr_search,character,TrackrDB-method
setGeneric("trackr_search", function(pattern, target, opts,
                               fields = NULL,
                               ret_type = c("doclist", "id", "backend"),
                               verbose = TRUE)
    standardGeneric("trackr_search"))



#' @title customizing-metadata
#' @description The generateTags is called when extracting metadata
#'     from an object. It is one mechanism by which custom metadata
#'     can be defined on a class-by-class basis without the more
#'     heavy-weight solution of defining an entirely new FeatureSet
#'     subclass.
#' @details The upsides of this mechanism is that it is easier to use
#'     and lighter weight than defining new FeatureSet classes and
#'     methods to generate them. The downsides are that the metadata
#'     are tags, rather than proper key-value pairs, as far as trackr
#'     is concerned. (A backend could be engineered such that it
#'     interpreted tags of the form \code{'key:value'} as key-value
#'     pairs, but this won't occur without extra work, and thus those
#'     implied fields will not be queriable via the trackr api
#'     specifically. The values will be included in the metadata
#'     generally though, so non-field-specific queries will work.
#' @name generateTags
#' @rdname customizing-metadata
#' @param object The object to generate tags for
#' @return A character vector of tags to associate with \code{object}
#'     during the recording process
#' @docType methods
#' @examples
#' generateTags(mtcars) #character(0)
#' @export
#' @aliases generateTags,ANY-method
setGeneric("generateTags", function(object) standardGeneric("generateTags"))


