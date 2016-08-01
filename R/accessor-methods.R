## Mutator methods for PlotFeatureSet -----------------------------------------

#' @rdname titles-methods
setReplaceMethod(f = "titles", 
    signature = "PlotFeatureSet",
    definition = function(object, value) {
        object@titles <- value
        # should do validity checking here
        # if (validObject(object)) {
            return(object)
        # }
    }
)

#' @rdname objCode-methods
setReplaceMethod(f = "objCode", 
    signature = "PlotFeatureSet",
    definition = function(object, value) {
        object@code <- parseCode(value)
        object@code.info <- CodeDepends::getInputs(object@code)
        # should do validity checking here
        # if (validObject(object)) {
            return(object)
        # }
    }
)

#' @rdname codeInfo-methods
setReplaceMethod(f = "codeInfo", 
    signature = "ObjFeatureSet",
    definition = function(object, value) {
        object@code.info <- value
        # should do validity checking here
        # if (validObject(object)) {
            return(object)
        # }
    }
)

#' @rdname dataLabels-methods
setReplaceMethod(f = "dataLabels", 
    signature = "PlotFeatureSet",
    definition = function(object, value) {
        object@var.labels <- value
        # should do validity checking here
        # if (validObject(object)) {
            return(object)
        # }
    }
)

#' @rdname geomObject-methods
setReplaceMethod(f = "geomObject", 
    signature = "GGplotFeatureSet",
    definition = function(object, value) {
        object@geom <- value
        # should do validity checking here
        # if (validObject(object)) {
            return(object)
        # }
    }
)

#' @rdname statTransform-methods
setReplaceMethod(f = "statTransform", 
    signature = "GGplotFeatureSet",
    definition = function(object, value) {
        object@stat <- value
        # should do validity checking here
        # if (validObject(object)) {
            return(object)
        # }
    }
)

#' @rdname nLayers-methods
setReplaceMethod(f = "nLayers", 
    signature = "GGplotFeatureSet",
    definition = function(object, value) {
        object@num.layers <- value
        # should do validity checking here
        # if (validObject(object)) {
            return(object)
        # }
    }
)

#' @rdname position-methods
setReplaceMethod(f = "position", 
    signature = "GGplotFeatureSet",
    definition = function(object, value) {
        object@position <- value
        # should do validity checking here
        # if (validObject(object)) {
            return(object)
        # }
    }
)

#' @rdname tags-methods
setReplaceMethod(f = "tags", 
    signature = "ObjFeatureSet",
    definition = function(object, value) {
        object@tags <- value
        # should do validity checking here
        # if (validObject(object)) {
            return(object)
        # }
    }
)

#' @rdname tags-methods
setReplaceMethod(f = "tags", 
    signature = "ggplot",
    definition = function(object, value) {
        object$tags <- value
        # should do validity checking here
        # if (validObject(object)) {
            return(object)
        # }
    }
)

#' @rdname tags-methods
setReplaceMethod(f = "tags", 
    signature = "trellis",
    definition = function(object, value) {
        object$tags <- value
        # should do validity checking here
        # if (validObject(object)) {
            return(object)
        # }
    }
)

#' @rdname tags-methods
setReplaceMethod(f = "tags", 
    signature = "gTree",
    definition = function(object, value) {
        not_implemented("tags on a gTree object.")
        return(object)
    }
)

#' @rdname tags-methods
setMethod(f = "editTags", 
    signature = "ObjFeatureSet",
    definition = function(object, value, option = c("replace", "add", "remove")) {
        option <- match.arg(option)
        switch(option,
            replace = {
                tags(object) <- unique(value)
            },
            add = {
                value <- setdiff(unique(value), object@tags)
                tags(object) <- c(object@tags, value)
            },
            remove = {
                value <- intersect(unique(value), object@tags)
                # if there are any to remove...
                if(length(value)>0){
                    tag.loc <- grep(paste0("^", value, "$", collapse="|"), 
                        object@tags)
                    tags(object) <- object@tags[-tag.loc]
                }
            },
            warning("No tags were modified.")
        )

        # should do validity checking here
        # if (validObject(object)) {
            return(object)
        # }
    }
)

## add tag parser

## Accessor methods for PlotFeatureSet ----------------------------------------

#' @rdname titles-methods
setMethod(f = "titles",
    signature = "PlotFeatureSet",
    definition = function(object) {
        object@titles
    }
)

#' @rdname objCode-methods
setMethod(f = "objCode",
    signature = "ObjFeatureSet",
    definition = function(object) {
        object@code
    }
)

#' @rdname codeInfo-methods
setMethod(f = "codeInfo",
    signature = "ObjFeatureSet",
    definition = function(object) {
        object@code.info
    }
)

#' @rdname fullData-methods
setMethod(f = "fullData",
    signature = "PlotFeatureSet",
    definition = function(object) {
        object@data
    }
)

#' @rdname annotationText-methods
setMethod(f = "annotationText",
    signature = "PlotFeatureSet",
    definition = function(object) {
        object@annotation.text
    }
)

#' @rdname dataLabels-methods
setMethod(f = "dataLabels",
    signature = "PlotFeatureSet",
    definition = function(object) {
        object@var.labels
    }
)

#' @rdname dataTypes-methods
setMethod(f = "dataTypes",
    signature = "PlotFeatureSet",
    definition = function(object) {
        object@var.types
    }
)

#' @rdname nLayers-methods
setMethod(f = "nLayers",
    signature = "GGplotFeatureSet",
    definition = function(object) {
        object@num.layers
    }
)

#' @rdname geomObject-methods
setMethod(f = "geomObject",
    signature = "GGplotFeatureSet",
    definition = function(object) {
        object@geom
    }
)

#' @rdname statTransform-methods
setMethod(f = "statTransform", 
    signature = "GGplotFeatureSet",
    definition = function(object) {
        object@stat
    }
)

#' @rdname groupInfo-methods
setMethod(f = "groupInfo", 
    signature = "PlotFeatureSet",
    definition = function(object) {
        object@grouping
    }
)

#' @rdname position-methods
setMethod(f = "position", 
    signature = "GGplotFeatureSet",
    definition = function(object) {
        object@position
    }
)

#' @rdname coordSystem-methods
setMethod(f = "coordSystem",
    signature = "PlotFeatureSet",
    definition = function(object) {
        object@coord.sys
    }
)

#' @rdname hasLegend-methods
setMethod(f = "hasLegend",
    signature = "PlotFeatureSet",
    definition = function(object) {
        object@has.legend
    }
)

#' @rdname nObs-methods
setMethod(f = "nObs",
    signature = "PlotFeatureSet",
    definition = function(object) {
        object@num.obs
    }
)

#' @rdname tags-methods
setMethod(f = "tags", 
    signature = "ObjFeatureSet",
    definition = function(object) {
        object@tags
    }
)

#' @rdname tags-methods
setMethod(f = "tags", 
    signature = "ggplot",
    definition = function(object) {
        res <- object$tags
        if(is.null(res)) {
            res <- character(0)
        }
        res
    }
)

#' @rdname tags-methods
setMethod(f = "tags", 
    signature = "trellis",
    definition = function(object) {
        res <- object$tags
        if(is.null(res)) {
            res <- character(0)
        }
        res
    }
)

#' @rdname tags-methods
setMethod(f = "tags", 
    signature = "gTree",
    definition = function(object) {
        not_implemented("tags on a gTree object.")
        character(0)
    }
)

#' @rdname tags-methods
setMethod(f = "tags",
          signature = "ANY",
          definition = function(object) {
    res <- attr(object, "tags", TRUE)
    if(is.null(res)) {
        res <- character(0)
    }
    res
    
})


## accessor methods for ViztrackrDB and ViztrackrOptions

setGeneric("vt_backend", function(db) standardGeneric("vt_backend"))
setGeneric("vt_backend<-", function(db, value) standardGeneric("vt_backend<-"))
#' @title options extraction
#' @rdname img_opts
#' @param db Object to extract information from
#' @docType methods
#' @export
setGeneric("vt_options", function(db) standardGeneric("vt_options"))
#' @rdname img_opts
#' @aliases vt_img_dir
#' @export
setGeneric("vt_img_dir", function(db) standardGeneric("vt_img_dir"))
#' @rdname img_opts
#' @aliases vt_img_ext
#' @export
setGeneric("vt_img_ext", function(db) standardGeneric("vt_img_ext"))


setMethod("vt_backend", "ViztrackrDB", function(db) db@backend)
setMethod("vt_backend<-", "ViztrackrDB",
          function(db, value) {
              db@backend = value
              db
          })

setMethod("vt_options", "ViztrackrDB", function(db) db@opts)

setMethod("vt_img_dir", "ViztrackrDB", function(db) vt_img_dir(vt_options(db)))
setMethod("vt_img_dir", "ViztrackrOptions", function(db) db@img_dir)


setMethod("vt_img_ext", "ViztrackrDB", function(db) vt_img_ext(vt_options(db)))
setMethod("vt_img_ext", "ViztrackrOptions", function(db) db@img_ext)

#' @rdname uniqueID-methods
setMethod(f = "uniqueID",
    signature = "ObjFeatureSet",
    definition = function(object) {
        object@uniqueid
    }
)

#' @rdname uniqueID-methods
setReplaceMethod(f = "uniqueID", 
    signature = "ObjFeatureSet",
    definition = function(object, value) {
        object@uniqueid <- value
        # should do validity checking here
        # if (validObject(object)) {
            return(object)
        # }
    }
)


setMethod(f = "uniqueID",
          signature = "ANY",
          definition = function(object) {
    tmp = makeFeatureSet(object)
    uniqueID(tmp)
    })

#' @title Number of docs in a vtdb
#' Query the backend for the number of docs it contains
#' @param x ViztrackrDB.
#' @param ... not used.
#' @aliases ndoc-method,ViztrackrDB
#' @docType methods
#' @export
setMethod("ndoc", "ViztrackrDB",
          function(x, ...) ndoc(vt_backend(x)))
