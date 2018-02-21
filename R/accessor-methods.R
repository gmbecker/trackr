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
    signature = "FeatureSet",
    definition = function(object, value) {
        object@code <- as.character(parseCode(value))
        object@codeinfo <- CodeDepends::getInputs(parseCode(object@code))
        # should do validity checking here
        # if (validObject(object)) {
            return(object)
        # }
    }
)

#' @rdname codeInfo-methods
setReplaceMethod(f = "codeInfo", 
    signature = "FeatureSet",
    definition = function(object, value) {
        object@codeinfo <- value
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
        object@varlabels <- value
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
    signature = "FeatureSet",
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
    signature = "FeatureSet",
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
    signature = "FeatureSet",
    definition = function(object) {
        object@code
    }
)

#' @rdname codeInfo-methods
setMethod(f = "codeInfo",
    signature = "FeatureSet",
    definition = function(object) {
        object@codeinfo
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
        object@annotationtext
    }
)

#' @rdname dataLabels-methods
setMethod(f = "dataLabels",
    signature = "PlotFeatureSet",
    definition = function(object) {
        object@varlabels
    }
)

#' @rdname dataTypes-methods
setMethod(f = "dataTypes",
    signature = "PlotFeatureSet",
    definition = function(object) {
        object@vartypes
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
        object@coordsys
    }
)

#' @rdname hasLegend-methods
setMethod(f = "hasLegend",
    signature = "PlotFeatureSet",
    definition = function(object) {
        object@haslegend
    }
)

#' @rdname nObs-methods
setMethod(f = "nObs",
    signature = "PlotFeatureSet",
    definition = function(object) {
        object@nobs
    }
)

#' @rdname tags-methods
setMethod(f = "tags", 
    signature = "FeatureSet",
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


## accessor methods for TrackrDB and TrackrOptions

setGeneric("trackr_backend", function(db) standardGeneric("trackr_backend"))
setGeneric("trackr_backend<-", function(db, value) standardGeneric("trackr_backend<-"))
#' @title options extraction
#' @description These functions extract the trackr options associated with a backend.
#' @rdname img_opts
#' @param db Object to extract information from
#' @docType methods
#' @export
#' @aliases trackr_options,ANY-method trackr_options,TrackrDB-method
setGeneric("trackr_options", function(db) standardGeneric("trackr_options"))
#' @rdname img_opts
#' @aliases img_dir img_dir,TrackrDB-method img_dir,TrackrOptions-method
#' @export
setGeneric("img_dir", function(db) standardGeneric("img_dir"))
#' @rdname img_opts
#' @aliases img_ext img_ext,TrackrDB-method img_ext,TrackrOptions-method
#' @export
setGeneric("img_ext", function(db) standardGeneric("img_ext"))


setMethod("trackr_backend", "TrackrDB", function(db) db@backend)
setMethod("trackr_backend<-", "TrackrDB",
          function(db, value) {
              db@backend = value
              db
          })

setMethod("trackr_options", "TrackrDB", function(db) db@opts)

setMethod("img_dir", "TrackrDB", function(db) img_dir(trackr_options(db)))
setMethod("img_dir", "TrackrOptions", function(db) {

    dr = db@img_dir
    if(!file.exists(dr)) {
        message("Creating image directory at ", dr)
        dir.create(dr, recursive=TRUE)
    }
    dr
    
})


setMethod("img_ext", "TrackrDB", function(db) img_ext(trackr_options(db)))
setMethod("img_ext", "TrackrOptions", function(db) db@img_ext)

#' @rdname uniqueID-methods
setMethod(f = "uniqueID",
    signature = "FeatureSet",
    definition = function(object) {
        object@uniqueid
    }
)

#' @rdname uniqueID-methods
setReplaceMethod(f = "uniqueID", 
    signature = "FeatureSet",
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

#' @title Number of docs in a TrackrDB
#' @description Query the backend for the number of docs it contains
#' @param x TrackrDB.
#' @param ... not used.
#' @aliases ndoc,TrackrDB-method
#' @docType methods
#' @export
setMethod("ndoc", "TrackrDB",
          function(x, ...) ndoc(trackr_backend(x)))



## function factory for accessors that *only* work on ObjFeatureSet objects
## and have no corresponding setters.

## XXX these should all probably be generic + method but it only
## needs to work for 1 class, so why?


makeToyAccessor = function(slname) {
    function(x) {
        if(!is(x, "FeatureSet"))
            stop("direct accessor called on non-ObjFeatureSet. This should never happen please contact the maintainer")
        slot(x, slname)
    }
}



fsuser = makeToyAccessor("user")


fsregdatetime = makeToyAccessor("regdate")

fsrstudioproject = makeToyAccessor("rstudioproject")

fsanalysisfile = makeToyAccessor("analysisfile")


setGeneric("imagepath", function(x, db) standardGeneric("imagepath"))
setMethod("imagepath", c("FeatureSet", "ANY") , function(x, db) NA_character_)
##setMethod("imagepath", c("PlotFeatureSet", "TrackrDB"), function(x, db) {
setMethod("imagepath", c("FeatureSet", "TrackrDB"), function(x, db) {
    if(!file.exists(img_dir(db)))
        dir.create(img_dir(db), recursive=TRUE)
    file.path(img_dir(db), paste(uniqueID(x), img_ext(db), sep="."))
})



setGeneric("previewpath", function(x, db) standardGeneric("previewpath"))
setMethod("previewpath", c("FeatureSet", "ANY"),  function(x, db) NA_character_)
##setMethod("previewpath", c("PlotFeatureSet", "TrackrDB"), function(x, db) {
setMethod("previewpath", c("FeatureSet", "TrackrDB"), function(x, db) {
    if(!file.exists(img_dir(db)))
        dir.create(img_dir(db), recursive=TRUE)
    file.path(img_dir(db), paste0(uniqueID(x), "_thumb.", img_ext(db)))
})



setGeneric("varnames", function(x) standardGeneric("varnames"))
setMethod("varnames", "FeatureSet", function(x) NA_character_)
setMethod("varnames", "DFFeatureSet", function(x) x@varnames)



## setGeneric("cline_args", function(x) standardGeneric("cline_args"))
## setMethod("cline_args", "missing", function(x) collapsed_cline_args())
## setMethod("cline_args", "FeatureSet", function(x) x@clineargs)
