#' @include Classes.R Generics.R PlotWrapper.R
NULL

#' @title Collapse a list to a comma-delimited character vector
#' @param l A list
#' @param with.names A boolean indicating whether the list is named. False by default.
#' @return A comma-delimited character vector
collapse_to_str <- function(l, with.names=FALSE) {
    if (with.names) {
        l <- sapply(l, function(x) paste(names(x), x, sep = " = "),
            simplify = FALSE)
    }
    l.new <- sapply(names(l), 
        function(x) paste(x, 
            ifelse(length(l[[x]]>0),
                paste(l[[x]], collapse=", "), "defaults"),
            sep=": "))
    names(l.new) <- NULL
    return(l.new)
}

# from start.dir, go up the specified number of directories (>=0)
# and report back the directory's name and path
# as a data.frame rowb
dir_path_pair <- function(dirs.up, start.dir = getwd()) {
    new.path <- normalizePath(paste(
        start.dir,
        paste(rep("..", dirs.up), collapse = .Platform$file.sep),
        sep = .Platform$file.sep))
    c(pkg.name = basename(new.path),
        pkg.dir = dirname(new.path))
}


scrape_descr <- function(start_dir = getwd(), fields = c("Package", "Title", "Description"),
                         verbose = FALSE) {

    curdir = normalizePath(start_dir)
    res = list()
    while(!curdir %in% c(normalizePath("~"), normalizePath("/"))) {
        dfile = file.path(curdir, "DESCRIPTION")
        if(file.exists(dfile)) {
            dsc = as.data.frame(read.dcf(dfile, fields), stringsAsFactors=FALSE)
            res = c(res, list(as.list(dsc)))
        }
        curdir = normalizePath(file.path(curdir, ".."))
    }
    res

}


##because S3, grumble mumble
getBaseS3Class = function(x) tail(class(x), 1)
getTopS3Class = function(x) head(class(x), 1)


ObjFeatureSet = function(  object,
                         code = as.character(parseCode(object)),
                         codeinfo = CodeDepends::getInputs(parseCode(code)),
                         klass = getTopS3Class(object),
                         uniqueid = gen_hash_id(object),
                         ## XXX IDifficult to ensure tags(object) and
                         ## generateTags(object) always included without
                         ## overriding/duplicating when I Call this to
                         ## resucitate a  feature set out of a db record
                         ## Hopefully the unique call below does it.
                         tags = character(),
                         user = unname(Sys.info()["user"]),
                         regdate = Sys.time(),
                         analysispkg = scrape_descr(),
                         analysisfile = .analysisFileOrNA(),
                         rstudioproject = .rstudioProjOrNA(),
                         fsetklass = "ObjFeatureSet" ,
                         isplot= FALSE,
                         ...) {
    
    tags <- unique(c(tags, tags(object), generateTags(object)))
    new("ObjFeatureSet",
        object = object,
        code = code,
        codeinfo = codeinfo,
        uniqueid = uniqueid,
        tags = tags,
        user = user,
        regdate = regdate,
        analysispkg = analysispkg,
        klass = class(object),
        analysisfile = analysisfile,
        rstudioproject = rstudioproject,
        fsetklass = fsetklass,
        isplot = isplot,
        ...)
}

PlotFeatureSet = function(object, fsetklass = "PlotFeatureSet",
                          package = NA_character_,
                          ...) {
    innerobj = ObjFeatureSet(object = object, fsetklass = fsetklass,
                             isplot = TRUE,
                             ...)
    new("PlotFeatureSet", innerobj,
        grouping = groupInfo(object),
        ## .Object@varlabels$group$panel = panelInfo(object)
        ## .Object@panel$var.levels = collapse_to_str(.Object@panel$var.levels)
        ## .Object@varlabels$group$panel$var.levels = collapse_to_str(.Object@panel$var.levels)
        vartypes = dataTypes(object),
        ## .Object@vartypes$panel = paste(.Object@vartypes$panel, collapse = ", ")
        titles = titles(object),
        data = fullData(object),
        annotationtext = annotationText(object),
        nobs = nObs(object),
        varlabels = dataLabels(object),
        coordsys = coordSystem(object),
        haslegend = hasLegend(object),
        package = package)
}

GGplotFeatureSet = function(object, fsetklass = "GGplotFeatureSet",...) {

    ## beef up the ggplot object with some theme info
    ## (this assumes the user hasn't changed the default theme 
    ## between plot creation and plot registration...)
    if(length(object$theme)==0) {
        object$theme <- ggplot2::theme_get()
    }

    innerobj = PlotFeatureSet(object = object,
                              fsetklass = fsetklass,
                              package = "ggplot2",
                              ...)
    new("GGplotFeatureSet", innerobj,
        
        # geom = list(type = paste(names(geomObject(object)), collapse=", "),
        geom = geomObject(object),
            # list(type = names(geomObject(object)),
            # params = collapse_to_str(geomObject(object), with.names = TRUE))

        # stat = list(type = paste(names(statTransform(object)), collapse=", "),
        stat = statTransform(object),
            # list(type = names(statTransform(object)),
            # params = collapse_to_str(statTransform(object), with.names = TRUE))

        # position = list(type = paste(names(position(object)), collapse=", "),
        # position = list(type = names(position(object)),
        #     params = collapse_to_str(position(object), with.names = TRUE))
        position = position(object),

        num.layers = nLayers(object)
        )
}

TrellisFeatureSet = function(object, fsetklass = "TrellisFeatureSet", ...) {

    innerobj = PlotFeatureSet(object = object,
                              fsetklass = fsetklass,
                              package = "lattice",
                              ...)
    ## no new semantics, just needed package and fsetklass labeling
    new("TrellisFeatureSet", innerobj)
}


GraphicsFeatureSet = function(object, fsetklass = "GraphicsFeatureSet", ...) {

    innerobj = PlotFeatureSet(object = object,
                              fsetklass = fsetklass,
                              package = "graphics",
                              ...)
    ## no new semantics, just needed package and fsetklass labeling
    new("GraphicsFeatureSet", innerobj)
}


DFFeatureSet = function(object, fsetklass = "DFFeatureSet",
                        vars = names(object),
                        varclasses = sapply(object, getTopS3Class),
                        varsummaries = structure(lapply(object, summary), names = names(object)),
                        nobs = nrow(object),
                        ...) {
    innerobj = ObjFeatureSet(object = object, fsetklass = fsetklass, ...)
    new("DFFeatureSet", innerobj,
        vars = vars,
        varclasses = varclasses,
        varsummaries = varsummaries,
        nobs = nobs
        )
     
}

        
        


        




#' @describeIn makeFeatureSet Construct a ObjFeatureSet from an object of class ggplot.
setMethod(f = "makeFeatureSet", 
    signature = signature(object = "ggplot"),
    definition = function(object, ...) {
        GGplotFeatureSet( object = object,
            fsetklass = "GGplotFeatureSet",
            ...)
    }
)

#' @describeIn makeFeatureSet Construct a ObjFeatureSet from an object of class trellis.
setMethod(f = "makeFeatureSet", 
    signature = signature(object = "trellis"),
    definition = function(object, ...) {
    TrellisFeatureSet( fsetklass = "TrellisFeatureSet",
        object = object, 
        ...)
    }
)




#' @describeIn makeFeatureSet Construct a ObjFeatureSet from an object of class gTree.
setMethod(f = "makeFeatureSet", 
    signature = signature(object = "gTree"),
    definition = function(object, ...) {
        GraphicsFeatureSet(
            object = object,
            fsetklass = "GraphicsFeatureSet",
            ...)
    }
)

#' @describeIn makeFeatureSet Construct a ObjFeatureSet from an object of class recordedplot.
setMethod(f = "makeFeatureSet", 
    signature = signature(object = "recordedplot"),
    definition = function(object, ...) {
        temp.plot.file <- tempfile(pattern = "Rplots", fileext = ".pdf")
        pdf(temp.plot.file)
        gridGraphics::grid.echo(object)
        plot2 <- grid::grid.grab()
        dev.off()
        unlink(temp.plot.file)
        makeFeatureSet(plot2, ...)
    }
)

#' @describeIn makeFeatureSet Construct a ObjFeatureSet from an unevaluated expression.
setMethod(f = "makeFeatureSet", 
    signature = signature(object = "expression"),
    definition = function(object, ...) {
        tryCatch({
                plot2 <- eval(object)
                makeFeatureSet(plot2, code = object)
            }, error = function(c) {
                msg <- conditionMessage(c)
                message(c)
                invisible(NULL)
            }
        )   
    }
)

#' @describeIn makeFeatureSet Construct a ObjFeatureSet from a call.
setMethod(f = "makeFeatureSet", 
    signature = signature(object = "call"),
    definition = function(object, ...) {
        plot2 <- as.expression(object)
        makeFeatureSet(plot2, ...)
    }
)

#' @describeIn makeFeatureSet Construct a ObjFeatureSet from a character vector representing a call to create a plot.
setMethod(f = "makeFeatureSet", 
    signature = signature(object = "character"),
    definition = function(object, ...) {
        plot2 <- parse(text = object)
        makeFeatureSet(plot2, ...)
    }
)

#' @describeIn makeFeatureSet No-op if we already have a ObjFeatureSet
setMethod(f = "makeFeatureSet", 
    signature = signature(object = "ObjFeatureSet"),
    definition = function(object, ...) object
)


#' @describeIn makeFeatureSet Catch-all for attempted construction of a ObjFeatureSet from an object not of class ggplot or trellis.
setMethod(f = "makeFeatureSet", 
    signature = signature(object = "ANY"),
    definition = function(object, ...) {
    cls = class(object)
    ssum = capture.output(str(obj, max.level=1))
    ObjFeatureSet(
        object = object,
        fsetklass = "ObjFeatureSet",
        ...)
    
    }
)

setOldClass(c("tbl_df", "tbl", "data.frame"))

setMethod(f = "makeFeatureSet",
    signature = signature(object = "data.frame"),
    function(object, ...) {
    DFFeatureSet(
        object = object,
        fsetklass = "DFFeatureSet",
        
        ...)
    })
    
    
## separate so people can call it in their custom tag generators
generateDefTags = function(object) {
    ret = character()
    if(length(class(object)) > 1) { #S3 class shenanigans
        ret["base.s3.class"] = getBaseS3Class(object)
        ret["full.s3.class.list"] = paste(class(object), collapse = ", ")
    }
    ret
}

## by default we only do a few things
setMethod(f = "generateTags", "ANY", generateDefTags)


## setMethod(f = "initialize", 
##     signature = "ObjFeatureSet", 
##     definition = function(.Object, object, ..., tags = NULL, code = NULL, fsetklass = "") {

##     if(is.null(code))
##         code = object #we can get code from some objs...
##     .Object@code <- paste(parseCode(code), collapse="\n")
##     .Object@codeinfo <- CodeDepends::getInputs(parseCode(.Object@code))
    
##     .Object@uniqueid = gen_hash_id(object)
##     .Object@tags <- c(tags, tags(object), generateTags(object))
##     .Object@object = object
##     .Object@user = unname(Sys.info()["user"])
##     .Object@regdate = Sys.time()
##     .Object@analysispkg = scrape_descr()
##     .Object@klass = getTopS3Class(object)
##     .Object@analysisfile = .analysisFileOrNA()
##     .Object@rstudioproject = .rstudioProjOrNA()
##     .Object@fsetklass = fsetklass
##     .Object
##                                         #callNextMethod(.Object, object = object, ...) 
## })



# @title Constructor method for an object of class ObjFeatureSet.
# @name ObjFeatureSet
# @rdname ObjFeatureSet
# @param .Object An object of (super)class ObjFeatureSet.
# @param object A plot object of class ggplot or trellis.
# @param ... Other named arguments which are sent on to the PlotWrapper constructor.
# @param tags A character vector of tags to add to the new object.
# @return A ObjFeatureSet object.
# @param wait A numeric value indicating how long to wait (in seconds) before returning from a Solr add.  No wait by default.  Set this to >0 for adding plots to Solr database via *apply.
## setMethod(f = "initialize", 
##     signature = "PlotFeatureSet", 
##     definition = function(.Object, object, ..., tags = NULL, wait = 0, code = NULL) {

##     .Object@grouping <- groupInfo(object)
##     ## .Object@varlabels$group$panel <- panelInfo(object)
##     ## .Object@panel$var.levels <- collapse_to_str(.Object@panel$var.levels)
##     ## .Object@varlabels$group$panel$var.levels <- collapse_to_str(.Object@panel$var.levels)
##     .Object@vartypes <- dataTypes(object)
##     ## .Object@vartypes$panel <- paste(.Object@vartypes$panel, collapse = ", ")
##     .Object@titles <- titles(object)
##     .Object@data <- fullData(object)
##     .Object@annotationtext <- annotationText(object)
##     .Object@nobs <- nObs(object)
##     .Object@varlabels <- dataLabels(object)
##     .Object@coordsys <- coordSystem(object)
##     .Object@haslegend <- hasLegend(object)
##     .Object@isplot <- TRUE
      
##     if (wait>0) {
##         message("Pausing...")
##         Sys.sleep(wait)
##     }
    
##     callNextMethod(.Object, object = object, code = code, tags = tags, ...) #calls ObjFeatureSet
## })

# @title Initialize a GGplotFeatureSet
# @name GGplotFeatureSet
# @rdname GGplotFeatureSet
# @param .Object An object of (super)class GGplotFeatureSet.
# @param object A plot object of class ggplot.
# @param ... Other named arguments which are sent on to the ObjFeatureSet constructor.
# @return A GGplotFeatureSet object.
## setMethod(f = "initialize", 
##     signature = "GGplotFeatureSet", 
##     definition = function(.Object, object, ...) {

##         # beef up the ggplot object with some theme info
##         # (this assumes the user hasn't changed the default theme 
##         # between plot creation and plot registration...)
##         if(length(object$theme)==0) {
##             object$theme <- ggplot2::theme_get()
##         }

##         # .Object@geom <- list(type = paste(names(geomObject(object)), collapse=", "),
##         .Object@geom <- geomObject(object)
##             # list(type = names(geomObject(object)),
##             # params = collapse_to_str(geomObject(object), with.names = TRUE))

##         # .Object@stat <- list(type = paste(names(statTransform(object)), collapse=", "),
##         .Object@stat <- statTransform(object)
##             # list(type = names(statTransform(object)),
##             # params = collapse_to_str(statTransform(object), with.names = TRUE))

##         # .Object@position <- list(type = paste(names(position(object)), collapse=", "),
##         # .Object@position <- list(type = names(position(object)),
##         #     params = collapse_to_str(position(object), with.names = TRUE))
##         .Object@position <- position(object)

##         .Object@num.layers <- nLayers(object)
##     .Object@package <- "ggplot"


##         callNextMethod(.Object, object = object, ...) # calls PlotFeatureSet
##     }
## )

# @title Initialize a TrellisFeatureSet
# @name TrellisFeatureSet
# @rdname TrellisFeatureSet
# @param .Object An object of (super)class TrellisFeatureSet.
# @param object A plot object of class trellis.
# @param ... Other named arguments which are sent on to the ObjFeatureSet constructor.
# @return A TrellisFeatureSet object.
## setMethod(f = "initialize", 
##     signature = "TrellisFeatureSet", 
##     definition = function(.Object, object, ...) {

##         # do extra stuff for a trellis object
##     .Object@package <-  "lattice"
 
##         callNextMethod(.Object, object = object, ...) # calls PlotFeatureSet
##     }
## )

## setMethod(f = "initialize", 
##     signature = "GraphicsFeatureSet", 
##     definition = function(.Object, object = object, ...) {

##         # do extra stuff for a base graphics (recorded) object
##     .Object@package <- "graphics"

##     callNextMethod(.Object, object = object, ...) # calls PlotFeatureSet
##     }
## )

## http://stackoverflow.com/questions/16247583/inheritance-in-r
# ObjFeatureSet <- function(object = NULL, ...) {
# makeFeatureSet <- function(object = NULL, ...) {

#     if (inherits(object, "ggplot")) {
#         new("GGplotFeatureSet", object, ..., 
#             package = "ggplot2", regdate = Sys.time())
#     } else if(inherits(object, "trellis")) {
#         new("TrellisFeatureSet", object, ..., 
#             package = "lattice", regdate = Sys.time())
#     } else {
#         warning("ObjFeatureSet constructor not implemented for objects of type: ", 
#             paste(class(object), collapse=", "), ".")
#         NULL
#     }
# }

## Utility methods for ObjFeatureSet -----------------------------------------

## Implementation of show method for ObjFeatureSet class

# setMethod(f = "show",
#     signature = "ObjFeatureSet",
#     definition = function(object) {
#         callNextMethod() # call the show method for PlotWrapper
#     }
# )

# ## Implementation of summary method for ObjFeatureSet class

# setMethod(f = "summary",
#     signature = "ObjFeatureSet",
#     definition = function(object) {
#         callNextMethod()
#     }          
# )

#' @title Export a ObjFeatureSet as list
#' @param x A ObjFeatureSet.
#' @param ... Other named arguments (currently unused).
#' @return A list.
#' @rdname as-methods
#' @export
as.list.ObjFeatureSet <- function(x, ...) {
    out =  lapply(slotNames(x), function(nm) slot(x, nm))
    names(out) <- slotNames(x)
    out$regdate <- format(x@regdate, "%Y-%m-%d")
    out$regtime = format(x@regdate, "%H:%M:%S")
    out$regdatetime = format(x@regdate, "%Y-%m-%dT%H:%M:%SZ")
    out$code = unlist(lapply(objCode(x)@.Data, function(i) paste(deparse(i), collapse="")))
    out$codeinfo <- lapply(codeInfo(x), as.list)

    ## out <- list(
    ##     user = x@user,
    ##     package = x@package, 
    ##     # date as string not ideal but the data.frame converts POSIXct to numeric
    ##     # regdate = format(x@regdate, "%Y-%m-%d"),
    ##     # regtime = format(x@regdate, "%H:%M:%S"),
    ##     # format so solr can read it
    ##     regdatetime = format(x@regdate, "%Y-%m-%dT%H:%M:%SZ"),
    ##     titles = x@titles,
    ##     varlabels = x@varlabels,
    ##     grouping = x@grouping,
    ##     nobs = x@nobs,
    ##     haslegend = x@haslegend,
    ##     coordsys = x@coordsys,
    ##     vartypes = x@vartypes,
    ##     annotationtext = x@annotationtext,
    ##     analysispkg = x@analysispkg,
    ##     tags = x@tags,
    ##     # code = x@code
    ##     code = unlist(lapply(plotCode(x)@.Data, function(i) paste(deparse(i), collapse=""))),
    ##     codeinfo = lapply(codeInfo(x), as.list)
    ##     isplot = x@isplot
    ##     objclass = x@klass
    ## )
    

    ## if (class(x)=="GGplotFeatureSet") {
    ##     out <- c(out, list(
    ##         geom = x@geom,
    ##         stat = x@stat,
    ##         position = x@position,
    ##         num.layers = x@num.layers))
    ## }
    ## lst <-
    ## lst$object <- NULL
    ## lst$codeinfo = lapply(codeInfo(x), as.list)
    ## out = flatten5(lst)
     # http://stackoverflow.com/a/18539199
     keys <- unique(unlist(lapply(out$codeinfo, names)))
     out$codeinfo <- setNames(do.call(mapply, c(FUN=c, lapply(out$codeinfo, `[`, keys))), keys)
     out$codeinfo <- sapply(out$codeinfo, unique)
    return(out)
}

setAs(from = "ObjFeatureSet", 
    to = "list", 
    def = function(from) {
        as.list.ObjFeatureSet(from)
    }
)

#' @title Export a ScriptNodeInfo object as a list
#' @param x A ScriptNodeInfo object.
#' @param ... Other named arguments (currently unused).
#' @return A list.
#' @rdname CodeDepends-methods
#' @export
as.list.ScriptNodeInfo <- function(x, ...) {
    sapply(slotNames(x), function(i) slot(x, i))
}

setAs(from = "ScriptNodeInfo", 
    to = "list", 
    def = function(from) {
        as.list.ScriptNodeInfo(from)
    }
)

.elstocollapse = c("user", "package", "regdatetime",
                   "titles", "varlabels", "grouping",
                   "num.objs", "haslegend",
                   "coordsys", "vartypes",
                   "annotationtext", "analysispkg",
                   "tags", "code", "codeinfo",
                   "isplot", "klass", "geom", "stat",
                   "position", "num.layers",
                   "varsummaries"
                   )
#' @title Collapse particular fields of a ObjFeatureSet to delimited character vectors.
#' @param pfs An object of (super)class ObjFeatureSet.
#' @return A list.
pfs_list_collapse <- function(pfs) {
    l <- as(pfs, "list")
    l <- l[names(l) %in% .elstocollapse]
    l$titles <- paste(l$titles, collapse="; ")
    l$varlabels$x <- paste(l$varlabels$x, collapse="; ")
    l$varlabels$y <- paste(l$varlabels$y, collapse="; ")
    l$varlabels$group$panel <- paste(l$varlabels$group$panel, collapse="; ")
    l$grouping$panel$vars <- paste(l$grouping$panel$vars, collapse="; ")
    l$grouping$panel$levels <- paste(l$grouping$panel$levels, collapse="; ")
    l$code <- paste(l$code, collapse="; ")

    if(length(l$geom$type)>0) {
        l$geom$type <- paste(l$geom$type, collapse = ", ")
        l$geom$params <- paste(collapse_to_str(l$geom$params, with.names=TRUE), collapse = "; ")
    }
    if(length(l$stat$type)>0) {
        l$stat$type <- paste(l$stat$type, collapse = ", ")
        l$stat$params <- paste(collapse_to_str(l$stat$params, with.names=TRUE), collapse = "; ")
    }
    if(length(l$position$type)>0) {
        l$position$type <- paste(l$position$type, collapse = ", ")
        l$position$params <- paste(collapse_to_str(l$position$params, with.names=TRUE), collapse = "; ")
    }
    if(length(l$grouping$levels)>1) {
        l$grouping$levels <- paste(l$grouping$levels, collapse = "; ")
    }
    # these will need to be generalized!
    # theoretically any var.type could have length>1
    if((!is.null(l$vartypes$group$panel)) & (length(l$vartypes$group$panel)>1)) {
        l$vartypes$group$panel <- paste(l$vartypes$group$panel, collapse = ", ")
    }
    if(length(l$codeinfo)>0) {
        codeinfo.fields <- names(l$codeinfo[[1]])
        l$codeinfo <- sapply(codeinfo.fields, function(i)
            paste(lapply(l$codeinfo, 
                function(x) paste(x[[i]], collapse = ", ")),
            collapse = "; "), simplify = FALSE)
    }
    l
}

## the recommended approach is to define the S3 method and 
## supply the identical function as the definition of the S4 method.

#' @title Export a ObjFeatureSet as a data.frame.
#' @param row.names NULL or a character vector giving the row names for the data frame. Missing values are not allowed. Only applicable in conjunction with long = FALSE.
#' @param optional Currently unused in this implementation.
#' @param long A boolean indicating whether the data.frame should be returned in 'long' (rather than 'wide') format.
#' @return A data.frame.
#' @rdname as-methods
#' @export
as.data.frame.ObjFeatureSet <- function(x, row.names = NULL, optional = FALSE, ..., long = FALSE) {
    df <- NULL
    l <- pfs_list_collapse(x)
    if(long) {
        ## long format - this is maybe not ideal...
        ## would eventually include plot ID as a column, as well
        df <- data.frame(
            id = uniqueID(x),
            feature = names(unlist(l)), value = unlist(l))
        rownames(df) <- row.names
        df <- df[!is.na(df$value),]
    } else {
        # note that this makes everything into a string
        # wide format
        df <- data.frame(id = uniqueID(x),
            t(unlist(l)), stringsAsFactors = FALSE)
    }
    return(df)
}

setAs(from = "ObjFeatureSet", 
    to = "data.frame", 
    def = function(from) {
        as.data.frame.ObjFeatureSet(from)
    }
)

# setMethod(f = "as.data.frame", 
#     signature = "ObjFeatureSet", 
#     def = function(object, row.names = NULL, optional = FALSE, long=FALSE, ...) {
#         df <- NULL
#         if(long) {
#             ## long format - this is maybe not ideal...
#             ## would eventually include plot ID as a column, as well
#             l <- pfs_list_collapse(object)
#             df <- data.frame(
#                 id = uniqueID(object),
#                 feature = names(unlist(l)), value = unlist(l))
#             rownames(df) <- NULL
#             df <- df[!is.na(df$value),]
#         } else {
#             df <- as(object, "data.frame")
#         }
#         return(df)
#     }
# )

## returns T/F if plot meets criteria
## can be used on ObjFeatureSets that have been converted to a list or data.frame

#' @rdname selectPlot-methods
#' @param features A character vector naming the features which should be searched. All by default.
#' @param improvise A boolean indicating whether the algorithm should automatically expand the search to all features if the search is fruitless in the specified features. True by default.
#' @param verbose A boolean indicating whether match information should be printed. True by default.
setMethod(f = "selectPlot",
    signature = "ObjFeatureSet",
    definition = function(object, terms, features = NULL, improvise = TRUE, verbose = TRUE) {
        # slot(object, features)
        df <- as(object, "data.frame")
        # special case: because ggplot likes to spell color with a "u"
        if (length(grep("color", features, ignore.case = TRUE))>0) {
            features <- c(features, "colour")
            message("Also checking alternative spellings of features...")
        }
        # if we grep NULL, it conveniently returns all columns anyway
        features <- grep(paste(features, collapse="|"), colnames(df), 
            ignore.case = TRUE, value = TRUE)
        if (length(features)==0) {
            if (improvise) {
                message("No features matching your query could be found -- searching all features.")
                features <- colnames(df)
            } else {
                # message("No features matching your query could be found.")
                return(FALSE)

                ## this is slow

                # valid.resp <- FALSE
                # while(!valid.resp) {
                #     resp <- readline("No features matching your query could be found -- search all features? ")
                #     valid.resp <- (resp%in%c("y", "n"))
                # }
                # if(resp=="n"){
                #     return(FALSE)
                # } else {
                #     features <- colnames(df)
                # }
            }
        }
        search.res <- grep(paste(terms, collapse="|"), df[,features, drop=FALSE],
            ignore.case = TRUE, value = TRUE)
        if (verbose&(length(search.res) > 0)) {
            message("Match(es) found:")
            # eventually print plot ID here too
            message(paste0("\t", names(search.res), ": ", search.res, "\n"))
        }
        return(length(search.res) > 0)
    }
)

# http://stackoverflow.com/a/8142955
flatten4 <- function(x) {
    while(any(vapply(x, is.list, logical(1)))) {
        x <- lapply(x, function(x) if(is.list(x)) x else list(x))
        x <- unlist(x, recursive=FALSE) 
    }
    code.type <- sapply(x, is.function)
    if(length(which(code.type))>0) {
        x[code.type] <- paste(deparse(x[[which(code.type)]]), collapse="")
    }
    x
}

is.code = function(x) is.function(x) || is.expression(x) || is.call(x) || is.name(x)


flatten5 <- function(x) {
    x$object <- NULL
    x$data <- NULL
    while(any(vapply(x, is.list, logical(1)))) {
        x <- lapply(x, function(x) if(is.list(x)) x else list(x))
        x <- unlist(x, recursive=FALSE) 
    }
#    code.type <- sapply(x, is.function)
    code.type <- sapply(x, is.code)
    if(length(which(code.type))>0) {
        x[code.type] <- lapply(x[code.type], function(y)
            paste(deparse(y), collapse="\n"))
    }
    x
}



