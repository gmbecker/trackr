#' @include Classes.R Generics.R
NULL

# ggplot and gg are S3 classes - must switch them over before sending to S4 method
# borrowed from ggsubplot:

#' @title ggplot S4 classes
#' @description formal S4 classes for S3 classes defined in other packages.
#' @name ggplot-class
#' @aliases ggplot-class
#' @rdname oldclasses
#' @exportClass ggplot
setOldClass(c("ggplot", "list"))

#' @name gg
#' @rdname oldclasses
#' @aliases gg-class
#' @exportClass gg
setOldClass(c("gg", "ggplot"))

# trellis is S3 class - must switch over before sending to S4 method

#' @name trellis
#' @rdname oldclasses
#' @aliases trellis-class
#' @exportClass trellis
setOldClass("trellis")

# gTree is S3 class - must switch over before sending to S4 method?

#' @name gTree
#' @rdname oldclasses
#' @aliases gTree-class
#' @exportClass gTree
setOldClass("gTree")


## ' @importFrom fastdigest fastdigest
## ' @title Create an 16 character "SpookyHashV2" hash of an R object.
## ' @param x An R object
## ' @return An character vector with a prefix and 16 character hash of \code{x}
gen_hash_id <- function(x) {
    # need to exclude regdate from hashing.
    # this is not ideal, but does the trick.
    # regDateTime(x) <- as.POSIXct("1970-01-01 00:00:01", tz = "UTC")
    ##    digest::digest(x, "xxhash32", seed = 0)
    paste0("SpkyV2_", fastdigest(x))
}

## Implementation of show method for PlotFeatureSet class

## doc hook is elsewhere
## ' @title Display an object of class/superclass PlotFeatureSet
## ' @description show a PlotFeatureSEt
## ' @rdname show-methods
## ' @param object An object of (super)class PlotFeatureSet.
## ' @return NULL
#' @export
setMethod(f = "show",
    signature = "PlotFeatureSet",
    definition = function(object) {
        cat("An object of class ", class(object), "\n", sep = "")
        cat(" containing a plot object created by ", object@package, 
            "\n entitled '", paste(titles(object), collapse = "; "),
            "' and registered on ", as.character(object@regdate), ".\n", sep = "")
        invisible(NULL)
    }
)

prettyPrint <- function(pw.el){
    if(is.null(names(pw.el))) {
        paste(pw.el, sep="", collapse = "\n\t\t\t\t\t\t")
    } else {
        paste(names(pw.el), pw.el, sep=": ", collapse = "\n\t\t\t\t\t\t")
    }
}

## Implementation of summary method for PlotFeatureSet class

#' @title Summarize an object of class/superclass PlotFeatureSet
#' @description Summary methods for PlotFeatureSet objects
#' @rdname summary-methods
#' @param object An object of (super)class PlotFeatureSet.
#' @return NULL
#' @export
setMethod(f = "summary",
    signature = "PlotFeatureSet",
    definition = function(object) {

        cat("--------------------\n")
        cat("Package:\t\t\t\t", graphSys(object), "\n", sep = "")
        cat("Registration time:\t\t", as.character(regDateTime(object)), "\n", sep = "")
        cat("Title(s):\t\t\t\t", prettyPrint(titles(object)), "\n", sep = "")
        cat("Grouping:\t\t\t\t", prettyPrint(groupInfo(object)), "\n", sep = "")
        cat("Coordinate System:\t\t", coordSystem(object), "\n", sep = "")

        cat("Variable...\n")
        # cat("  Names:\t\t\t\t", prettyPrint(dataNames(object)), "\n", sep = "")
        cat("  Labels:\t\t\t\t", prettyPrint(dataLabels(object)), "\n", sep = "")
        cat("  Types:\t\t\t\t", prettyPrint(dataTypes(object)), "\n", sep = "")

        cat("Num. Observations:\t\t", nObs(object), "\n", sep = "")
        cat("Legend?:\t\t\t\t", c("No", "Yes")[hasLegend(object)+1], "\n", sep = "")
        cat("Tags:\t\t\t\t\t", paste(tags(object), collapse=", "), "\n", sep = "")

        # annotationText(object)
        # allDataNamesTypes(object)

        if (class(object)=="GGplotFeatureSet") {
            cat("Layers:\t\t\t\t\t", nLayers(object), "\n", sep = "")
            cat("  Geometric Object(s):\t", prettyPrint(geomObject(object)), "\n", sep = "")
            cat("  Stat. Transform(s):\t", prettyPrint(statTransform(object)), "\n", sep = "")
            cat("  Positioning:\t\t\t", prettyPrint(position(object)), "\n", sep = "")
        }
        invisible(NULL)
    }          
)

## Implementation of plot method for PlotFeatureSet class

#' @title Print method for an object of class gTree.
#' @description print a gTree object.
#' @param x An object of class gTree.
#' @param ... Other named arguments (currently unused).
#' @rdname print-methods
#' @export
print.gTree <- function(x, ...) {
    grid::grid.newpage()
    grid::grid.draw(x)
}

#' @title Display the plot object owned by PlotFeatureSet objects and
#'     subclasses thereof.
#' @description (re)draw the plot associated with a PlotFeatureSEt
#'     record.
#' @rdname plot-methods
#' @param x An object of (super)class PlotFeatureSet.
#' @param y Should be left empty. Included only because it is a
#'     mandatory signature element.
#' @param ... Other named arguments (currently ignored).
#' @return The plot object, plotted through its native print method.
#' @importFrom graphics plot
#' @method plot PlotFeatureSet
#' @export
setMethod(f = "plot", 
    signature = signature(x = "PlotFeatureSet", y = "missing"),
    definition = function(x, y, ...) {
        print(x@object)
    }
)

## ' @title Internal utility function to warn the user that a requested feature is not yet implemented.
## ' @param reason An optional character vector describing the feature.
## ' @return NULL
not_implemented <- function(reason = NULL) {
    ## warning(paste("Functionality not yet implemented",
    ##     reason, sep=": "),
    ##     call. = FALSE
    ## )
    invisible(NULL)
}

## #' @param type The type of metadata-enriched image to save. Default is "png"
## #' @rdname enrichedPlot-methods
## #' @export
## setMethod(f = "saveEnrichedPlot",
##     signature = "PlotFeatureSet",
##     definition = function(object, filename, type = "png", ...) {
##         success <- saveBasicPlot(object, filename, type, ...)
##         ## we don't enrich plots anymore. It makes the image files too big. If we
##         ## want the object saved it should be submitted to the backend itself (e.g.,
##         ## in the client bundle of a grex)
        
##         invisible(NULL)
##     }
## )

## #' @describeIn enrichedPlot-methods Load the (R) plot object previously saved as metadata in an image file.
## #' @export
## loadEnrichedPlot <- function(filename, type = "png", ...) {
##     # just read in R object (png metadata)
##     res <- NULL
##     if (type=="png") {
##         if (file.exists(filename)) {
##             p.png <- png::readPNG(filename, info = TRUE)

##             res <- attr(p.png, "metadata")
##         } else {
##             warning("Plot not found.")
##         }
##     } else {
##         not_implemented("loading plot with metadata as type other than PNG.")
##     }
##     res
## }

#' @describeIn saveBasicPlot Save the plot object owned by an object of class/superclass PlotFeatureSet as an image.
setMethod(f = "saveBasicPlot",
    signature = "PlotFeatureSet",
    definition = function(object, filename, type = c("png", "jpeg", "jpg", "tiff", "tif", "bmp"), width = 7, height = 7, dpi = 300) {
        
        type <- match.arg(type)
        if(type=="jpeg") type <- "jpg"
    if(type=="tiff") type <- "tif"
    success <- FALSE
    if(is.null(object@object))
        return(FALSE)
    
    ## too much repetition -- refactor! look at ggsave for a good example.
    switch(type,
           png = {
        ## write "plain" PNG (no metadata)
        grDevices::png(filename, width = width, height = height, res = dpi, units = "in")
        print(object@object)
        invisible(dev.off())
        success <- TRUE
    },
    jpg = {
        grDevices::jpeg(filename, width = width, height = height, res = dpi, units = "in")
        print(object@object)
        invisible(dev.off())
        success <- TRUE
    },
    bmp = {
        grDevices::bmp(filename, width = width, height = height, res = dpi, units = "in")
        print(object@object)
        invisible(dev.off())
        success <- TRUE
    },
    tif = {
        grDevices::tiff(filename, width = width, height = height, res = dpi, units = "in")
        print(object@object)
        invisible(dev.off())
        success <- TRUE
    },
    not_implemented(paste0("saving plot object as type ", type, "."))
    )
    return(success)
})

## No-op for non-plot feature sets. This shouldn't be necessary but
## it is for now because of the way prep_for_backend is factored.
## TODO: make this unnecessary and then remove it

setMethod(f="saveBasicPlot",
##          signature = "FeatureSet",
          definition = function(object, filename, type = c("png", "jpeg", "jpg", "tiff", "tif", "bmp"), width = 7, height = 7, dpi = 300) {
    TRUE
})


## setMethod(f="saveEnrichedPlot",
##           signature = "FeatureSet",
##             definition = function(object, filename, type = "png", ...) {
##     TRUE
## })



# # @describeIn saveBasicPlot Save the plot object owned by an object of class/superclass PlotFeatureSet as an image.
# setMethod(f = "saveBasicPlot",
#     signature = "PlotFeatureSet",
#     definition = function(object, filename, type = c("png", "jpeg", "jpg", "tiff", "tif", "bmp"), ...) {
#         saveBasicPlot(object@object, filename, match.arg(type), ...)
#     }
# )

# # @describeIn saveBasicPlot Save a plot object of class ggplot as an image.
# setMethod(f = "saveBasicPlot",
#     signature = "ggplot",
#     definition = function(object, filename, type = c("png", "jpeg", "jpg", "tiff", "tif", "bmp"), width = 7, height = 7, dpi = 300) {

#         type <- match.arg(type)
#         if(type%in%c("eps", "ps", "tex", "pdf", "jpeg", "jpg", "tiff", "tif", "png", "bmp", "svg")) {
#             ## write "plain" image (no metadata) from ggplot object
#             ## (for png format, ggsave uses grDevices::png in the background)
#             ggplot2::ggsave(filename, object, 
#                 width = width, height = height, units = "in", dpi = dpi)
#             return(TRUE)
#         } else {
#             not_implemented(paste0("saving ggplot object as type ", type, "."))
#             return(FALSE)
#         }
#     }
# )

# # @describeIn saveBasicPlot Save a plot object of class trellis as an image.
# setMethod(f = "saveBasicPlot",
#     signature = "trellis",
#     definition = function(object, filename, type = c("png", "jpeg", "jpg", "tiff", "tif", "bmp"), width = 7, height = 7, dpi = 300) {

#         type <- match.arg(type)
#         if(type=="jpeg") type <- "jpg"
#         if(type=="tiff") type <- "tif"
#         success <- FALSE
#         # too much repetition -- refactor!
#         switch(type,
#             png = {
#                 ## write "plain" PNG (no metadata) from trellis object
#                 grDevices::png(filename, width = width, height = height, res = dpi, units = "in")
#                 print(object)
#                 invisible(dev.off())
#                 success <- TRUE
#             },
#             jpg = {
#                 grDevices::jpeg(filename, width = width, height = height, res = dpi, units = "in")
#                 print(object)
#                 invisible(dev.off())
#                 success <- TRUE
#             },
#             bmp = {
#                 grDevices::bmp(filename, width = width, height = height, res = dpi, units = "in")
#                 print(object)
#                 invisible(dev.off())
#                 success <- TRUE
#             },
#             tif = {
#                 grDevices::tiff(filename, width = width, height = height, res = dpi, units = "in")
#                 print(object)
#                 invisible(dev.off())
#                 success <- TRUE
#             },
#             not_implemented(paste0("saving trellis object as type ", type, "."))
#         )
#         return(success)
#     }
# )

## Implementation of mutator methods for PlotFeatureSet -----------------------

#' @rdname user-methods
setReplaceMethod(f = "user", 
    signature = "FeatureSet",
    definition = function(object, value) {
        object@user <- value
        # should do validity checking here
        # if (validObject(object)) {
            return(object)
        # }
    }
)

#' @rdname regDateTime-methods
setReplaceMethod(f = "regDateTime", 
    signature = "FeatureSet",
    definition = function(object, value) {
        object@regdate <- value
        # should do validity checking here
        # if (validObject(object)) {
            return(object)
        # }
    }
)

## Accessor methods for ObjFeatureSet ----------------------------------------

#' @rdname user-methods
setMethod(f = "user",
    signature = "FeatureSet",
    definition = function(object) {
        object@user
    }
)

#' @rdname regDateTime-methods
setMethod(f = "regDateTime",
    signature = "FeatureSet",
    definition = function(object) {
        object@regdate
    }
)

#' @rdname graphSys-methods
setMethod(f = "graphSys",
    signature = "PlotFeatureSet",
    definition = function(object) {
        object@package
    }
)

#' @rdname describePackage-methods
setMethod(f = "describePackage", 
    signature = "FeatureSet", 
    definition = function(object) {
        object@analysispkg
    }
)


## --------------------------------------------------------
## THIS SECTION ONLY:
## based on content in Hadley Wickham's Advanced R book (2015)
## relevant section available here: http://adv-r.had.co.nz/dsl.html
## an earlier version of the same code is here: https://gist.github.com/hadley/5576263

binary_op <- function(sep) {
    force(sep)
    function(e1, e2) {
        paste0(e1, sep, e2)
    }
}

# pretty.print <- function(x) {
#     UseMethod("pretty.print")
# }
# pretty.print.numeric <- function(x) {
#     print(x, digits = getOption("digits"))
# }

other_op <- function(op) {
    force(op)
    function(...) {
        op.args <- list(...)
        # invisible(sapply(op.args,message))
        num.args <- sapply(op.args, is.numeric)
        op.args[num.args] <- format(op.args[num.args])
        contents <- do.call(paste, c(op.args, collapse = ", ", sep=""))

        formatting.ops <- c("plain", "bold", "italic", "bolditalic", "symbol",
            "displaystyle", "textstyle", "scriptstyle", "scriptscriptstyle", "underline")
        if (op%in%formatting.ops) {
            # make this scenario fit into the switch statement below
            op <- "paste"
        }
        switch(op,
            atop = paste0("\n", contents),
            paste = paste(contents),
            list = paste(contents, sep=", "),
            paste0(op, "(", contents, ")"))

        # remove:
        # plain(x)
        # bold(x)
        # italic(x)
        # bolditalic(x)
        # symbol(x)
        # displaystyle(x)
        # textstyle(x)
        # scriptstyle(x)
        # scriptscriptstyle(x)
        # underline(x)
    }
}

all_names <- function(x) {
    if (is.atomic(x)) {
        character()
    } else if (is.name(x)) {
        as.character(x)
    } else if (is.call(x) || is.pairlist(x)) {
        children <- lapply(x[-1], all_names)
        unique(unlist(children))
    } else {
        stop("Don't know how to parse type ", typeof(x), call. = FALSE)
    }
}

all_calls <- function(x) {
    if (is.atomic(x) || is.name(x)) {
        character()
    } else if (is.call(x)) {
        fname <- as.character(x[[1]])
        children <- lapply(x[-1], all_calls)
        unique(c(fname, unlist(children)))
    } else if (is.pairlist(x)) {
        unique(unlist(lapply(x[-1], all_calls), use.names = FALSE))
    } else {
        stop("Don't know how to parse type ", typeof(x), call. = FALSE)
    }
}

latex_env <- function(expr) {
    # Binary operators
    f_env <- new.env(parent = emptyenv())
    f_env$"==" <- binary_op("=")
    f_env$"<=" <- binary_op("<=") # \leq
    f_env$">=" <- binary_op(">=") # \geq
    f_env$"!=" <- binary_op("!=") # \neq

    f_env$"%<->%" <- binary_op("<->") # \leftrightarrow
    f_env$"%->%" <- binary_op("->") # \rightarrow
    f_env$"%<-%" <- binary_op("<-") # \leftarrow
    f_env$"%<=>%" <- binary_op("<=>") # \Leftrightarrow
    f_env$"%=>%" <- binary_op("=>") # \Rightarrow
    f_env$"%<=%" <- binary_op("<=") # \Leftarrow

    # here we are communicating meaning but not same character
    f_env$"%==%" <- binary_op(":=") # \equiv
    f_env$"%/%" <- binary_op("/") # \div
    f_env$"%*%" <- binary_op("*") # \times
    f_env$"%~%" <- binary_op("~") # \sim

    # LaTeX seems necessary here
    f_env$"%.%" <- binary_op("\\cdot")
    f_env$"%+-%" <- binary_op("\\pm")
    f_env$"%~~%" <- binary_op("\\approx") # x is approximately equal to y
    f_env$"%=~%" <- binary_op("\\cong") # x and y are congruent
    f_env$"%prop%" <- binary_op("\\propto") # x is proportional to y
    f_env$"%subset%" <- binary_op("\\subset") # x is a proper subset of y
    f_env$"%subseteq%" <- binary_op("\\subseteq") # x is a subset of y
    f_env$"%notsubset%" <- binary_op("\\not\\subset") # x is not a subset of y
    f_env$"%supset%" <- binary_op("\\supset") # x is a proper superset of y
    f_env$"%supseteq%" <- binary_op("\\supseteq") # x is a superset of y
    f_env$"%in%" <- binary_op("\\in") # x is an element of y
    f_env$"%notin%" <- binary_op("\\not\\in") # x is not an element of y
    f_env$"%up%" <- binary_op("\\uparrow") # x up-arrow y
    f_env$"%down%" <- binary_op("\\downarrow") # x down-arrow y
    f_env$"%dblup%" <- binary_op("\\Uparrow") # x double-up-arrow y
    f_env$"%dbldown%" <- binary_op("\\Downarrow") # x double-down-arrow y

    # what about these??
    # \sqrt[y]{x}
    # x + phantom(0) + y : leave gap for "0", but don't draw it
    # x + over(1, phantom(0)) : leave vertical gap for "0" (don't draw)
    # frac(x, y) : x over y
    # over(x, y) : x over y

    # single characters
    f_env$"+" <- binary_op("+")
    f_env$"-" <- binary_op("-")
    f_env$"*" <- binary_op(" ") # x*y : juxtapose x and y
    f_env$"/" <- binary_op("/")
    f_env$"=" <- binary_op("=")
    f_env$"^" <- binary_op("^")
    f_env$"<" <- binary_op("<")
    f_env$">" <- binary_op(">")
    f_env$"[" <- binary_op("_")
    f_env$"~" <- binary_op(" ")

    calls <- all_calls(expr)
    call_list <- setNames(lapply(calls, other_op), calls)
    call_env <- list2env(call_list)
    f_env <- list2env(as.list(f_env), parent = call_env)
    symbols <- all_names(expr)
    symbol_list <- setNames(as.list(symbols), symbols)
    symbol_env <- list2env(symbol_list, parent = f_env)
}

texify <- function(expr) {
    if (is.expression(expr)) {
        sapply(expr, texify)
    } else {
        eval(expr, latex_env(expr))
    }
}

## --------------------------------------------------------

# return a character vector or NULL.
parse.label <- function(x) {
    # "language": "call" or "expression" (or "name", maybe not a concern?)
    # need to treat these differently?
    if(is.character(x)) {
        # assume line break indicates subtitle/sub-label
        x <- unlist(strsplit(x, "\\n"))
        # trim leading and trailing whitespace
        x <- sapply(x, trimws, USE.NAMES = FALSE)
        # get rid of empty list elements; return
        x[x!=""]
    } else if(is.expression(x)) {
        # each element of the expression is a call. parse each.
        # don't simplify, as that could break the list.
        # as.list(do.call(c, sapply(x, parse.label)))
        as.vector(sapply(x, parse.label))
    }
    else if(is.call(x)) {
        parse.label(texify(x))
    } else {
        # in case of NULL, or if something weird slips through
        NULL
    }
}

# inspired by http://stackoverflow.com/questions/25081263/
rm.null <- function(x, keep.list = FALSE) {
    # go down
    x <- sapply(x, 
        function(y) if (is.list(y)) rm.null(y, keep.list = keep.list) else y,
        USE.NAMES = FALSE, simplify = !keep.list)
    # and then back up
    x[sapply(x, length)>0]
}

clean.up.labels <- function(x) {
    x[c("x","y")] <- lapply(x[c("x","y")], parse.label)
    x$group <- lapply(x$group, parse.label)

    # remove empty lists and return
    rm.null(x, keep.list = TRUE)
}

# with apologies to Hadley
americanize <- function(x) {
    gsub("colour", "color", x)
}

get.grob.labels <- function(g) {
    if (inherits(g, "gList")) {
    # if (length(g) > 1) {
        unique(sapply(g, "[[", "label"))
    } else if (inherits(g, "grob")) {
    # } else if (length(g) > 0) {
        g$label
    } else {
        NULL
    }
}

prep.labels <- function(labs) {
    names(labs) <- americanize(names(labs))
    group.labs <- grep("^x|^y", names(labs), invert = TRUE, value = TRUE)
    if (length(group.labs) > 0) {
        # assign non-"x"/"y" elements to a sublist called "group"
        labs$group <- as.list(labs[group.labs])
        # remove non-"x*"/"y*"/"group" elements from main list
        labs <- labs[-which(names(labs)%in%group.labs)]
        # want to look at legend title too for data label?
        # or leave legend to separate slot, since may not be unique (multiple vars under one legend title)?
    }
    return(labs)
}

# this prioritizes x. may not work as expected with nested lists.
merge.named.lists <- function(x, y) {
    x <- x[!sapply(x, is.null)]
    newx.names <- union(names(x), names(y))
    newx <- setNames(vector("list", length = length(newx.names)), newx.names)
    sapply(newx.names, 
        function(i) ifelse(!is.null(x[[i]]), x[[i]], y[[i]]),
        simplify = FALSE)
}

# this keeps both sets
append.labels <- function(x, y) {
    non.group.labs <- grep("group", 
        union(names(x), names(y)), 
        value = TRUE, invert = TRUE)
    labs <- sapply(
        non.group.labs, 
        function(i) union(x[[i]], y[[i]]), 
        simplify = FALSE)

    labs$group <- sapply(
        union(names(x$group), names(y$group)), 
        function(i) union(x$group[[i]], y$group[[i]]), 
        simplify = FALSE)
    return(labs)
}

#' @rdname dataLabels-methods
setMethod(f = "dataLabels",
    signature = "PlotFeatureSet",
    definition = function(object) {
        dataLabels(object@object)
    }
)

#' @rdname dataLabels-methods
setMethod(f = "dataLabels",
    signature = "ggplot",
    definition = function(object) {

        # return everything that's set, except the title
        # x, y, 
        # alpha, colour/color, fill, shape/linetype, size, facet
        data.labs <- unlist(object$labels[setdiff(names(object$labels), "title")])
        empty.labs <- which(data.labs=="NA")
        if (length(empty.labs)>0) {
            data.labs <- data.labs[-empty.labs]
        }
        data.labs <- prep.labels(as.list(data.labs))
        data.labs <- clean.up.labels(data.labs)

        # if they're identical to the dataNames, they're redundant, and we should remove them
        # data.labs <- data.labs[!(data.labs %in% dataNames(object))]

        # yet another way to label vars
        data.labs.scales <- sapply(object$scales$scales, "[[", "name")
        data.labs.scales <- rm.null(data.labs.scales)
        if (length(data.labs.scales) > 0) {
            data.labs.scales.names <- sapply(object$scales$scales, "[[", "aesthetics")
            if(is.list(data.labs.scales.names)) {            
                for(i in seq_along(data.labs.scales)) {
                    # keep an eye on this
                    names(data.labs.scales)[i] <- data.labs.scales.names[[i]][1]
                }
            } else {
                names(data.labs.scales) <- data.labs.scales.names
            }
            data.labs.scales <- prep.labels(as.list(data.labs.scales))
        }

        data.labs <- append.labels(data.labs, data.labs.scales)
        if (!is.null(groupInfo(object)$panel$vars)) {
            data.labs$group$panel <- union(data.labs$group$panel, groupInfo(object)$panel$vars)
        }

        append.labels(data.labs, dataNames(object))
    }
)

#' @rdname dataLabels-methods
setMethod(f = "dataLabels",
    signature = "trellis",
    definition = function(object) {
        labs.x <- object$xlab.default
        if (!is.null(object$xlab)) {
            if ((!is.null(object$xlab)) & (is.character(object$xlab)) & (object$xlab!=object$xlab.default)){
                labs.x <- object$xlab
            }
        }

        labs.y <- object$ylab.default
        if (!is.null(object$ylab)) {
            if ((is.character(object$ylab)) & (object$ylab!=object$ylab.default)){
                labs.y <- object$ylab
            }
        }

        # add paneling/faceting variable(s) to "group"
        # plus other grouping variables: alpha/transparency, color, fill, shape/linetype, size
        labs.group <- NULL

        data.labs <- list(x = labs.x, y = labs.y, group = labs.group)
        data.labs <- clean.up.labels(data.labs)

        append.labels(data.labs, dataNames(object))
    }
)

#' @rdname dataLabels-methods
setMethod(f = "dataLabels",
    signature = "gTree",
    definition = function(object) {
        labs.x <- grid::getGrob(object, "xlab", grep=TRUE, global=TRUE)
        labs.y <- grid::getGrob(object, "ylab", grep=TRUE, global=TRUE)
        data.labs <- list(x = get.grob.labels(labs.x), y = get.grob.labels(labs.y))

        # if possible - add paneling/faceting variable(s) to "group"
        # plus other grouping variables: alpha/transparency, color, fill, shape/linetype, size

        clean.up.labels(data.labs)
    }
)

#' @rdname titles-methods
setMethod(f = "titles",
    signature = "PlotFeatureSet",
    definition = function(object) {
        titles(object@object)
    }
)

#' @rdname titles-methods
setMethod(f = "titles",
    signature = "ggplot",
    definition = function(object) {
        parse.label(object$labels$title)
    }
)

#' @rdname titles-methods
setMethod(f = "titles",
    signature = "trellis",
    definition = function(object) {
        # discarding subtitle info, for now...
        labs.main <- parse.label(object$main)
        labs.sub <- parse.label(object$sub)
        c(labs.main, labs.sub)
    }
)

#' @rdname titles-methods
setMethod(f = "titles",
    signature = "gTree",
    definition = function(object) {
        labs.main <- grid::getGrob(object, "main", grep=TRUE, global=TRUE)
        labs.main <- parse.label(get.grob.labels(labs.main))
        labs.sub <- grid::getGrob(object, "sub", grep=TRUE, global=TRUE)
        labs.sub <- parse.label(get.grob.labels(labs.sub))
        c(labs.main, labs.sub)
    }
)

#' @rdname annotationText-methods
setMethod(f = "annotationText",
    signature = "PlotFeatureSet",
    definition = function(object) {
        annotationText(object@object)
    }
)

#' @rdname annotationText-methods
setMethod(f = "annotationText",
    signature = "ggplot",
    definition = function(object) {
        p.text.layers <- object$layers[which(sapply(object$layers, function(x) x$geom$objname)=="text")]
        if (length(p.text.layers)>0) {
            as.character(sapply(p.text.layers, function(x) x$geom_params$label))
        } else {
            NULL
        }

        #### add call to parse.label for annotations, too!!!

    }
)

#' @rdname annotationText-methods
setMethod(f = "annotationText",
    signature = "trellis",
    definition = function(object) {
        # look at panel function
        # object$panel
        not_implemented("Annotation for trellis objects.")
        NULL
    }
)

#' @rdname annotationText-methods
setMethod(f = "annotationText",
    signature = "gTree",
    definition = function(object) {
        # note that mtext does NOT cover all cases.
        # also need to look at "text"
        # but ignore non-annotations within that category (like legends, etc.)
        # not clear how to tell these apart...

        # p.mtext.grobs <- grid::getGrob(object, "mtext", grep=TRUE, global=TRUE)
        # p.text.grobs <- grid::getGrob(object, "text", grep=TRUE, global=TRUE)
        # c(get.grob.labels(p.mtext.grobs), get.grob.labels(p.text.grobs))

        not_implemented("Annotation for gTree objects.")
        NULL
    }
)

# for example: "factor(x)"
strip.call <- function(x) {
    new.x <- gsub(paste(paste0("^", "[[:alnum:]_.]+", "\\("), collapse="|"), "", x)
    # also grab the trailing parenthesis
    # could do this in one call but that increases the likelihood of stripping out trailing parens in there by user definition...
    if (!identical(new.x, x)) {
        new.x <- gsub("\\)$", "", new.x)
    }
    new.x
}
# for example: "interaction(x, z)"
## superceded by below version, which will also handle x/z, but
## must be passed _only_ non-column terms to avoid ambiguity ~GB
strip.and.sep.call <- function(x) {
    new.x <- strip.call(x)

    # if a fxn was stripped, separate vars on comma
    # this could still separate variable names which contain commas by user definition...
    if (!identical(new.x, x)) {
        new.x <- unlist(strsplit(new.x, split=",[[:blank:]]*"))
    }
    new.x
}

strip.and.sep.call <-  function(x) {
    if(length(x) > 1)
        return(unlist(lapply(x, strip.and.sep.call)))
    cinfo = getInputs(readScript(txt = x))[[1]]
    cinfo@inputs
}

# #' @rdname plotCode-methods
# setMethod(f = "plotCode",
#     signature = "PlotFeatureSet",
#     definition = function(object) {
#         plotCode(object@object)
#     }
# )

#' @rdname fullData-methods
setMethod(f = "fullData",
    signature = "PlotFeatureSet",
    definition = function(object, quiet = FALSE) {
        fullData(object@object, quiet = quiet)
    }
)

#' @rdname fullData-methods
setMethod(f = "fullData",
    signature = "ggplot",
    definition = function(object, quiet = FALSE) {
        # only include columns that are relevant.
        # this assumes that dataNames grabs all relevant var names.
        p <- unique(unlist(dataNames(object), use.names = FALSE))

        # data is not retained in the same way when the 
        # entire data.frame is not provided in the call to ggplot

        # what about when multiple sets of data are provided?
        # need to return a list of data.frames
        p.extra.data <- sapply(lapply(object$layers, as.list), "[[", "data", simplify=FALSE)
        if (length(p.extra.data) > 0) {
            p.extra.data <- p.extra.data[sapply(p.extra.data, function(x) class(x)!="waiver")]
        } else {
            p.extra.data <- NULL
        }

        p.main.data <- NULL
        # ggplot2:::is.waive()
        if (length(object$data)>0) {
            cls = colnames(object$data)
            noncols = setdiff(p, cls)
            if (length(noncols)>0) {
                # check first before stripping...
                p <- c(p[p %in% cls], strip.and.sep.call(noncols))
                p <- p[!duplicated(p)]
                
            }
            # just in case object is, for example, a cast_df, etc.
            p.main.data <- list(as.data.frame(object$data[, p, drop=FALSE]))
        } else {
            if(!quiet)
                warning("Only plotted data will be returned, which may be limited to a summary of the original data. This may be due to use of vectors (rather than a data.frame) in the ggplot call.")
            # is there a case where this will be longer than 1?

            suppressMessages(built.object <- ggplot2::ggplot_build(object))
            # if(length(built.object$data)>1) {
            #     warning("data list is being truncated!")
            # }
            # built.object$data[[1]]
            p.main.data <- built.object$data
        }
        return(c(p.main.data, p.extra.data))
    }
)

#' @rdname fullData-methods
setMethod(f = "fullData",
    signature = "trellis",
    definition = function(object, quiet = FALSE) {
        df.list <- lapply(object$panel.args, as.data.frame)
        df <- do.call(rbind, df.list)
        # this works for x and y... are there others? will this work for any situation?
        colnames(df) <- unlist(dataNames(object)[colnames(df)], 
            use.names=FALSE)
        if (length(object$packet.sizes) > 1) {
            panel.df <- as.data.frame.table(object$packet.sizes)
            # assuming here that "Freq" will be the last column in the panel df.
            # don't want to call it by name in case one of the panel vars is called Freq.
            # checking here because if lattice summarizes,
            # it does not change the panel vars accordingly.
            if (!isTRUE(all.equal(sapply(df.list, nrow), panel.df[,ncol(panel.df)]))) {
                panel.df[,ncol(panel.df)] <- sapply(df.list, nrow)
            }
            panel.df <- panel.df[rep(seq(nrow(panel.df)), panel.df[,ncol(panel.df)]), 
                -ncol(panel.df), drop=FALSE]
            colnames(panel.df) <- strip.call(dataNames(object)$group$panel)
            df <- cbind(df, panel.df)
        }
        df
    }
)

#' @rdname fullData-methods
setMethod(f = "fullData",
    signature = "gTree",
    definition = function(object) {
        not_implemented("Data from gTree objects.")
        # in the case of a simple scatterplot:
        # data.frame(
        #   x = as.vector(grid::getGrob(object, "points", grep=TRUE)$x),
        #   y = as.vector(grid::getGrob(object, "points", grep=TRUE)$y),
        # )
        data.frame()
    }
)

setMethod(f = "dataNames",
    signature = "PlotFeatureSet",
    definition = function(object) {
        dataNames(object@object)
    }
)

setMethod(f = "dataNames",
    signature = "ggplot",
    definition = function(object) {
        data.names <- NULL
        if (length(object$mapping) > 0) {
            data.names <- as.character(object$mapping)
        } else { ## sometimes mappings are in layers
            mps = lapply(object$layers, function(x) x$mapping)
            mps = mps[sapply(mps, function(x) length(x) > 0)]
            if(length(mps) ==0)
                warning("No mappings found")
            else
                data.names <- as.character(mps[[1]])

        }
        data.names <- as.list(data.names)

        # wrap
        data.names$panel <- names(object$facet$facets)
        if (is.null(data.names$panel)) {
            # grid
            data.names$panel <- c(names(object$facet$rows), names(object$facet$cols))
        }

        # c(data.names, panel = paste(panel.names, collapse=", "))
        prep.labels(data.names)
    }
)

setMethod(f = "dataNames",
    signature = "trellis",
    definition = function(object) {
        # panel.names <- NA_character_
        panel.names <- NULL
        if (!is.null(names(object$condlevels))) {
            # panel.names <- paste(names(object$condlevels), collapse=", ")
            panel.names <- names(object$condlevels)
        }

        group.names <- NULL
        if ("groups"%in%names(as.list(getCall(object)))) {
            group.names <- deparse(as.list(getCall(object))$groups)
        }

        # work-around for what appears to be a bug in lattice::qq
        # where ylab.default is assigned unique(levels(y))[y]
        # rather than unique(levels(y))[2]
        # ideally should also check if (object$panel=="panel.qq")
        # (in the case of character object$panel), or
        # panel.qq is called by object$panel (in the case of formula object$panel)
        if (length(object$ylab.default)>1) {
            # ideally we'd want to assign this to the second level of the original factor
            # but without the original data frame this will have to do
            # if the labels haven't been changed, this should return the correct label.
            # if the labels have been changed, but there are only two levels, we can return the non-x level
            # if we can't make an educated guess, return the custom label
            if (object$ylab%in%object$ylab.default) {
                object$ylab.default <- object$ylab
            } else if(length(unique(object$ylab.default))==2) {
                not.x <- unique(object$ylab.default)!=object$xlab.default
                if(sum(not.x)==1){
                    object$ylab.default <- unique(object$ylab.default)[not.x]
                }
            } else {
                # a little redundant but ok
                object$ylab.default <- object$ylab
            }
        }

        data.names <- list(x = object$xlab.default, y = object$ylab.default,
            group = list(color = group.names, panel = panel.names))
        rm.null(data.names, keep.list = TRUE)
    }
)

setMethod(f = "dataNames",
    signature = "gTree",
    definition = function(object) {
        not_implemented("Data names from gTree objects.")
        list()
    }
)

#' @rdname dataTypes-methods
setMethod(f = "dataTypes",
    signature = "PlotFeatureSet",
    definition = function(object) {
        dataTypes(object@object)
    }
)

.safeclass = function(df, col) {
    if(col %in% names(df))
        class(df[[col]])[1]
    else
        NA_character_
}


#' @rdname dataTypes-methods
setMethod(f = "dataTypes",
    signature = "ggplot",
    definition = function(object) {

    ## dat = fullData(object, quiet = TRUE)
    ## vars = dataNames(object)
    ## var.type.list = lapply(vars, function(varnm) {
    ##     typs = sapply(dat, .safeclass, col = varnm)
    ##     typs = typs[!is.na(typs)]
    ##     utyp = unique(typs)
    ##     if(length(utyp) ==0)
    ##         stop("Unable to determine type (class) for variable ", varnm)
    ##     ## if(length(utyp) > 1)
    ##     ##     stop("Variable ", varname, " has different classes in the data for different layers This is not currently supported. Please contact the maintainer if you need this feature.")
    ##     utyp
    ## })
    ## names(var.type.list) = names(vars)
        # a hack to get the highest-level class for each variable
        # (for example, this grabs "factor" for objects of class "ordered" and "factor")
        all.vartypes <- sapply(object$data, function(x) class(x)[length(class(x))])
        vartypes <- all.vartypes[which(names(all.vartypes)%in%unlist(dataNames(object)))]


    
    
    
    
        # need to fix the names to be the dims - maybe with a merge?
        # names(vartypes) <- names(unlist(dataNames(object)))[
        #     match(names(vartypes), unname(unlist(dataNames(object))))]

        var.type.list <- dataNames(object)
        # keep same structure as var.names
        for (i in seq(vartypes)) {
            var.type.list <- rapply(var.type.list, gsub, 
                pattern = paste0("^",names(vartypes)[i],"$"), 
                replacement = vartypes[i], how = "list")
        }

        # is this still needed?
        # now fix anything that could have multiple entries:
        # multi.type <- names(which(sapply(dataNames(object), length)>1))
        # if (length(multi.type)>0) {
        #     multi.type.idx <- sapply(multi.type, 
        #             function(x) grep(paste0("^",x), names(vartypes)),
        #         simplify = FALSE)
        #     vartypes <- c(vartypes[-unlist(multi.type.idx)],
        #         sapply(multi.type.idx, function(i) list(unname(vartypes[i]))))
        # }
        # return(as.list(vartypes))

        return(var.type.list)
    }
)

#' @rdname dataTypes-methods
setMethod(f = "dataTypes",
    signature = "trellis",
    definition = function(object) {
        # this needs work
        x.type <- y.type <- NA_character_
        if (!is.null(object$panel.args[[1]]$x)) {
            x.type <- class(object$panel.args[[1]]$x)
        }
        if (!is.null(object$panel.args[[1]]$y)) {
            y.type <- class(object$panel.args[[1]]$y)
        }
        panel.types <- NULL
        if (length(groupInfo(object)$panel)>0) {
            if (groupInfo(object)$panel$num.vars>1) {
                panel.types <- unname(
                    sapply(fullData(object, quiet = TRUE)[,groupInfo(object)$panel$vars], class))
            } else if (groupInfo(object)$panel$num.vars==1) {
                panel.types <- class(fullData(object, quiet =TRUE)[,groupInfo(object)$panel$vars])
            }
        }
        
        list(x = x.type, y = y.type, 
            group = list(panel = panel.types))
    }
)

#' @rdname dataTypes-methods
setMethod(f = "dataTypes",
    signature = "gTree",
    definition = function(object) {
        not_implemented("Data types from gTree objects.")
        list()
    }
)

## add function regarding aesthetics
# color:
# unique(ggplot2::ggplot_build(object)$data[[1]]$colour)

#' @rdname geomObject-methods
setMethod(f = "geomObject",
    signature = "PlotFeatureSet",
    definition = function(object) {
        geomObject(object@object)
    }
)

#' @rdname geomObject-methods
setMethod(f = "geomObject",
          signature = "ggplot",
          definition = function(object) {
    suppressWarnings(requireNamespace("proto", quietly=TRUE))
    
    ## access to geom name thanks to http://stackoverflow.com/questions/13457562/
    ## if original geom order does not matter, could sort these before collapsing
    ## but should re-sort stat in geom order too
    ## may also want an auxillary function that "translates" ggplot's geoms into more common language
    p.geom.params <- sapply(lapply(object$layers, as.list), "[[", "geom_params",
                            simplify=FALSE)
    ## names(p.geom.params) <- make.names(sapply(lapply(object$layers, as.list), 
    ##     function(x) x$geom$objname))
    
    names(p.geom.params) <- make.names(sapply(lapply(object$layers, as.list),
                                              .geom_name))
    
    ## for the future
    ## sapply(lapply(object$layers, as.list), x$geom$parent.env()$objname)

    ##in ggplot 2.0 default_aes is not a function anymore, at least sometimes.
    
    p.geom.defaults <- sapply(lapply(object$layers, as.list), 
                              function(x) {
        if(is(x$geom$default_aes, "function"))
            x$geom$default_aes()
        else
            x$geom$default_aes
    }, simplify=FALSE)
    ## sapply(lapply(object$layers, as.list), 
    ##   function(x) x$stat$default_geom(), simplify=FALSE)
    
    p.geom <- mapply(merge.named.lists, p.geom.params, p.geom.defaults, SIMPLIFY = FALSE)

    p.geom <- sapply(p.geom, 
                     function(x) setNames(object = x, americanize(names(x))), simplify=FALSE)
    list(type = names(p.geom), params = p.geom)
})

#' @rdname geomObject-methods
setMethod(f = "geomObject",
    signature = "trellis",
    definition = function(object) {
        list()
        not_implemented("trellis objects don't understand geometric objects.")
    }
)

#' @rdname geomObject-methods
setMethod(f = "geomObject",
    signature = "gTree",
    definition = function(object) {
        not_implemented("gTree objects don't understand geometric objects.")
        list()
    }
)

#' @rdname statTransform-methods
setMethod(f = "statTransform",
    signature = "PlotFeatureSet",
    definition = function(object) {
        statTransform(object@object)
    }
)

#' @rdname statTransform-methods
setMethod(f = "statTransform",
    signature = "ggplot",
    definition = function(object) {
        p.stat.params <- sapply(lapply(object$layers, as.list), "[[", "stat_params",
            simplify=FALSE)
        names(p.stat.params) <- make.names(sapply(lapply(object$layers, as.list), 
                                                  .stat_name))
                                                  ##         function(x) x$stat$objname))

        ## causing issues.  may not be appropriate to combine stat settings with 
        ## default aesthetic settings for a stat. revisit later.
        # require(proto)
        # p.stat.defaults <- sapply(lapply(object$layers, as.list), 
        #     function(x) x$stat$default_aes(), simplify=FALSE)

        # p.stat <- mapply(merge.named.lists, p.stat.params, p.stat.defaults, SIMPLIFY = FALSE)
        p.stat <- p.stat.params

        # probably no "colour" in stat but just in case...
        p.stat <- sapply(p.stat, 
                function(x) {if(length(x) > 0){ names(x) <- americanize(names(x)) }; x },
            simplify=FALSE)

        list(type = names(p.stat), params = p.stat)
    }
)

#' @rdname statTransform-methods
setMethod(f = "statTransform",
    signature = "trellis",
    definition = function(object) {
        list()
        not_implemented("trellis objects don't understand statistical transformations.")
    }
)

#' @rdname statTransform-methods
setMethod(f = "statTransform",
    signature = "gTree",
    definition = function(object) {
        not_implemented("gTree objects don't understand statistical transformations.")
        list()
    }
)

#' @rdname groupInfo-methods
setMethod(f = "groupInfo",
    signature = "PlotFeatureSet",
    definition = function(object) {
        groupInfo(object@object)
    }
)


.ggplotPanelScheme = function(obj, v2.0 = ggplot_2.0()) {
    if(v2.0) {
        panel.scheme = tolower(gsub("Facet(.*)", "\\1", class(obj$facet)[[1]]))
    } else {
        panel.scheme <- grep("facet", class(obj$facet), value=TRUE, invert=TRUE)
    }
    
    if(panel.scheme=="null"){
        panel.scheme <- "none"   
    }
    panel.scheme

}

.ggplotPanelNames = function(obj, psch, v2.0 = ggplot_2.0()) {
    if(v2.0) {
        facinfo = obj$facet$params
    } else {
        facinfo = obj$facet
    }
    ret = NULL
    if(psch =="wrap")
        ret = names(facinfo$facets)
    else if (psch == "grid")
        ret = c(names(facinfo$rows), names(facinfo$cols))
    ret
    
}

.ggplotPanelLayoutDF = function(builtobj, v2.0=ggplot_2.0()){
    
    if(v2.0) {
        ldf = builtobj$layout$panel_layout
    } else {
        ldf = builtobj$panel$layout
    }
    ldf
}
.ggplotPanelLevels = function(builtobj, pnames, ldf = .ggplotPanelLayoutDF(builtobj)) {
    apply(ldf[pnames], 2, function(x) as.character(unique(x)))
}


#' @rdname groupInfo-methods
setMethod(f = "groupInfo",
    signature = "ggplot",
    definition = function(object) {
    panel.scheme = .ggplotPanelScheme(object)
    panel.names = .ggplotPanelNames(object, panel.scheme)
    ## panel.names <- NULL
    ## if (panel.scheme=="wrap") {
    ##     panel.names <- names(object$facet$facets)
    ## } else if (panel.scheme=="grid") {
    ##     panel.names <- c(names(object$facet$rows), names(object$facet$cols))
    ## }
    
    suppressMessages(built.object <- ggplot2::ggplot_build(object))
                                        # convert factor to character
    panel.levels <- .ggplotPanelLevels(built.object, pnames = panel.names)
    ## make into a list for uniformity
    if (is.matrix(panel.levels)) {
        panel.levels <- split(panel.levels, 
                              rep(colnames(panel.levels), each = nrow(panel.levels)))
    }
    ## this parses calls like `factor(x)` and leaves us with just `x`
    ## not necessary for ggplot panels right now b/c it doesn't allow this to happen
    ## panel.names.info <- CodeDepends::readScript(txt = panel.names)
    ## panel.names <- sapply(CodeDepends::getInputs(panel.names.info), slot, "inputs")
    
    ## names(panel.info$var.levels) <- NULL
    
    panel.info <- list(
        count = nrow(.ggplotPanelLayoutDF(built.object)),
                                        # sizes = , # this could be tricky b/c ggplot summarizes...
                                        # scheme = panel.scheme,
        vars = panel.names,
        levels = collapse_to_str(panel.levels),
                                        # levels = panel.levels,
        num.vars = length(panel.names))
    if (panel.info$count==1) {
        panel.info = list()
    }
    list(panel = panel.info)
})

#' @rdname groupInfo-methods
setMethod(f = "groupInfo",
    signature = "trellis",
    definition = function(object) {
        # this parses calls like `factor(x)` and leaves us with just `x`
        # overkill for now - custom function will do ok
        # panel.names <- names(object$condlevels)
        # panel.names.info <- CodeDepends::readScript(txt = panel.names)
        # panel.names <- sapply(CodeDepends::getInputs(panel.names.info), slot, "inputs")

        panel.names <- strip.call(names(object$condlevels))

        panel.info <- list(
            count = length(object$panel.args),
            # sizes = object$packet.sizes,
            vars = panel.names,
            # levels = object$condlevels,
            levels = collapse_to_str(object$condlevels),
            num.vars = length(object$condlevels))
        # names(panel.info$var.levels) <- NULL
        if (panel.info$count==1) {
            # panel.info$var.count <- 0
            panel.info = list()
        }

        list(panel = panel.info)
    }
)

#' @rdname groupInfo-methods
setMethod(f = "groupInfo",
    signature = "gTree",
    definition = function(object) {
        not_implemented("Grouping info from gTree objects.")
        list()
    }
)


# suppressMessages(built.object <- ggplot2::ggplot_build(object))
# built.object$data

# $ data :List of 1
#   ..$ :'data.frame':    1000 obs. of  5 variables:
#   .. ..$ colour: chr [1:1000] "#984EA3" "#4DAF4A" "#984EA3" "#FF7F00" ...
#   .. ..$ x     : num [1:1000] 1.27 0.71 0.5 0.3 1.01 0.72 1.2 0.52 0.4 0.41 ...
#   .. ..$ y     : num [1:1000] 7312 3007 1581 673 3461 ...
#   .. ..$ PANEL : int [1:1000] 1 1 1 1 1 1 1 1 1 1 ...
#   .. ..$ group : int [1:1000] 4 3 4 5 2 2 4 6 3 6 ...

#   ggplot_build(pg)$data


#' @rdname nObs-methods
setMethod(f = "nObs",
    signature = "PlotFeatureSet",
    definition = function(object) {
        nObs(object@object)
    }
)

#' @rdname nObs-methods
setMethod(f = "nObs",
    signature = "trellis",
    definition = function(object) {
        # this may need work...
        sum(sapply(object$panel.args, function(i) length(i$x)))
    }
)

#' @rdname nObs-methods
setMethod(f = "nObs",
    signature = "gTree",
    definition = function(object) {
        not_implemented("Number of observations from gTree objects.")
        NA_integer_
    }
)

#' @rdname nLayers-methods
setMethod(f = "nLayers",
    signature = "PlotFeatureSet",
    definition = function(object) {
        nLayers(object@object)
    }
)

#' @rdname nLayers-methods
setMethod(f = "nLayers",
    signature = "ggplot",
    definition = function(object) {
        length(object$layers)
    }
)

#' @rdname nLayers-methods
setMethod(f = "nLayers",
    signature = "trellis",
    definition = function(object) {
        # refine later
        NA_integer_
        not_implemented("trellis objects don't understand layers.")
    }
)

#' @rdname nLayers-methods
setMethod(f = "nLayers",
    signature = "gTree",
    definition = function(object) {
        # refine later
        NA_integer_
        not_implemented("gTree objects don't understand layers.")
    }
)

#' @rdname position-methods
setMethod(f = "position",
    signature = "PlotFeatureSet",
    definition = function(object) {
        position(object@object)
    }
)

#' @rdname position-methods
setMethod(f = "position",
    signature = "ggplot",
    definition = function(object) {

        p.pos <- lapply(lapply(object$layers, as.list), function(x) as.list(x$position))
        # p.pos.params <- sapply(lapply(object$layers, as.list), "[[", "position",
            # simplify=FALSE)
        names(p.pos) <- make.names(sapply(lapply(object$layers, as.list), 
            ## function(x) x$position$objname))
                                          .position_name))
        # names(p.pos.params) <- sapply(lapply(object$layers, as.list), 
        #     function(x) parent.env(x$position)$objname)
        list(type = names(p.pos), params = p.pos)
    }
)

#' @rdname position-methods
setMethod(f = "position",
    signature = "trellis",
    definition = function(object) {
        list()
        not_implemented("trellis objects don't understand positioning.")
    }
)

#' @rdname position-methods
setMethod(f = "position",
    signature = "gTree",
    definition = function(object) {
        list()
        not_implemented("gTree objects don't understand positioning.")
    }
)

#' @rdname hasLegend-methods
setMethod(f = "hasLegend",
    signature = "PlotFeatureSet",
    definition = function(object) {
        hasLegend(object@object)
    }
)

#' @rdname hasLegend-methods
setMethod(f = "hasLegend",
    signature = "ggplot",
    definition = function(object) {

        # suppressMessages(built.object <- ggplot2::ggplot_build(object))
        # scale.legends <- sapply(built.object$plot$scales$scales, function(x) x$guide)
        # # other valid options include FALSE and "none"... are there others?
        # scale.haslegend <- (scale.legends=="legend"|scale.legends=="colourbar"|scale.legends=="colorbar")

        # scales.with.legend <- unlist(
        #     lapply(built.object$plot$scales$scales, 
        #         function(x) x$aesthetics)[scale.haslegend])

        # pos <- object$theme$legend.position
        # # check modified from plot-render.r in ggplot2
        # if (length(pos) > 1) {
        #     pos <- "manual"
        # }
        # # if default theme...
        # if (is.null(pos)) {
        #     pos <- ggplot2::theme_get()$legend.position
        # }

        # all.guides.removed <- FALSE
        # # if there are some guides at the top level of the plot
        # if (!is.null(object$guides)) {
        #     # and if all of the guides that should appear by default
        #     # appear in the list of guides at the top level of the plot
        #     if (length(setdiff(scales.with.legend, names(object$guides)))==0) {
        #         # then be sure none of them are "guides"
        #         if (all(unlist(!sapply(object$guides, inherits, "guide")))) {
        #             # then be sure that they are all set to FALSE.
        #             # if so, they are really all removed.
        #             all.guides.removed <- all(!unlist(object$guides[scales.with.legend]))
        #         }
        #     }
        # }
        # haslegend <- (any(scale.haslegend) & (pos!="none") & (!all.guides.removed))

        # soooooo much easier than the above...
        # except that it opens a plot window. https://github.com/hadley/ggplot2/issues/809
        # this should fix that...
        pdf(file = NULL)
        suppressMessages(grid.object <- ggplot2::ggplotGrob(object))
        haslegend <- "guide-box"%in%grid.object$layout$name
        invisible(dev.off())

        return(haslegend)
    }
)

#' @rdname hasLegend-methods
setMethod(f = "hasLegend",
    signature = "trellis",
    definition = function(object) {
        return(!is.null(object$legend))
    }
)

#' @rdname hasLegend-methods
setMethod(f = "hasLegend",
    signature = "gTree",
    definition = function(object) {
        not_implemented("Legends from gTree objects.")
        NA
    }
)

#' @rdname coordSystem-methods
setMethod(f = "coordSystem",
    signature = "PlotFeatureSet",
    definition = function(object) {
        coordSystem(object@object)
    }
)

#' @rdname coordSystem-methods
setMethod(f = "coordSystem",
    signature = "ggplot",
    definition = function(object) {
        p.coordsys <- class(object$coordinates)[1]
        if (p.coordsys %in% c("fixed", "equal", "flip", "trans")) {
            p.coordsys <- "cartesian"
        } else if (p.coordsys=="map") {
            p.coordsys <- paste(p.coordsys, object$coordinates$projection, sep=": ")
        }
        p.coordsys

        ## need to implement
        # if (!is.null(object$coordinates$trans)) {
        ## do something with object$coordinates$trans
        # } else {
        # assume identity transformation
        # }
    }
)

#' @rdname coordSystem-methods
setMethod(f = "coordSystem",
    signature = "trellis",
    definition = function(object) {
        return("cartesian")
    }
)

#' @rdname coordSystem-methods
setMethod(f = "coordSystem",
    signature = "gTree",
    definition = function(object) {
        not_implemented("Coordinate systems for gTree objects.")
        character(0)
    }
)
