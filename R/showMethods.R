.showHeader = function(object, restype = if(is(object, "ObjFeatureSet")) object@klass else "Unknown result type",
                       file = object@analysisfile) {
    cat("An", class(object), "for a", restype,
        "\nid:", uniqueID(object), 
        "\ntags:", object@tags, 
        "\nlocation:",length(code), "lines of code in",
        if(length(file) > 0 && !is.na(file) && nzchar(file)) basename(file) else "<unknown file>",  "within",
        "\n\trstudio project:", object@rstudioproject,
        "\n\tpackage:", if(length(object@analysispkg) > 0) object@analysispkg$Package else character(), sep=" ")
}

.showPlotHeader = function(object, ...) {
    .showHeader(object, ...)
    tmpvarlabs = unlist(object@varlabels)
    cat("\ntitles:", object@titles, 
        "\nvars:", paste(paste0(tmpvarlabs, " <", names(tmpvarlabs), ">"),
                       collapse = ", "), 
        "\nfacets:", if(length(object@grouping) > 0)  object@grouping$panel$vars,
        sep = " ")
}

setMethod("show", "FeatureSet",
          function(object) {
    .showHeader(object)
    cat("\n\n")
})

setMethod("show", "PlotFeatureSet",
          function(object) {
    .showPlotHeader(object, restype = paste(object@klass[1], "plot"))
})

setMethod("show", "GGplotFeatureSet",
          function(object) {
    .showPlotHeader(object, restype = "ggplot2 plot")
    cat("\ngeom(s):", object@geom$type,
        "\nstat(s):", object@stat$type, "\n\n",
        sep=" ")
})

setMethod("show", "RmdFeatureSet",
          function(object) {
    .showHeader(object, "Rmd woven report", file = object@rmdfile)
    cat("\ncontains: \n\t", length(object@chunks), "chunks total,\n\t",
        length(object@fullcode), "lines of code\n\t",
        object@numplots, "plots and", object@numouts - object@numplots,
        "other displayed outputs\n\n", sep = " ")
})
