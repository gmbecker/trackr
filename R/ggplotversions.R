ggplot_2.0 = function() {
    compareVersion(as.character(packageVersion("ggplot2")), "2.0.0") >=0
    
}



#' @rdname nObs-methods
setMethod(f = "nObs",
    signature = "ggplot",
    definition = function(object) {
    data = fullData(object, quiet=TRUE)
    ## XXX: Figure out what to do about diff when data in layer v main call
    nrow(data[[1]]) 

    ## data = object$data

    
    
    ## if(is.null(data))
    ##     NA_integer_
    ## else if (is(data, "waiver")) {
    ##     dats = lapply(object$layers, function(x) x$data)
    ##     dats = dats[!sapply(dats, function(x) is.null(x) || is(x, "waiver"))]
    ##     if(length(dats) > 1) {
    ##         warning("Layers appear to have different data. This is not currently handled")
    ##         NA_integer_
    ##     } else if(length(dats) == 0)
    ##         NA_integer_
    ##     else
    ##         nrow(dats[[1]])
    ## } else {
    ##     nrow(data)
    ## }
}
)


.geom_name = function(layer) {
    if(ggplot_2.0()) {
        tolower(gsub("Geom", "", class(layer$geom)[1]))
    } else {
        layer$geom$objname
    }
}

.stat_name = function(layer) {
    if(ggplot_2.0()) {
        tolower(gsub("Stat", "", class(layer$stat)[1]))
    } else {
        layer$stat$objname
    }
}

.position_name = function(layer) {
    if(ggplot_2.0()) {
        tolower(gsub("Position", "", class(layer$position)[1]))
    } else {
        layer$position$objname
    }
}

