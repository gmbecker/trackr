#' @importFrom rlang quo_name
ggplot_2.0 = function(strict = FALSE) {
    curvers = as.character(packageVersion("ggplot2"))
    res = compareVersion(curvers, "2.0.0") >=0
    if(strict)
        res = res && compareVersion(curvers, "2.2.1") <= 0
    res
    
    
}

ggplot_2.3 = function()  {
    curvers = as.character(packageVersion("ggplot2"))
    compareVersion(curvers, "2.2.1") > 0
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

.ggplotPanelLayoutDF = function(builtobj) {
    if(ggplot_2.3()) {
        ldf = builtobj$layout$layout
    } else if(ggplot_2.0(strict = TRUE)) {
        ldf = builtobj$layout$panel_layout
    } else {
        ldf = builtobj$panel$layout
    }
    ldf
}
.ggplotPanelLevels = function(builtobj, pnames, ldf = .ggplotPanelLayoutDF(builtobj)) {
    apply(ldf[pnames], 2, function(x) as.character(unique(x)))
}

.ggplotFacetVars = function(object) {
    if(is.null(object$facet))
        return(NULL)
    if(ggplot_2.3()) {
        ret = object$facet$vars()
        if(length(ret) == 0)
            ret = NULL
    } else {
        if(ggplot_2.0()) {
            facob = object$facet$params
        } else { ## "very old" versions of ggplot2 < 2.0, not well tested
            facob = object$facet
        }
        ## look for wrap style faceting first
        ret = names(facob$facets)      
        ## grid style faceting
        if(is.null(ret))
            ret = c(names(facob$rows), names(facob$cols))
    }
    ret
}

.ggplotMappingVars = function(object) {
    if(ggplot_2.3())
        namefun = function(x) lapply(x, quo_name)
    else
        namefun = as.character
    if (length(object$mapping) > 0) {
        data.names <- namefun(object$mapping)
        } else { ## sometimes mappings are in layers
            mps = lapply(object$layers, function(x) x$mapping)
            mps = mps[sapply(mps, function(x) length(x) > 0)]
            if(length(mps) ==0)
                warning("No mappings found")
            else
                data.names <- namefun(mps[[1]])
            
        }
    data.names
}
