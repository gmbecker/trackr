recplothook = function(x, opts, ...) {
    if(is.null(defaultVTDB())) {
        warning("cannot record plot, default db not set. Use defaultVTDB() in the first chunk to do this")
    } else if(!is.null(ggplot2:::last_plot())){
        
        len = length(histropts$history$exprs)
        record(ggplot2:::last_plot(), symorpos = len)
    }
    paste0("![", x, "]")

}
