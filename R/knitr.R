recplothook = function(x, opts, ...) {
    if(is.null(defaultTDB())) {
        warning("cannot record plot, default db not set. Use defaultTDB() in the first chunk to do this")
    } else if(!is.null(ggplot2:::last_plot())){
        
        len = length(histry::histropts$history$exprs)
        record(ggplot2:::last_plot(), symorpos = len)
    }
    paste0("![", x, "]")

}
