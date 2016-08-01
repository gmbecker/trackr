
#' @title Convenience constructors for specific VT backends
#' @rdname conv_constr
#' @param file character. The json file to use
#' @param opts ViztrackrOptions. The options for the DB
#' @param \dots{} ANY. Used to construct \code{opts}. Ignored if
#' \code{opts} is specified explicitly.
#' @return A ViztrackrDB object
#' @export
jsonVTDB = function(file, opts = ViztrackrOptions(...),  ...) {
    backend = VTJSONBackend(file)
    ViztrackrDB(opts = opts, backend = backend)
}

#'@rdname conv_constr
#' @param core character. The URI for the solr core to use
#' @param requestHandler character. Passed to \code{SolrList} constructor
#' @export
solrVTDB = function(core, requestHandler = "search", opts = ViztrackrOptions(...),
                    ...) {
    if(!require("rsolr"))
        stop("Can't create a solar-backed ViztrackrDB without rsolr installed")
    backend = SolrList(core, requestHandler = requestHandler)
    ViztrackrDB(opts = opts, backend = backend)
}
    


    
