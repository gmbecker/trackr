
#' @title Convenience constructors for specific VT backends
#' @rdname conv_constr
#' @param file character. The json file to use
#' @param opts ViztrackrOptions. The options for the DB
#' @param img_dir character. The directory plot images should be
#' saved into. Passed to default construction of \code{opts}. Defaults to
#' \code{<directory of file>/images}. ignored if \code{opts} is specified explicitly.
#' @param \dots{} ANY. Used to construct \code{opts}. Ignored if
#' \code{opts} is specified explicitly.
#' @return A ViztrackrDB object
#' @export
jsonVTDB = function(file = "~/.recordr/objdb.json",
                    opts = ViztrackrOptions(img_dir = img_dir, ...),
                    img_dir = file.path(dirname(file), "images"),
                    ...) {
    if(!file.exists(vt_img_dir(opts))) {
        message("Creating image directory at ", vt_img_dir(opts))
        dir.create(vt_img_dir(opts), recursive=TRUE)
    }
    if(!file.exists(dirname(file))) {
        message("Creating directory for JSON file backend at ", dirname(file))
        dir.create(dirname(file))
    }
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
    


    
