
#' @title Convenience constructors for specific trackr  backends
#' @description Convenience constructors for JSON and Solr-based trackr backends.
#' @rdname conv_constr
#' @param file character. The json file to use
#' @param opts TrackrOptions. The options for the DB
#' @param img_dir character. The directory plot images should be
#' saved into. Passed to default construction of \code{opts}. Defaults to
#' \code{<directory of file>/images}. ignored if \code{opts} is specified explicitly.
#' @param \dots ANY. Used to construct \code{opts}. Ignored if
#' \code{opts} is specified explicitly.
#' @return A TrackrDB object
#' @export
jsonTDB = function(file = "~/.trackr/objdb.json",
                    opts = TrackrOptions(img_dir = img_dir, ...),
                    img_dir = file.path(dirname(file), "images"),
                    ...) {
    if(!file.exists(img_dir(opts))) {
        message("Creating image directory at ", img_dir(opts))
        dir.create(img_dir(opts), recursive=TRUE)
    }
    if(!file.exists(dirname(file))) {
        message("Creating directory for JSON file backend at ", dirname(file))
        dir.create(dirname(file))
    }
    backend = JSONBackend(file)
    TrackrDB(opts = opts, backend = backend)
}

#'@rdname conv_constr
#' @param core character. The URI for the solr core to use
#' @param requestHandler character. Passed to \code{SolrList} constructor
#' @export
solrTDB = function(core, requestHandler = "search", opts = TrackrOptions(...),
                    ...) {
    backend = SolrList(core, requestHandler = requestHandler)
    TrackrDB(opts = opts, backend = backend)
}
    


    
