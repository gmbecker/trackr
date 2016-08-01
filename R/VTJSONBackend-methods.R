
## #'@rdname VTJSONBackend-methods
## #' @title Low level, backend end specific methods for JSON backend
## #'
## #' The double bracket access and replace methods manipulate the
## #' \code{data} slot of the VTJSONBackend object.
## #' 
## #' @param x VTJSONBackend. The backend object
## #' @param i ANY. Subsetting index. Passed down to list `[[` method
## #' @param j ANY. ignored
## #' @param dots NA. Passed to `[[` methods (most ignored)
## #' @docType methods
## #' @export
## setMethod("[[", "VTJSONBackend", function(x, i, j, ...) {
##     docs([[i]]
## })

## #' @rdname VTJSONBackend-methods
## #' @param value ANY. New value for index replacement method
## #' @export
## setMethod("[[<-", "VTJSONBackend", function(x, i, j, ..., value) {
##     dat = x$data
##     dat[[i]] = value
##     x$data = dat
##     x
## })

## #' @rdname VTJSONBackend-methods
## #' @export

## setMethod("[", "VTJSONBackend", function(x, i, j, ...) {
##     x$data[i]
## })

## #' @rdname VTJSONBackend-methods
## #' @export
## setMethod("[<-", "VTJSONBackend", function(x, i, j, ..., value) {
##     dat = x$data
##     dat[i] = value
##     x$data = dat
##     x
## })
