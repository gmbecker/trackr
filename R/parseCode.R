
#' @rdname parseCode
setMethod(f = "parseCode",
    signature = signature( code = "expression"),
    definition = function( code) {
        parseCode( as.character(code))
    }
)

#' @rdname parseCode
setMethod(f = "parseCode",
    signature = signature(code = "call"),
    definition = function(code) {
        parseCode(as.character(code))
    }
)

#' @rdname parseCode
setMethod(f = "parseCode",
    signature = signature(code = "character"),
    definition = function(code) {
        CodeDepends::readScript(txt = code)
    }
)

## #' @rdname parseCode
## setMethod(f = "parseCode",
##     signature = signature(object = "ANY", code = "NULL"),
##     definition = function(object, code) {
##         parseCode(object)
##     }
## )

#' @rdname parseCode
setMethod(f = "parseCode",
    signature = signature(code = "ggplot"),
    definition = function(code) {
        new("Script", vector("list", 0), location = as.character(NA))
    }
)

#' @rdname parseCode
setMethod(f = "parseCode",
    signature = signature(code = "trellis"),
    definition = function(code) {
        if (!is.null(getCall(code))) {
            if (is.symbol(getCall(code)$panel)) {
                # save the panel function and its name
                parseCode(c(
                    parse(text=c(
                        paste0(getCall(code)$panel, " <- "), 
                        deparse(code$panel)
                    )),
                    as.expression(getCall(code))
                ))
            } else {
                parseCode(as.expression(getCall(code)))
            }
        } else {
            new("Script", vector("list", 0), location = as.character(NA))
        }
    }
)

## #' @rdname parseCode
## setMethod(f = "parseCode",
##     signature = signature(object = "gTree", code = "missing"),
##     definition = function(object, code) {
##         new("Script", vector("list", 0), location = as.character(NA))
##     }
## )

## #' @rdname parseCode
## setMethod(f = "parseCode",
##     signature = signature(object = "recordedplot", code = "missing"),
##     definition = function(object, code) {
##         new("Script", vector("list", 0), location = as.character(NA))
##     }
## )

#' @rdname parseCode
setMethod(f = "parseCode",
          signature = signature(code = "ANY"),
          definition = function(code) {
    new("Script", vector("list", 0), location = as.character(NA))
}
)
    
