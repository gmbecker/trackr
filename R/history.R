
#' @title Construct and activate a HistoryTracker object
#' @param id character. The id of the taskback
#' @return A HistoryTracker object
#' @rdname tracker-constructors
#' @export
history_tracker = function(id = "history_tracker") {
    h_tracker$new(id = id)
}

#' @rdname tracker-constructors
#' @aliases knitr_tracker
#' @export
knitr_tracker = function() {
    kh_tracker$new()
}

setMethod("show", "VirtHistoryTracker",
          function(object) {

    cat("A history tracker of class", class(object), "\nRecent expressions:",
        paste(tail(object$exprs, 5), collapse = "\n"),
        "\n(For full history do obj$exprs)")
})

