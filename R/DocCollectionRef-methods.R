## copied from rsolr
ROWNAMES <- function (x) if (length(dim(x)) != 0L) rownames(x) else names(x)

setMethod("ids", "DocCollectionRef", function(x) ROWNAMES(docs(x)))
setGeneric("ids<-", function(x, value) standardGeneric("ids<-"))
setReplaceMethod("ids", "DocCollectionRef", function(x, value) {
  ids(docs(x)) <- value
  x
})


setReplaceMethod("names", "DocCollectionRef", function(x, value) {
    names(docs(x)) <- value
    x
})


setMethod("fieldNames", "DocCollectionRef", function(x) fieldNames(docs(x)))
setGeneric("fieldNames<-", function(x, value) standardGeneric("fieldNames<-"))
setReplaceMethod("fieldNames", "DocCollectionRef", function(x, value) {
  fieldNames(docs(x)) <- value
  x
})

setMethod("ndoc", "DocCollectionRef", function(x) ndoc(docs(x)))

setMethod("meta", "DocCollectionRef", function(x) meta(docs(x)))

setGeneric("meta<-", function(x, value) standardGeneric("meta<-"))
## XXX Is this ambiguous with the above?
setReplaceMethod("meta", c("DocCollectionRef", "ANY"),
                 function(x, value) {
    meta(docs(x)) <- value
    x
    })
setMethod("docs", "DocCollectionRef", function(x) x$docs)
setGeneric("docs<-", function(x, ..., value) standardGeneric("docs<-"))
setReplaceMethod("docs", "DocCollectionRef", function(x, ..., value) {
    x$docs = value
    x})

