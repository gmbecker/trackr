

setMethod("prep_for_backend", c("ObjFeatureSet", "TrackrDB"),
          function(object, target, opts,  verbose = FALSE)
{
    
    prep_for_backend(object, target = trackr_backend(target), opts = trackr_options(target),
                  verbose = verbose)
})




## everything that isn't a TrackrDB. probably isn't safe long term, want
## more specific virtual class here?

setMethod("prep_for_backend", c("ObjFeatureSet", "ANY"),
          function(object, target, opts, verbose = FALSE) {

    id = uniqueID(object)
    if(verbose) {
        message("Adding ", graphSys(object), " object with ID ", id, "...")
    }
    
    ## UTC required for Solr dates
    object@regdate <- .POSIXct(object@regdate, tz="UTC")
    doc <- c(id = id, flatten5(as(object, "list")))
    
    img.save.dir = img_dir(opts)
    img.ext = img_ext(opts)
    
    
    if(!is.null(img.save.dir)) {
        if(!dir.exists(img.save.dir))
            dir.create(img.save.dir, recursive=TRUE)
        
        thumbpath= file.path(img.save.dir, paste0(id, "_thumb.", img.ext))
        invisible(saveBasicPlot(object, 
                                thumbpath,
                                width = 5, height = 5, dpi = 50))
        feedpath = file.path(img.save.dir, paste0(id, "_feed.", img.ext))
        invisible(saveBasicPlot(object, 
                                feedpath,
                                width = 5, height = 5, dpi = 100))
                                        # main image with R object embedded as metadata
        mainpath = file.path(img.save.dir, paste0(id, ".", img.ext))
        saveEnrichedPlot(object, mainpath)
        
        
        doc$preview.path = thumbpath
        doc$image.path = mainpath
    }
    
    if (verbose) {
        cat("ID ", id, "\n")
    }
    
    doc
})

## setMethod("prep_for_backend", c("PlotFeatureSet", "TrackrDB"),
##           function(object, target, opts, verbose = FALSE) {
##     res = callNextMethod()
##     ## we don't want the actual object in the database
##     ## it won't serialize properly to, e.g., JSON
##     res$object = NULL
##     res
## })


## Funnel to the character (id) method
setMethod("trackr_lookup", c(object = "ANY", target = "ANY"),
          function(object, target, opts,  exist = FALSE)
          {
              id = uniqueID(object)
              trackr_lookup(id, target = target, opts = opts, exist = exist)
          })


## This will work for any list-like backend object that has a [[ method provided
## which can accept names (character values)


.trackr_lookup_bracket = function(object, target, opts, exist = FALSE)
{
    ## for explicitness. This is an id by the time we get to
    ## the character method
    id = object
    res = tryCatch(target[[id]], error = function(e) NULL)
    
    if(exist)
        !is.null(res)
    else
        res
    
}



setMethod("trackr_lookup", c("character", "TrackrDB"),
         function(object, target, opts, exist) {
    trackr_lookup(object, trackr_backend(target), opts = trackr_options(target), exist = exist)
})


setMethod("trackr_lookup", c("character", "DocCollectionRef"),
         function(object, target, opts, exist) {
    trackr_lookup(object, docs(target), opts = trackr_options(target), exist = exist)
})


setMethod("trackr_lookup", c("character", "ANY"), .trackr_lookup_bracket )



## trackr_write is endomorphic. It should returned a modified object of the same class
## it was passed. This means that the TrackrDB method has to construct a new
## TrackrDB after generated a modified version of the backend.

setMethod("trackr_write", c("TrackrDB"),
          function(target, opts, verbose = FALSE)
              {
                  backend = trackr_write(target = trackr_backend(target),
                                     opts = trackr_options(target),
                                     verbose = verbose)
                  TrackrDB(backend = backend, opts = trackr_options(target))
              })


##' @import RJSONIO
## write default is a no-op
setMethod("trackr_write", c("ANY"),
          function(target, opts, verbose) target)


setMethod("trackr_write", c("JSONBackend"),
          function(target, opts, verbose) {
    cat(toJSON(as(docs(target), "list")), file = target$file)
    target
})




## endomorphism
## each level delegates but ultimately returns the same class it was passed

## don't like that I only have list, * methods here, but doc should be the first
## argument...

setMethod("insert_record", c(target="TrackrDB"),
          function(object, id, target, opts, verbose = FALSE)
          {
              tdb = target
              target = trackr_backend(tdb)
              opts = trackr_options(tdb)
              res = callGeneric()
              trackr_backend(tdb) = res
              tdb
          })

setMethod("insert_record", c(target ="DocCollectionRef"),
          function(object, id, target, opts, verbose = FALSE)
    {
        ref = target
        target = docs(ref)
        docs(ref) = callGeneric()
        ref
    })

setMethod("insert_record", c(target = "DocCollection"),
          function(object, id, target, opts, verbose = FALSE)
    {
        target[[id]] = object
        target
    })

setMethod("insert_record", c(target = "ANY"),
          function(object, id, target, opts, verbose = FALSE)
    {
        target[[id]] = object
        target
    })





## We funnel all dispatch to through c("character", "TrackrDB") method
## which performs the existence check, then calls down
## to the respective c("character", <backend>) method.
## That is the *only* method specific backends should provide.


## remove_record function is endomorphic, will return same class it was
## passed (via target)

setMethod("remove_record", c("ANY", "TrackrDB"),
          function(object, target, opts, 
                   verbose = FALSE) {
    id = uniqueID(object)
    
    
    ## opts intentionally missing below
    remove_record(object = id, target = TrackrDB, verbose = verbose)
})

setMethod("remove_record", c("character", "TrackrDB"),
          function(object, target, opts, 
                   verbose = FALSE) {
    ## object is an ID (a la uniqueID) here.
    found = trackr_lookup(object = object, target = target, opts = opts, 
                      exist=TRUE)
    if(!found) {
        warning("Id ", object, " not found in this ", class(target), " Trackr backend")
        return(target)
    }
    
    backend = remove_record(object, target = trackr_backend(target), opts = trackr_options(target),
                          verbose = verbose)
    trackr_backend(target) = backend
    target
})


.remove_record_bracket2 = function( object, target, opts, verbose) {
                                        #explicitness
    id = object
    if(verbose) {
        message("Removing entry with id ", id)
    }
    
    img.save.dir = img_dir(opts)
    img.ext = img_ext(opts)
    
    ##this is clunky and unnecessary in the case of non-plots, but should be
    ## safe due to the the file.exists check. Leaving as is to avoid refactor
    ## for now.
    if(!is.null(img.save.dir)) {
        possfils = file.path(img.save.dir, paste0(id, c("_thumb", "feed", ""), img.ext))
        fils = possfils[file.exists(possfils)]
        if(length(fils))
            file.remove(normalizePath(fils))
    }
    target[[id]] = NULL
    target
}

              





## setMethod("remove_record", c("ANY", "ANY"),
##           function(object, target, opts, verbose = FALSE) 
##     remove_record(uniqueID(object), target = target, opts = opts,
##                 verbose = verbose)
##     )

## the default method that actual does the work via the R [[ api.

setMethod("remove_record", c("character", "DocCollectionRef"),
          function(object, target, opts, verbose) {
    tmp = target
    target = docs(target)
    res = callNextMethod()
    docs(tmp) = res
    tmp

    })
setMethod("remove_record", c("character", "ANY"),
          .remove_record_bracket2)


setMethod("trackr_search", c("character", "TrackrDB"),
          function(pattern, target, opts, fields = NULL, 
                   ret_type = c("doclist","id", "backend"), verbose=TRUE) {
    ret_type = match.arg(ret_type)
    trackr_search(pattern = pattern, target = trackr_backend(target),
            opts = trackr_options(target),
            fields = fields, verbose = verbose, ret_type = ret_type)
})


setMethod("trackr_search", c("character", "DocCollectionRef"),
          function(pattern, target, opts, fields = NULL, ret_type = c("doclist", "id", "backend"),
                   verbose = TRUE) {
    target = docs(target)
    callGeneric()
})

.grepl_helper = function(pattern, x) {
    if(length(x) == 0)
        return(FALSE)
    if(is(x, "list")) {
        lens  = lengths(x)
        if(any(lens == 0)) {
            x[lens == 0] = ""
            lens = lengths(x)
        }
        dat = unlist(x)
        logs = grepl(pattern, dat)
        fac = rep(seq_len(length(x)), times = lens)
        ret = tapply(logs, fac, any)
    } else if (is(x, "matrix")) {
        x = t(x)
        dm = dim(x)
        logs = grepl(pattern, x)
        dim(logs)  = dm
        ret = rowSums(logs) > 0
    } else {
        ret = grepl(pattern, x)
    }
    if(length(ret) == 0)
        ret = FALSE
    ret

}


setMethod("trackr_search", c("character", "DocCollection"),
          function(pattern, target, opts, fields = NULL, ret_type = c("doclist", "id", "backend"),
                   verbose = TRUE) {
    if(is.null(fields))
        fields = fieldNames(target)
    
    res = .grepl_helper(pattern, target[,fields[1]])
    for(f in fields[-1]) 
        res = res | .grepl_helper(pattern, target[,f])
    
    inds = which(res) # this drops NAs
    ret = target[inds]
    switch(ret_type,
           id = ids(ret),
           doclist = as(ret, "DocList"),
           df = as(ret, "data.frame"),
           backend = stop("No backend-specific return type implemented for this backend."))
})


setMethod("trackr_search", c("character", "DocCollection"),
          function(pattern, target, opts, fields = NULL, ret_type = c("doclist", "id", "backend"),
                   verbose = TRUE) {
    if(is.null(fields))
        fields = fieldNames(target)
    
    res = .grepl_helper(pattern, target[,fields[1]])
    for(f in fields[-1]) 
        res = res | .grepl_helper(pattern, target[,f])
    
    inds = which(res) # this drops NAs
    ret = target[inds]
    switch(ret_type,
           id = ids(ret),
           doclist = as(ret, "DocList"),
           df = as(ret, "data.frame"),
           backend = stop("No backend-specific return type implemented for this backend."))
})

    

setMethod("trackr_search", c("character", "SolrList"),
          function(pattern, target, opts, fields = NULL, ret_type = c("doclist", "id", "backend"),
                   verbose = TRUE) {
##    if(is.null(fields))
##        fields = fieldNames(target)
    if(is.null(fields))
        fields = "text"
    ##res = .grepl_helper(pattern, target[,fields[1]])
    res = grepl(pattern, target[,fields[1]])
    for(f in fields[-1]) 
        ##res = res | .grepl_helper(pattern, target[,f])
        res = res | grepl(pattern, target[,f])
    
    inds = which(res) # this drops NAs
    ret = target[inds]
    switch(ret_type,
           id = ids(ret),
           doclist = ret, 
           df = as(ret, "data.frame"),
           backend = stop("No backend-specific return type implemented for this backend."))
})
    
