

setMethod("prep_for_vtdb", c("ObjFeatureSet", "ViztrackrDB"),
          function(object, target, opts,  verbose = FALSE)
{
    
    prep_for_vtdb(object, target = vt_backend(target), opts = vt_options(target),
                  verbose = verbose)
})




## everything that isn't a ViztrackrDB. probably isn't safe long term, want
## more specific virtual class here?
setMethod("prep_for_vtdb", c("ObjFeatureSet", "ANY"),
          function(object, target, opts, verbose = FALSE) {

              id = uniqueID(object)
              if(verbose) {
                  message("Adding ", graphSys(object), " object with ID ", id, "...")
              }

              ## UTC required for Solr dates
              object@regdate <- .POSIXct(object@regdate, tz="UTC")
    doc <- c(id = id, flatten5(as(object, "list")))

              img.save.dir = vt_img_dir(opts)
              img.ext = vt_img_ext(opts)
              
              
              if(!is.null(img.save.dir)) {
                  invisible(saveBasicPlot(object, 
                                          file.path(img.save.dir, paste0(id, "_thumb.", img.ext)),
                                          width = 5, height = 5, dpi = 50))
                  invisible(saveBasicPlot(object, 
                                          file.path(img.save.dir, paste0(id, "_feed.", img.ext)),
                                          width = 5, height = 5, dpi = 100))
                                        # main image with R object embedded as metadata
                  saveEnrichedPlot(object, 
                                   file.path(img.save.dir, paste(id, img.ext, sep=".")))

                  doc$preview.path = paste0(id, "_thumb.", img.ext)
                  doc$image.path = paste(id, img.ext, sep=".")
              }
              
              if (verbose) {
                  cat("ID ", id, "\n")
              }

              doc
          })

## setMethod("prep_for_vtdb", c("PlotFeatureSet", "ViztrackrDB"),
##           function(object, target, opts, verbose = FALSE) {
##     res = callNextMethod()
##     ## we don't want the actual object in the database
##     ## it won't serialize properly to, e.g., JSON
##     res$object = NULL
##     res
## })


## Funnel to the character (id) method
setMethod("vt_lookup", c(object = "ANY", target = "ANY"),
          function(object, target, opts,  exist = FALSE)
          {
              id = uniqueID(object)
              vt_lookup(id, target = target, opts = opts, exist = exist)
          })


## This will work for any list-like backend object that has a [[ method provided
## which can accept names (character values)


.vt_lookup_bracket = function(object, target, opts, exist = FALSE)
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



setMethod("vt_lookup", c("character", "ViztrackrDB"),
         function(object, target, opts, exist) {
    vt_lookup(object, vt_backend(target), opts = vt_options(target), exist = exist)
})


setMethod("vt_lookup", c("character", "DocCollectionRef"),
         function(object, target, opts, exist) {
    vt_lookup(object, docs(target), opts = vt_options(target), exist = exist)
})


setMethod("vt_lookup", c("character", "ANY"), .vt_lookup_bracket )



## vt_write is endomorphic. It should returned a modified object of the same class
## it was passed. This means that the ViztrackrDB method has to construct a new
## ViztrackrDB after generated a modified version of the backend.

setMethod("vt_write", c("ViztrackrDB"),
          function(target, opts, verbose = FALSE)
              {
                  backend = vt_write(target = vt_backend(target),
                                     opts = vt_options(target),
                                     verbose = verbose)
                  ViztrackrDB(backend = backend, opts = vt_options(target))
              })


##' @import RJSONIO
## write default is a no-op
setMethod("vt_write", c("ANY"),
          function(target, opts, verbose) target)


setMethod("vt_write", c("VTJSONBackend"),
          function(target, opts, verbose) {
    cat(toJSON(as(docs(target), "list")), file = target$file)
    target
})



## setMethod("insert_plot", c("list", "character", "ViztrackrDB", "missing"),
##           function(doc, id, db, backend = vt_backend(db), verbose = FALSE)
##           {
##               force(backend)
##               insert_plot(doc, id, db, backend, verbose)
##           })




## endomorphism
## each level delegates but ultimately returns the same class it was passed

## don't like that I only have list, * methods here, but doc should be the first
## argument...

setMethod("insert_plot", c(target="ViztrackrDB"),
          function(object, id, target, opts, verbose = FALSE)
          {
              vtdb = target
              target = vt_backend(vtdb)
              opts = vt_options(vtdb)
              res = callGeneric()
              vt_backend(vtdb) = res
              vtdb
          })

setMethod("insert_plot", c(target ="DocCollectionRef"),
          function(object, id, target, opts, verbose = FALSE)
    {
        ref = target
        target = docs(ref)
        docs(ref) = callGeneric()
        ref
    })

setMethod("insert_plot", c(target = "DocCollection"),
          function(object, id, target, opts, verbose = FALSE)
    {
        target[[id]] = object
        target
    })

setMethod("insert_plot", c(target = "ANY"),
          function(object, id, target, opts, verbose = FALSE)
    {
        target[[id]] = object
        target
    })




## ## rendered unnecessary by "list", "characteR", "ViztrackrDB", "ANY" method

## setMethod("insert_plot", c("list", "character", "ViztrackrDB",  "SolrList"),
##           function(doc, id, db, backend = vt_backend(db), verbose = FALSE)
##           {
##               .insert_plot_bracket(doc = doc, id = id, db = db, backend = backend,
##                                    verbose = verbose)
##           })


## setMethod("insert_plot", c("list", "character", "ViztrackrDB",  "VTJSONBackend"),
##           function(doc, id, db, backend = vt_backend(db), verbose = FALSE)
##           {
##               .insert_plot_bracket(doc = doc, id = id, db = db, backend = backend,
##                                    verbose = verbose)
##           })


## .insert_plot_bracket = function(doc, id, db, backend, verbose) {
##     ##we don't need insert=TRUE for RSolrList, and the normal R list api
##     ## accept that arg at all
##     backend[[id]] = doc
##     vt_backend(db) = backend
##     db
    

## }


## We funnel all dispatch to through c("character", "ViztrackrDB") method
## which performs the existence check, then calls down
## to the respective c("character", <backend>) method.
## That is the *only* method specific backends should provide.


## remove_plot function is endomorphic, will return same class it was
## passed (via target)

setMethod("remove_plot", c("ANY", "ViztrackrDB"),
          function(object, target, opts, 
                   verbose = FALSE) {
    id = uniqueID(object)
    
    
    ## opts intentionally missing below
    remove_plot(object = id, target = ViztrackrDB, verbose = verbose)
})

setMethod("remove_plot", c("character", "ViztrackrDB"),
          function(object, target, opts, 
                   verbose = FALSE) {
    ## object is an ID (a la uniqueID) here.
    found = vt_lookup(object = object, target = target, opts = opts, 
                      exist=TRUE)
    if(!found) {
        warning("Id ", object, " not found in this ", class(target), " Viztrackr backend")
        return(target)
    }
    
    backend = remove_plot(object, target = vt_backend(target), opts = vt_options(target),
                          verbose = verbose)
    vt_backend(target) = backend
    target
})


.remove_plot_bracket2 = function( object, target, opts, verbose) {
                                        #explicitness
    id = object
    if(verbose) {
        message("Removing entry with id ", id)
    }
    
    img.save.dir = vt_img_dir(opts)
    img.ext = vt_img_ext(opts)
    
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

              





## setMethod("remove_plot", c("ANY", "ANY"),
##           function(object, target, opts, verbose = FALSE) 
##     remove_plot(uniqueID(object), target = target, opts = opts,
##                 verbose = verbose)
##     )

## the default method that actual does the work via the R [[ api.

setMethod("remove_plot", c("character", "DocCollectionRef"),
          function(object, target, opts, verbose) {
    tmp = target
    target = docs(target)
    res = callNextMethod()
    docs(tmp) = res
    tmp

    })
setMethod("remove_plot", c("character", "ANY"),
          .remove_plot_bracket2)


## I think everything remove_plot related below this point is now unneccesary
## with the simplified dispatch mechanism (and won't work as is with the
## new signature anyway). Leaving it commented out for a bit until
## I can do some testing.

## ## compute id and delegate to c("missing", "character", "missing")

## setMethod("remove_plot", c("ANY", "missing", "ViztrackrDB"),
##           function(object, id = uniqueID(object), db,  backend = vt_backend(db),
##                     verbose = FALSE) {
##               pfs = makeFeatureSet(object)
##               id = uniqueID(pfs)
##               remove_plot(id = id, db = db, verbose = verbose)
##           })


## ## id associated with non-missing object arg overrides what is passed to id
## ## object is turned into a PFS and id is recomputed using uniqueID
## ## method then delegates to c("missing", "character", "missing")
## ## for existence check and final dispatch to backend specific method

## setMethod("remove_plot", c("ANY", "ANY", "ViztrackrDB", "missing"),
##           function(object, id = uniqueID(object), db, backend = vt_backend(db),
##                    verbose = FALSE) {
##               pfs = makeFeatureSet(object)
##               id = uniqueID(object)
##               remove_plot( id = id, db = db, verbose = verbose)
##           })

## ## even though backend isn't missing here, we compute the id then
## ## force it to go through c("missing", "character", "ViztrackrDB", "missing"
## ## for existence check before backend-specific dispatch

## setMethod("remove_plot", c(object = "ANY", id = "missing", "ViztrackrDB",
##                            backend = "ANY"),
##           function(object, id, db, backend, verbose = FALSE) {
##               pfs = makeFeatureSet(object)
##               id = uniqueID(pfs)
##               remove_plot(id = id, db = db, verbose = verbose)
##           })

## ## setMethod("remove_plot", c("ANY", "character", "ANY"),
## ##           function(object, id, backend, db, verbose) {
## ##               pfs = makeFeatureSet(object)
## ##               id = uniqueID(pfs)
## ##               remove_plot(id = id, backend = backend, db = db, verbose = verbose)
## ##           })


## setMethod("remove_plot", c("missing", "character", "ViztrackrDB", "ANY"),
##           function(object, id, db, backend, verbose = FALSE) {
##     .remove_plot_bracket(id = id, db = db, backend = backend, verbose = verbose)
## })


## ## rendered unnecessary by "missing", "character", "ViztrackrDB", "ANY" above
## setMethod("remove_plot", c("missing", "character", "ViztrackrDB", "SolrList"),
##           function(object, id, db, backend, verbose = FALSE) {
##     .remove_plot_bracket(id = id, db = db, backend = backend, verbose = verbose)
## })

## setMethod("remove_plot", c("missing", "character", "ViztrackrDB", "VTJSONBackend"),
##           function(object, id, db, backend, verbose = FALSE) {
##     .remove_plot_bracket(id = id, db = db, backend = backend, verbose = verbose)
## })

## .remove_plot_bracket = function( id, db, backend, verbose) {
##               if(verbose) {
##                   message("Removing plot with id ", id)
##               }

##               img.save.dir = vt_img_dir(db)
##               img.ext = vt_img_ext(db)
              
##               if(!is.null(img.save.dir)) {
##                   possfils = file.path(img.save.dir, paste0(id, c("_thumb", "feed", ""), img.ext))
##                   fils = possfils[file.exists(possfils)]
##                   file.remove(normalizePath(fils))
##               }
##               backend[[id]] = NULL
##               vt_backend(db) = backend
##               db
## }
    
              




setMethod("vt_grep", c("character", "ViztrackrDB"),
          function(pattern, target, opts, fields = NULL, 
                   ret_type = c("doclist","id", "backend"), verbose=TRUE) {
    ret_type = match.arg(ret_type)
    vt_grep(pattern = pattern, target = vt_backend(target),
            opts = vt_options(target),
            fields = fields, verbose = verbose, ret_type = ret_type)
})

## setMethod("vt_grep", c("character", "ViztrackrDB", "SolrList"),
##           function(pattern, db, backend, fields = NULL, ret_type = c("id", "list", "backend"),
##                    verbose = TRUE) {
##     if(is.null(fields)) {
##         fields = "text"
##     }
  
##     logprom = grepl(pattern, backend[,fields[1] ])
##     for(f in fields[-1]) {
##         ## I really think this needs to be or ...
##         ##        logprom = logprom & grepl(pattern, backend$f)
##         logprom = logprom | grepl(pattern, backend$f)
##     }
        
##     ret = backend[logprom]
##     switch(ret_type,
##            id = ret$id,
##            list = as.list(ret),
##            backend = ret)
## })

setMethod("vt_grep", c("character", "DocCollectionRef"),
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


setMethod("vt_grep", c("character", "DocCollection"),
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


setMethod("vt_grep", c("character", "DocCollection"),
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

    

setMethod("vt_grep", c("character", "SolrList"),
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
    

## ##this is NOT going to be fast...
## setMethod("vt_grep", c("character", "ViztrackrDB", "VTJSONBackend"),
##           function(pattern, db, backend, fields = NULL, ret_type = c("id", "list", "backend"),
##                    verbose = TRUE) {
##     if(is.null(fields)) {
##         fields = TRUE ## grab all of them
##     }

##     inds = sapply(backend$data, function(x)  any(grepl(pattern, paste(x[fields]))))
##     switch(ret_type,
##            id = names(backend$data)[inds],
##            list = backend$data[inds],
##            backend = {warning("No backend specific return value for VTJSONBackend, returning a list"); backend$data[inds]})
##     })
