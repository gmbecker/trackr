

setMethod("prep_for_backend", c("FeatureSet", "TrackrDB"),
          function(object, target, opts,  verbose = FALSE)
{
    
    prep_for_backend(object, target = trackr_backend(target), opts = trackr_options(target),
                  verbose = verbose)
})



##' Make image files for a featureset
##' @param object A FeatureSet object
##' @param opts Options
##' @return A named list with two entries: preview.path, and
##'     image.path. These should be paths to (now) existing iamge
##'     fiels for thumbnail and main display, respectively
##' @docType methods
##' @export
##' @rdname make_image_files
setGeneric("make_image_files", function(object, opts) standardGeneric("make_image_files"))
##' @export
##' @rdname make_image_files
##' @aliases make_image_files,PlotFeatureSet-method
setMethod("make_image_files", "PlotFeatureSet",
          function(object, opts) {
    

    img.save.dir = img_dir(opts)
    img.ext = img_ext(opts)
   
    ## make sure the paths don't have a bunch of :s in them
    id =  gsub(":", "_", uniqueID(object))
    
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
    ## main full res image. We no longer embed the R object as metadata due to size
    ## issues when used in practice.
    mainpath = file.path(img.save.dir, paste0(id, ".", img.ext))
    saveBasicPlot(object, mainpath)
    
        
    list(preview.path = basename(thumbpath), image.path = basename(mainpath))
})

##' @export
##' @rdname make_image_files
##' @aliases make_image_files,RmdFeatureSet-method

setMethod("make_image_files", "RmdFeatureSet",
          function(object, opts) {
    img.save.dir = img_dir(opts)
    img.ext = img_ext(opts)
    
    
    if(!dir.exists(img.save.dir))
        dir.create(img.save.dir, recursive=TRUE)
    ret = NULL
    ## if(requireNamespace("RSelenium")) {
    ##     ret = tryCatch({suppressMessages(rs <- RSelenium::rsDriver(browser = "chrome", geckover=NULL, check=FALSE)) ##"phantomjs"))
    ##         on.exit(rs$server$stop(), add = TRUE)
            
    ##         cl = rs[["client"]]
    ##         cl$setWindowSize(480L, 640L)         
    ##         cl$navigate(paste0("file://", normalizePath(object@outfile)))
            
    ##         fil = file.path(img.save.dir, paste0(uniqueID(object), c("_thumb.",".", "_feed."), img.ext))
    ##         cl$screenshot(file = fil[1])
    ##         ## RSelenium doesn't appeart to allow you to control the size of screenshots, so we
    ##         ## will have to hope that blacklight can shrink the full size image itself.
    ##         cl$close()
    ##         file.copy(fil[1], fil[2], overwrite = TRUE)
    ##         file.copy(fil[1], fil[3], overwrite = TRUE)
            
    ##         list(preview.path = basename(fil[1]), image.path = basename(fil[2]))},
    ##         error = function(e) NULL)
    ## }
    if(is.null(ret))
        ##this will hit featurset, ie the default and always work
        ret = callNextMethod() 

    ret
})


draw_text_icon = function(object, kl = object@klass) {
    plot.new()
    plot.window(xlim = c(0,1), ylim = c(0,1))
    old = par(mar = c(0.1, 0.1, 0.1, 0.1))
    on.exit(par(old))
    
    text(abrevClass(kl), x = .45, y = .45, cex = 11,
         family = "sans", pos = 3, adj = c(.5, .5), offset = 0)
    text(kl[1], x = .45, y = .05, cex = 1.5,
         family = "sans", pos = 3, adj = c(.5, .5), offset = 0)
}

##' @export
##' @rdname make_image_files
##' @aliases make_image_files,ObjFeatureSet-method
setMethod("make_image_files", "ObjFeatureSet",
          function(object, opts) {
    img.save.dir = img_dir(opts)
    img.ext = img_ext(opts)
    
    
    if(!dir.exists(img.save.dir))
        dir.create(img.save.dir, recursive=TRUE)
    id = gsub(":", "_", uniqueID(object))
    paths = file.path(img.save.dir, paste0(id, c("_thumb.", ".","_feed."), img.ext))
    png(paths[1], width = 250, height = 250, units = "px")
    draw_text_icon(object = object)
    dev.off()
    
    file.copy(paths[1], to = paths[2])
    file.copy(paths[1], to = paths[3])
    list(preview.path = basename(paths[1]), image.path = basename(paths[2]))
})

##' @export
##' @rdname make_image_files
##' @aliases make_image_files,FeatureSet-method
setMethod("make_image_files", "FeatureSet",
          function(object, opts) {
    img.save.dir = img_dir(opts)
    img.ext = img_ext(opts)
    
    
    if(!dir.exists(img.save.dir))
        dir.create(img.save.dir, recursive=TRUE)
    id = gsub(":", "_", uniqueID(object))
    paths = file.path(img.save.dir, paste0(id, c("_thumb.", ".","_feed."), img.ext))

    file.copy(system.file("images", "Rlogo.png", package = "trackr"), paths[1])
    file.copy(system.file("images", "Rlogo.png", package = "trackr"), paths[2])
    file.copy(system.file("images", "Rlogo.png", package = "trackr"), paths[3])
    list(preview.path = basename(paths[1]), image.path = basename(paths[2]))
})

##' @export
##' @rdname make_image_files
##' @aliases make_image_files,ANY-method
setMethod("make_image_files", "ANY",
          function(object, opts) stop("make_image_files called on a non FeatureSet object. This shouldn't happen, please contact the package maintainer."))
    

## everything that isn't a TrackrDB. probably isn't safe long term, want
## more specific virtual class here?

setMethod("prep_for_backend", c("FeatureSet", "ANY"),
          function(object, target, opts = trackr_options(target), verbose = FALSE) {

    id = uniqueID(object)
    if(verbose) {
        message("Adding ", class(object)[1], " object with ID ", id, "...")
    }

    ## UTC required for Solr dates

    object@regdate <- .POSIXct(object@regdate, tz="UTC")
    
    ## make images. This is a generic, so handles all FeatureSet types and errors
    ## if a non-FeatureSet obj ever gets to it. 
    imgpathlist = make_image_files(object, opts)
    doc <- c(id = id, flatten5(as(object, "list")), imgpathlist)
    doc = doc
    if (verbose) {
        cat("ID ", id, "\n")
    }
    
    doc
})

savefile = function(object, opts) {

    dir = img_dir(opts)
    file.path(dir, paste0(idPath(uniqueID(object)), ".rds"))
}


setMethod("prep_for_backend", c("ObjFeatureSet", "ANY"),
          function(object, target, opts = trackr_options(target), verbose = FALSE) {
    doc = callNextMethod() ## FeatureSet

    if(!is.null(object@object) && !file.exists(savefile(object, opts))) {
        saveRDS(object@object, file = savefile(object, opts))
    }
    doc
    

})
          


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
    if(!file.exists(dirname(target$file))) {
        message("Creating directory for JSON file backend at ", dirname(target$file))
        dir.create(dirname(target$file))
    }

    cat(toJSON(as(docs(target), "list")), file = target$file)
    target
})




## endomorphism (on target arg, NOT first arg)
## each level delegates but ultimately returns the same class it was passed in target

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
        tmp = callGeneric()
        docs(ref) = as(tmp, "DocList")
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



setMethod("trackr_search", c("character", "DocCollectionRef"),
          function(pattern, target, opts, fields = NULL, ret_type = c("doclist", "id", "backend"),
                   verbose = TRUE) {
    trackr_search(pattern = pattern, target = target$docs, opts = opts, fields = fields, ret_type = ret_type)
})


.ignorefields = c("provtable")
## setMethod("trackr_search", c("character", "DocCollection"),
##           function(pattern, target, opts, fields = NULL, ret_type = c("doclist", "id", "backend"),
##                    verbose = TRUE) {
##     if(is.null(fields)) {
##         ## provtable has too much stuff in it, drives spurious hits
##         fields = setdiff(fieldNames(target), .ignorefields)
##     }
##     res = .grepl_helper(pattern, target[,fields[1]])
##     for(f in fields[-1]) 
##         res = res | .grepl_helper(pattern, target[,f])
    
##     inds = which(res) # this drops NAs
##     ret = target[inds]
##     switch(ret_type,
##            id = ids(ret),
##            doclist = as(ret, "DocList"),
##            df = as(ret, "data.frame"),
##            backend = stop("No backend-specific return type implemented for this backend."))
## })


setMethod("trackr_search", c("character", "DocCollection"),
          function(pattern, target, opts, fields = NULL, ret_type = c("doclist", "id", "backend"),
                   verbose = TRUE) {
      if(is.null(fields)) {
        ## provtable has too much stuff in it, drives spurious hits
        fields = setdiff(fieldNames(target), .ignorefields)
    }
    
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
  
    ##res = .grepl_helper(pattern, target[,fields[1]])
    ## res = grepl(pattern, target[,fields[1]])
    ## for(f in fields[-1]) 
    ##     ##res = res | .grepl_helper(pattern, target[,f])
    ##     res = res | grepl(pattern, target[,f])

    if(is.null(fields))
        res = searchDocs(target, pattern)
    else {
        
        solrq = paste(paste(fields, pattern, sep = ":"), collapse = " OR ")
        res = searchDocs(target, solrq)
    }
    
    switch(ret_type,
           id = ids(res),
           doclist = res, 
           df = as(res, "data.frame"),
           backend = stop("No backend-specific return type implemented for this backend."))
})
    
