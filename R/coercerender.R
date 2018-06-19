 ## out$regdate <- format(x@regdate, "%Y-%m-%d")
 ##    out$regtime = format(x@regdate, "%H:%M:%S")
##    out$regdatetime = format(x@regdate, "%Y-%m-%dT%H:%M:%SZ")

noflat = c("codeinfo", "object", "fullcodeinfo")
listRecToFeatureSet = function(lst) {
    clsdef = getClass(lst$fsetklass)
    slts = getSlots(clsdef)
    sltpres = names(slts) [ names(slts) %in% names(lst)]
    ## "missing" values go to NA_character
    lst[sapply(lst, is.null)] = NA_character_
    ## slots go in as characters unless they are "AsIs in which case
    ## the values don't go in at all
    
    lst[sltpres] = lapply(sltpres, function(x) {if(is(lst[[x]], "AsIs")) character() else lst[[x]]})
    ## fix up time issues
    if(!is.null(lst$regdatetime) && is(lst$regdatetime, "POSIXct"))
        lst$regdate = lst$regdatetime
    else if(!is.null(lst$regdatetime))
        lst$regdate = as.POSIXct(strptime(lst$regdatetime, "%Y-%m-%dT%H:%M:%SZ" ))
    else if(!is.null(lst$regdate) && is(lst$regdate, "character"))
        lst$regdate = as.POSIXct(strptime(lst$regdate, "%Y-%m-%dT%H:%M:%SZ" ))    
    toflat = setdiff(names(slts), c(names(lst), noflat))
    for(sl in toflat) {
        lst = unflattenField(lst, sl)

    }
    lst$codeinfo = getInputs(parseCode(lst$code))

    ## non-slot list elemenst will go into the extramdata slot which
    ## is handled specially in both directions
    nonslots = names(lst)[!names(lst) %in% names(slts)]
    ## grab then clear non-slot list elements and put them into lst$extramdata
    nonsltlist = lst[nonslots]
    lst[nonslots] = NULL
    lst$extramdata = nonsltlist
    lst$provtable = hydrateProvTable(lst$provtable)


    
    lst$object = NULL
    
    ## by this time, we've created the extramdata element so that gets
    ## correctly included here
    ret = do.call(new, c(Class = lst$fsetklass,
                         lst[names(lst) %in% names(slts)]))
    objCode(ret) = paste(as.character(lst$code), collapse="\n")
    ret
    
}


hydrateProvTable = function(json) {
    df = fromJSON(json)
    if(!is.data.frame(df))
        df = as.data.frame(df, stringsAsFactors = FALSE)
    

    prefix = gsub("^([^:]+):.*", "\\1", df$outputvarhash[1])
    df$outputvarhash = gsub("^[^:]+:", "", df$outputvarhash)
    ## this should work fine even for ""
    
    df$inputvarhash = gsub("^[^:]+:", "", df$inputvarhash)
    new("ProvStoreDF", hashprefix = prefix, provdata = df)
}
    
norecurse = c("varnames", "varsummaries", "varclasses", "na", ## for na.rm
              "codeinfo", "fullcodeinfo", "outputids", "chunks",
              "sessioninfo"
              )

## we "flattened" by splitting nesting levels by ".", so we undo that here
## we have some special cases we have to handle where recursion shouldn't
## happen
unflattenField = function(lst, sl, recursive=TRUE) {
    pattern = sprintf("^%s\\.", sl)
    elinds = grep(pattern, names(lst))
    if(!length(elinds))
        return(lst)

    els = lst[elinds]
    lst = lst[-elinds]
    names(els) = gsub(pattern, "", names(els))
    if(recursive && !(sl %in% norecurse) ) {
        subpat = "^([^\\.]+)\\..*"
        torecurse = grep(subpat, names(els), value=TRUE)
        sls = unique(gsub(subpat, "\\1", setdiff(torecurse, norecurse)))
        for(nm in sls) {
            els = unflattenField(els, nm)
        }

    }

    ## ugh, special casing. Terrible.
    if(sl == "na" && identical(names(els), "rm"))
        lst[["na.rm"]] = els[[1]]
    else
        lst[[sl]] = els
    lst
}

