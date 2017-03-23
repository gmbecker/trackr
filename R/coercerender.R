 ## out$regdate <- format(x@regdate, "%Y-%m-%d")
 ##    out$regtime = format(x@regdate, "%H:%M:%S")
##    out$regdatetime = format(x@regdate, "%Y-%m-%dT%H:%M:%SZ")

noflat = c("codeinfo", "object", "fullcodeinfo")
listRecToFeatureSet = function(lst) {
    clsdef = getClass(lst$fsetklass)
    slts = getSlots(clsdef)
    lst[sapply(lst, is.null)] = NA_character_
    lst$regdate = as.POSIXct(strptime(lst$regdatetime, "%Y-%m-%dT%H:%M:%SZ" ))
    toflat = setdiff(names(slts), c(names(lst), noflat))
    for(sl in toflat) {
        lst = unflattenField(lst, sl)

    }
    lst$codeinfo = getInputs(parseCode(lst$code))
    
    
    ret = do.call(new, c(Class = lst$fsetklass,
                         object = list(NULL),
                         lst[names(lst) %in% names(slts)]))
    objCode(ret) = paste(as.character(lst$code), collapse="\n")
    ret
    
}



norecurse = c("varnames", "varsummaries", "varclasses", "na", ## for na.rm
              "codeinfo", "fullcodeinfo", "outputids", "chunks" 
              )

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

