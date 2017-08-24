specialcases = list(character = "chr",
                 list = "lst")
## function to abbreviate class names for automatic bare-bones
## icon generation.
##
## handles 3 cases. Title case (ie standard S4 naming scheme),
##     split-by-period/underscore (data.frame), single word (integer)
##
## respects specialcases listed in specialcases object
## data.frame -> df
## integer -> int
## IRanges -> IR
## DataFrame -> DF
## tbl_df -> td

abrevClass = function(klass, spc = specialcases) {
    if(klass %in% names(spc))
        return(spc[[klass]])
    
    if(length(klass) > 1)
        klass = klass[1]
    hasupper = grepl("[[:upper:]]", klass)
    hassplit = grepl("[\\._]", klass)
    if(hasupper) {
        abrev = gsub("[^[:upper:]]", "", klass)
    } else if (hassplit) {
        absects = strsplit(klass, "(\\.|_)")[[1]]
        
        abrev = substr(absects, 1, 1)
        abrev = paste(abrev, collapse = "")
    } else {
        abrev = substr(klass, 1, 3)
        
    }
    if(nchar(abrev) > 3) {
        abchars = strsplit(abrev, "")[[1]]
        abrev = paste(abchars[c(1, length(abchars))], collapse = "")
        abrev = paste(strsplit(abrev, "")[[1]], collapse = "")
    }
    tolower(abrev)
}
