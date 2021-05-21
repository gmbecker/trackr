library(trackr)
library(knitr)
be = JSONBackend(tempfile())
tdb = TrackrDB(backend=be, img_dir = tempdir())
defaultTDB(tdb)
recs = 0
sysfil = system.file("test_docs/knitr_test.Rmd", package = "trackr")
rmdfil = file.path(tempdir(), "knitr_test.Rmd")
file.copy(sysfil, rmdfil)
if(rmarkdown::pandoc_available()) {
    knit_and_record(rmdfil, verbose = TRUE)
    stopifnot(length(findRecords("mtcars")) == 2) ## recvars is 'fit' so that plus report
    recs = recs + 2
} else {
    message("Pandoc not available. Skipping knit_and_record test")
}

f = function(paths) readLines(paths)
fil = system.file("test_docs", "knitr_test.Rmd", package = "trackr")
recordFiles(fil, ingestfun = f, verbose = TRUE)
recs = recs + 2 ## file and read-in value
res= findRecords("test_docs")
stopifnot(length(res) == 1, # it doesn't find anything from the Rmd because the copy disassociates with original path
          length(findRecords("*")) == recs)

## for now just ensure that it works
if(require("switchr")) {
    library(switchr)
    drepos <- tryCatch(defaultRepos(), error = function(e) NA_character_)
    if(any(grepl("bioc", drepos, ignore.case = TRUE)) &&
       !anyNA(drepos)) { #on some systems bioc repos not available. this will fail.
        man = manifestFromRecord(res[[1]])
        stopifnot(all(!is.na(manifest_df(man)$type)))
    }
}
