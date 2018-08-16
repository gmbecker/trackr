library(trackr)
library(knitr)
be = JSONBackend(tempfile())
tdb = TrackrDB(backend=be, img_dir = tempdir())
defaultTDB(tdb)

sysfil = system.file("test_docs/knitr_test.Rmd", package = "trackr")
rmdfil = file.path(tempdir(), "knitr_test.Rmd")
file.copy(sysfil, rmdfil)
if(rmarkdown::pandoc_available()) {
    knit_and_record(rmdfil, verbose = TRUE)
    stopifnot(length(findRecords("mtcars")) == 2) ## recvars is 'fit' so that plus report
} else {
    message("Pandoc not available. Skipping knit_and_record test")
}

f = function(paths) readLines(paths)
fil = system.file("test_docs", "knitr_test.Rmd", package = "trackr")
recordFiles(fil, ingestfun = f, verbose = TRUE)

res= findRecords("test_docs")
stopifnot(length(res) == 2, # it doesn't find the "fit" record because not directly associated iwth test_docs
          length(findRecords("*")) == 4)
