library(trackr)


library(ggplot2)


## examples from 
##      http://www.cookbook-r.com/Graphs
##      https://learnr.files.wordpress.com/2009/08/latbook.pdf
##      http://docs.ggplot2.org/current/index.html
##      http://lmdvr.r-forge.r-project.org/figures/figures.html

data(Chem97, package = "mlmRev")
## and a modified version of its ggplot2 counterpart, from
## https://learnr.files.wordpress.com/2009/08/latbook.pdf
pg <- ggplot(Chem97, aes(gcsescore)) + 
    geom_histogram(binwidth = 0.5) + 
    facet_wrap(~score) +
    ggtitle(expression(atop("ggplot2 Histogram of gcsescore", atop("facetted by score")))) +
    theme_bw()
pg

pgdnames = list(x = "gcsescore", group = list(panel = "score"))
pgdtypes = list(x = "numeric", group = list(panel = "numeric"))
pgdlabels = list(x = "gcsescore", y = "count", group = list(panel = "score"))

## this test needs to be re-enabled as soon as I put in the workaround for the ggplot2 bug
stopifnot(#identical(trackr:::dataNames(pg), pgdnames),
    identical(dim(fullData(pg)[[1]]), c(31022L, 2L)),
    identical(names(fullData(pg)[[1]]), c("gcsescore", "score")),
    identical(dataTypes(pg), pgdtypes),
    ## currently failing for ggplot2 2.3 due to extraneous "weight" label
    identical(dataLabels(pg), pgdlabels)
    )


set.seed(620)
dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
d <- ggplot(dsamp, aes(carat, price, colour = clarity)) +
  ggtitle("Diamond price by carat and clarity") + 
    geom_point() + theme_bw()
d

ddatanames = list(x = "carat", y = "price", group = list(color = "clarity"))

stopifnot(identical(trackr:::dataNames(d),ddatanames),
          identical(names(fullData(d)[[1]]), c("carat", "price", "clarity")),
          ## these will be the same because there's no extra stuff happening
          identical(dataLabels(d), ddatanames))
          
          
