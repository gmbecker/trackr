options(error=recover)
library(recordr)
library(ggplot2)

data(Chem97, package = "mlmRev")
pg <- ggplot(Chem97, aes(gcsescore)) + 
    geom_histogram(binwidth = .35) + 
    facet_wrap(~score) +
    ggtitle(expression(atop("Figure 2.5", atop("bindwidth .35")))) +
    theme_bw()

pg2 <- ggplot(Chem97, aes(gcsescore)) + 
    geom_histogram(binwidth = 1) + 
    facet_wrap(~score) +
    ggtitle(expression(atop("Figure 1.1", atop("bindwidth 1")))) +
    theme_bw()

pg3 <- ggplot(Chem97, aes(gcsescore)) + 
    geom_histogram(binwidth = .5) + 
    facet_wrap(~score) +
    ggtitle(expression(atop("Figure 1.2", atop("bindwidth .5")))) +
    theme_bw()

pg4 <- ggplot(Chem97, aes(gcsescore)) + 
    geom_histogram(binwidth = 2) + 
    facet_wrap(~score) +
    ggtitle(expression(atop("Figure 1.3", atop("bindwidth 2")))) +
    theme_bw()

library(rsolr)
blklight.img.dir = "~/gabe/railsapps/viztrackr/app/assets/images/"
solr.uri <- "http://localhost:8983/solr/recordr"
## ## non-standard requestHandler not strictly required, 
## ## but will maintain consistency with search results received via UI
sl <- SolrList(solr.uri, requestHandler="search")


#addToDataStore(pg, sl, img.save.dir = blklight.img.dir, verbose=TRUE)

## db =new("ViztrackrDB", backend = sl, opts = new("ViztrackrOptions", insert_delay = 1, img_dir = blklight.img.dir, img_ext = "png"))

dir =  "~/gabe/checkedout"
dir.create(file.path(dir, "vt_json_imgs"))
fil = file.path(dir,"vtdb_json_test.json")
unlink(fil)
db = jsonVTDB(fil, img_dir = file.path(dir, "vt_json_imgs"))


#db= addPlot(pg3, db, verbose = TRUE)
db= record(pg, db, verbose = TRUE)

res  = vtSearch("Figure 1.2", db = db, ret_type = "doclist")

x = rnorm(100)
y = rnorm(100, x, sd = 1/(abs(x) + .25))
df = data.frame(x =x, y = y, z = sample(c("hi", "lo"), 100, replace=TRUE), stringsAsFactors=TRUE)
