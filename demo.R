library(recordr)
library(ggplot2)
library(rsolr)

getCode = function(solr) as.list(solr)[[1]]$code

blklight.img.dir = "~/gabe/railsapps/viztrackr/app/assets/images/"
solr.uri <- "http://localhost:8983/solr/recordr"
sl <- SolrList(solr.uri, requestHandler="search")
db =new("ViztrackrDB", backend = sl,
        opts = new("ViztrackrOptions",
                   insert_delay = 1,
                   img_dir = blklight.img.dir,
                   img_ext = "png"))





carplot = qplot(wt, mpg, data = mtcars,
                main = "Super Amazing Plot!!! dry run 2")
carplot
record(carplot, db)














res = vtSearch("af56a166", fields = c("id", "text"),
               db= db)
code = getCode(res)
eval(parse(text=code))
gg1
