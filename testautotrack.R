library(viztrackr)
x = rnorm(100)

imdir = file.path(tempdir(), "images")
dir.create(imdir)
vtdb = viztrackr:::ViztrackrDB(img_dir = imdir, backend = VTJSONBackend())
viztrackr:::autotrackPlots(vtdb)
y = rnorm(100, x)
library(ggplot2)
z = "unrelated"
storage = viztrackr:::storage
qplot(x,y)

storage$vtdb@backend$data[[1]]$code
