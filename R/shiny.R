

setGeneric("thumbnailHTML", function(rec, imgurlfun, tdb = defaultTDB()) standardGeneric("thumbnailHTML"),
           signature = "rec")

## coerce list to FeatureSet then call generic again to get the right dispatch
setMethod("thumbnailHTML", "list",
          function(rec, imgurlfun, tdb) {
    rec = listRecToFeatureSet(rec)
    thumbnailHTML(rec, imgurlfun)


})

## setMethod("thumbnailHTML", "ObjFeatureSet", function(rec, imgurlfun, tdb) {
##     htmltools::tags$a(href=paste0("#", uniqueID(rec)),
##                                   class = "group-cbox1 inline",
##                       div(paste(rec@klass, "object")))
## })

setMethod("thumbnailHTML", "DFFeatureSet", function(rec, imgurlfun,
                                                    tdb = defaultTDB()) {
    tags = htmltools::tags
    tags$a(href=paste0("#", uniqueID(rec)),
                      class = "group-cbox1 inline",
                      tags$p("Recorded data.frame"),
                      tags$table(
                          tags$tr(
                              tags$td("nobs"), tags$td(nobs(rec))),
                          tags$tr(
                              tags$td("first columns"),
                              tags$td(paste(head(varnames(rec), 5),
                                            collapse="\n"))
                          )
                      )
           )
})

## setMethod("thumbnailHTML", "PlotFeatureSet",
setMethod("thumbnailHTML", "FeatureSet",
          
          function(rec, imgurlfun, tdb = defaultTDB()) {
    pfimg = previewpath(rec, tdb)
    if(!file.exists(pfimg))
        stop("not found")
    file.copy(pfimg, file.path("./images", basename(pfimg)))
    
    htmltools::tags$a(href = paste0("#", uniqueID(rec)),
                      class = "group-cbox1 inline",
                      img(src = imgurlfun(file = file.path("./images/",
                                                           basename(pfimg))
                                          ) ,
                          class = "shadow",
                          style="width:20vw;height:20vw;"
                          ))
})

## setMethod("thumbnailHTML", "RmdFeatureSet",
##           function(rec, imgurlfun, tdb = defaultTDB()) {
##     tags = htmltools::tags
##     tags$a(href = paste0("#", uniqueID(rec)),
##            class = "group-cbox1 inline",
##            tags$p("Recorded Rmd Report"),
##            tags$p(rmdfile(rec)),
##            tags$div(class = "thumbnail",
##                     tags$iframe()
## })


ifnotnull = function(x) if(length(x) >0) x else NA

genericMetadata = function(rec) {
    div(
        p(sprintf("Created by %s at %s", fsuser(rec), fsregdatetime(rec))),
        div(p(sprintf("RStudio Project (if any): %s\nAnalysis Script (if known): %s", 
                      ifnotnull(fsrstudioproject(rec)),
                      ifnotnull(fsanalysisfile(rec))))
            )
    )
}

setGeneric("detailHTML", function(rec, imgurlfun, tdb = defaultTDB()) standardGeneric("detailHTML"),
           signature = "rec")

setMethod("detailHTML", "list",
          function(rec, imgurlfun, tdb) {
    rec = listRecToFeatureSet(rec)
    detailHTML(rec, imgurlfun = imgurlfun, tdb = tdb)

})

## setMethod("detailHTML", "PlotFeatureSet",
setMethod("detailHTML", "FeatureSet",
          function(rec, imgurlfun, tdb = defaultTDB()) {

    imgfil = imagepath(rec, tdb)
    file.copy(imgfil, file.path("./images", basename(imgfil)))
    div(style="display:none",
        div(id = uniqueID(rec),
            class = "inner-overlay-box",
            div(class = "media",
                htmltools::tags$a(href="#", classs = "img",
                                  img(alt="", 
                                      src = imgurlfun(file = file.path("./images/",
                                                                       basename(imgfil))
                                                      ) ,
                                      class = "img-inner-window"
                                      )
                                  ),
                genericMetadata(rec))
            )
        )
  
})

## setMethod("detailHTML", "DFFeatureSet",
##           function(rec, imgurlfun, tdb = defaultVDTB()) {
##     htags = htmltools::tags
##     addAppendChildren(htmltools::htags$table,
##                       c(list(htags$th(htags$td("Variable"), htags$td("summary"))),
##                         mapply(function(nm, sm) {
##                             htags$tr(htags$td(nm), htags$td(sm))
##                         }, nm = varnames(rec), sm = varsumdputs(rec),
##                         SIMPLIFY=FALSE))
##                       )
    

## })





codestr = '
$(document).ready(function(){

$(".group-cbox1").colorbox({rel:"group-cbox1", transition:"none", width:"80%", height:"50%"});

$(".inline").colorbox({inline:true, width:"736px"});
});
'  


.multiToHTML2 =  function(reclist, imgurlfun, tdb = defaultTDB()) {
    
    if(is.null(reclist) || length(reclist) < 1)
        div(class = "trackr_results")
    else {
        nms = names(reclist)
        if(is.null(nms))
          nms = 1:length(reclist)
        retdiv = div(class="trackr_results", 
                     tagAppendChildren(htmltools::tags$section(class="image-wrap cf clear",
                                                               id="image-wrap"),
                                       list = lapply(nms, function(i, ...) thumbnailHTML(reclist[[i]], ...),
                                                     imgurlfun = imgurlfun,
                                                     tdb = tdb)))
        tagAppendChildren(retdiv, list = c(lapply(nms, function(i, iuf, db) {
                                                           rec = reclist[[i]]
                                                           div(style="display:none",
                                                               detailHTML(rec, imgurlfun = iuf,
                                                                          tdb = db)
                                                           )
        
                                  }, iuf = imgurlfun, db = tdb),
                                  list(htmltools::tags$script(HTML(codestr)))))
    }
}





## <section class="image-wrap cf clear" id="image-wrap">
##<a href="#test1Content" class="group-cbox1 inline" title="Lorem ipsum 1">



# 
# <div style='display:none'>
#   <div id="test1Content" class="inner-overlay-box">
#     <div class="media">
#       <a href="#" class="img">
#         <img alt="" src="http://placehold.it/384x256/" class="img-inner-window" /></a>
#           
#           <div class="hd"><h3>Image 1</h3></div>
#             <div class="bd">
#               <p class="p-inner-window">Consequat ea Investigationes in enim congue. Option velit volutpat quod blandit ex. Congue parum praesent aliquam nam clari. Qui praesent quam sollemnes id vulputate. In imperdiet diam at sequitur et. Minim delenit in dolor dolore typi.</p> </div>
#                 </div>
#                 </div>
#                 </div>
#                 


##' @title RStudio addin/Shiny app for artifact discovery
##'
##' @description This function initiates a shiny server which  allows users
##' to search a trackr database and view the results. It can
##' be used as a stand-alone Shiny application or from within the
##' RStudio IDE as an addin
##'
##' @name trackrAddin
##' @param tdb The database to search. Defaults to the current default database
##' @import miniUI
##' @import shiny
##' @import htmltools
##' @export
trackrAddin = function(tdb = defaultTDB()) {
  
  ui <- miniPage(
               htmltools::tags$style(type = "text/css",
              HTML(paste(readLines(system.file("shinyweb/css/style.css", package =
              "trackr")), collapse = "\n"))),
              htmltools::tags$script(type="text/javascript",
                                     HTML(paste(readLines(system.file("shinyweb/js/index.js",
                                           package = "trackr")), collapse = "\n"))),
    gadgetTitleBar("Discover plots"),
    miniContentPanel(
      textInput("search_pattern", value="mtcars", label = "search pattern"),
      textOutput("count"),
      htmlOutput("results")
    )
  )
    
    
  server <- function(input, output, session) {

      if(!file.exists("./images")) {
          
          res = dir.create("./images")
          if(res) {
              cleanup = TRUE
              dircl = normalizePath("./images")
          } else
              stop("Unable to create image directory. Running app from directory with no permissions?")
      } else {
          cleanup = FALSE
      }
      found = reactive({findRecords(pattern = input$search_pattern, db = tdb)})
      output$results = renderUI({.multiToHTML2(found(),
                                               imgurlfun = session$fileUrl,
                                               tdb = tdb)})
      output$count = renderText(sprintf("Found %s plots matching your search", ndoc(found())))
      
      ## When the Done button is clicked, return a value
      observeEvent(input$done, {
          returnValue <- found()
          if(cleanup)
              unlink(dircl, recursive=TRUE)
          stopApp(returnValue)
      })
  }
  
  runGadget(ui, server)
}
