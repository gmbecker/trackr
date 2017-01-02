# setGeneric(".toHTML", function(obj, ...) standardGeneric(".toHTML"))
# setMethod(".toHTML", "RecordrArtifact", function(obj, ...) stop("not implemented yet"))
##' Create HTML representation of record
##' @import htmltools

##<a href="#test1Content" class="group-cbox1 inline" title="Lorem ipsum 1">
##   <img alt="" src="http://placehold.it/128x128/" class="shadow"></a>


setGeneric("thumbnailHTML", function(rec, imgurlfun, vtdb = defaultVTDB()) standardGeneric("thumbnailHTML"),
           signature = "rec")

## coerce list to FeatureSet then call generic again to get the right dispatch
setMethod("thumbnailHTML", "list",
          function(rec, imgurlfun, vtdb) {
    rec = listRecToFeatureSet(rec)
    thumbnailHTML(rec, imgurlfun)


})

setMethod("thumbnailHTML", "ObjFeatureSet", function(rec, imgurlfun, vtdb) {
    htmltools::tags$a(href=paste0("#", uniqueID(rec)),
                                  class = "group-cbox1 inline",
                      div(paste(rec@klass, "object")))
})

setMethod("thumbnailHTML", "DFFeatureSet", function(rec, imgurlfun,
                                                    vtdb = defaultVTDB()) {
    tags = htmltools::tags
    tags$a(href=paste0("#", uniqueID(rec)),
                      class = "group-cbox1 inline",
                      tags$p("Recorded data.frame"),
                      tags$table(
                          tags$tr(
                              tags$td("nobs"), tags$td(nobs(rec))),
                          tags$tr(
                              tags$td("first columns"),
                              tags$td(paste(head(varnamesrec), 5),
                                            collapse="\n"))
                          )
           )
})

setMethod("thumbnailHTML", "PlotFeatureSet",
          function(rec, imgurlfun, vtdb = defaultVTDB()) {
    pfimg = previewpath(rec, vtdb)
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



genericMetadata = function(rec) {
    div(
        p(sprintf("Created by %s at %s", fsuser(rec), fsregdatetime(rec))),
        div(p(sprintf("RStudio Project (if any): %s\nAnalysis Script (if known): %s", 
                      fsrstudioproject(rec),
                      fsanalysisfile(rec)))
            )
    )
}

setGeneric("detailHTML", function(rec, imgurlfun, vtdb = defaultVTDB()) standardGeneric("detailHTML"),
           signature = "rec")

setMethod("detailHTML", "list",
          function(rec, imgurlfun, vtdb) {
    rec = listRecToFeatureSet(rec)
    detailHTML(rec, imgurlfun = imgurlfun, vtdb = vtdb)

})

setMethod("detailHTML", "PlotFeatureSet",
          function(rec, imgurlfun, vtdb = defaultVTDB()) {

    imgfil = imagepath(rec, vtdb)
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

setMethod("detailHTML", "DFFeatureSet",
          function(rec, imgurlfun, vtdb = defaultVDTB()) {
    htags = htmltools::tags
    addAppendChildren(htmltools::htags$table,
                      c(list(htags$th(htags$td("Variable"), htags$td("summary"))),
                        mapply(function(nm, sm) {
                            htags$tr(htags$td(nm), htags$td(sm))
                        }, nm = varnames(rec), sm = varsumdputs(rec),
                        SIMPLIFY=FALSE))
                      )
    

})





codestr = '
$(document).ready(function(){

$(".group-cbox1").colorbox({rel:"group-cbox1", transition:"none", width:"80%", height:"50%"});

$(".inline").colorbox({inline:true, width:"736px"});
});
'  


.multiToHTML2 =  function(reclist, imgurlfun, vtdb = defaultVTDB()) {
    
    if(is.null(reclist) || length(reclist) < 1)
        div(class = "recordr_results")
    else {
        retdiv = div(class="recordr_results", 
                     tagAppendChildren(htmltools::tags$section(class="image-wrap cf clear",
                                                               id="image-wrap"),
                                       list = lapply(reclist,
                                               thumbnailHTML, 
                                               imgurlfun = imgurlfun,
                                               vtdb = vtdb)))
        tagAppendChildren(retdiv, list = c(lapply(reclist,
                                                  function(rec, iuf, db) {
                                      div(style="display:none",
                                          detailHTML(rec, imgurlfun = iuf,
                                                     vtdb = db
                                                     )
                                          )
                                  }, iuf = imgurlfun, db = vtdb),
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
                
## .innerToHTML2 = function(rec, imgurlfun) {
##   file.copy(rec[["image.path"]], file.path("./images", basename(rec[["image.path"]])))
##     div(style="display:none",
##     div(id = rec[["id"]],
##         class = "inner-overlay-box",
##         div(class = "media",
##             htmltools::tags$a(href="#", classs = "img",
##                    img(alt="", 
##                        src = imgurlfun(file = file.path("./images/",
##                                                         basename(rec[["image.path"]]))
##                                        ) ,
##                        class = "img-inner-window"
##                        )
##                    ),
##             p(sprintf("Created by %s at %s", rec[["user"]], rec[["regdatetime"]])),
##             div(p(sprintf("RStudio Project (if any): %s\nAnalysis Script (if known): %s", 
##                           rec[["rstudioinfo"]]["project"],
##                           rec[["rstudioinfo"]]["file"])))
##         )
##     ))
            
  
  
  
  
## }

## .multiToHTML = function(reclist, imgurlfun) {
##   if(is.null(reclist) || length(reclist) < 1)
##     div(class = "recordr_results")
##   else
##     tagAppendChildren(div(class = "recordr_results"), list = lapply(reclist, .innerToHTML, imgurlfun = imgurlfun))
## }


##' RStudio addin/Shiny app for artifact discovery
##'
##' This function initiates a shiny server which  allows users
##' to search a recordr database and view the results. It can
##' be used as a stand-alone Shiny application or from within the
##' RStudio IDE as an addin
##'
##' @param vtdb The database to search. Defaults to the current default database
##' @import minUI
##' @import shiny
##' @import htmltools
##' @export
recordrAddin = function(vtdb = defaultVTDB()) {
  if(!require(miniUI) || !require("shiny") || !require(htmltools)) 
    stop("The miniUI, htmltools, and shiny packages are required to run the addin/shiny app")
  
  ui <- miniPage(
               htmltools::tags$style(type = "text/css",
              HTML(paste(readLines(system.file("shinyweb/css/style.css", package =
              "recordr")), collapse = "\n"))),
              htmltools::tags$script(type="text/javascript",
                                     HTML(paste(readLines(system.file("shinyweb/js/index.js",
                                           package = "recordr")), collapse = "\n"))),
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
      print(normalizePath(".getw"))
        
      found = reactive({vtSearch(pattern = input$search_pattern, db = vtdb)})
      output$results = renderUI({.multiToHTML2(found(),
                                               imgurlfun = session$fileUrl,
                                               vtdb = vtdb)})
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
