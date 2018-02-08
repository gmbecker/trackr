## modifys

.inRStudio = function() {
    requireNamespace("rstudioapi", quietly = TRUE) &&
        !is(tryCatch(rstudioapi::versionInfo(), error = function(x) x),
            "error")
    }

    
.analysisFileOrNA = function() {
    if(.inRStudio())
        tryCatch(rstudioapi::getSourceEditorContext()[["path"]],
                 error = function(x) NA_character_)
    else
        NA_character_
}


.rstudioProjOrNA = function() {
    
    proj = NA_character_
    if(.inRStudio()) {
        proj = rstudioapi::getActiveProject()
        if(is.null(proj)) proj = NA_character_
    }
    proj
}
        
