cancelJob <- function(x) {
    
    x <- as.character(x)
    if(length(x) > 1)
        x <- paste0(x, collapse = " ")
    
    systemSubmit(paste("scancel", x), wait = rSubmitterOpts$TIME_WAIT_FAILED_CMD, ignore.stdout = T)  
}
