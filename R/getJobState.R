getJobState <- function(x) {
    
    x <- as.character(x)
    if(length(x) > 1)
        x <- parseArg(x, ",")
    
    
    outSystem <- systemSubmit(paste0("sacct --noheader --parsable2 --format=JobID,State --job=", x), wait = rSubmitterOpts$TIME_WAIT_FAILED_CMD, ignore.stdout = F)
    if (length(outSystem) == 0) {
        jobInfo <- "NOTAVAIL"
    } else {
        outSystem <- strsplit(outSystem, "\\|")
        jobInfo <- sapply(outSystem, function(x) x[2])
        names(jobInfo) <- sapply(outSystem, function(x) x[1])
    }
    
    return(jobInfo)
}
