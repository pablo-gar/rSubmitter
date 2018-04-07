getJobState <- function(x, dateJob = NULL) {
    
    x <- as.character(x)
    if(length(x) > 1)
        x <- paste0(x, collapse = ",")
    
    if(is.null(dateJob)) {
        sacct <- "sacct -S `date -d '-10 days' +'%Y-%m-%d'` --noheader --parsable2 --format=JobID,JobName,State --job="
    } else {
        sacct <- paste0("sacct -S ", dateJob, " --noheader --parsable2 --format=JobID,JobName,State --job=")
    }
    
    jobInfo <- systemSubmit(paste0(sacct, x), wait = rSubmitterOpts$TIME_WAIT_FAILED_CMD, ignore.stdout = F)  
    
    if (length(jobInfo) == 0) {
        jobState = data.frame(jobId = "NOTAVAIL", jobName = "NOTAVAIL", jobState = "NOTAVAIL", stringsAsFactors = F)
    } else {
        
        jobInfo <- jobInfo[ !grepl("\\.batch|\\.extern|\\.[0-9]+\\|", jobInfo, perl = T) ]
        
        jobIds <- gsub("(.+)\\|.+\\|.+", "\\1", jobInfo) 
        jobNames <- gsub(".+\\|(.+)\\|.+", "\\1", jobInfo) 
        jobState <- gsub(".+\\|.+\\|(.+)", "\\1", jobInfo)
        jobState <- gsub("CANCELLED.*", "CANCELLED", jobState)
    
        jobState = data.frame(jobId = jobIds, jobName = jobNames, jobState = jobState, stringsAsFactors = F)
    
    }
    
    return(jobState)
}
