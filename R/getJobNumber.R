#' Gets the number of existing SLURM jobs for a given user
#'
#' @param user Character - username, if blank it is gotten from sytem info
#' @return Integer - the current number of SLURM jobs in queue
#' @export
getJobNumber <- function(user = Sys.info()["user"]) {
    
    currentJobs <- systemSubmit(paste0("squeue -u ", user), wait = rSubmitterOpts$TIME_WAIT_FAILED_CMD, intern = TRUE, ignore.stderr = TRUE, ignore.stdout = FALSE)
    jobNumber <- length(currentJobs)
    
    # See if there are any job arrays
    toExpand <- grepl("_\\[(\\d+)-\\d+\\]", currentJobs)
    if(any(toExpand)){
        toExpand <- currentJobs[toExpand]
        for(row in toExpand) {
            fromI <- as.numeric(gsub(".+_\\[(\\d+)-\\d+\\].+", "\\1", row))
            toI <-  as.numeric(gsub(".+_\\[\\d+-(\\d+)\\].+", "\\1", row))
            
            jobNumber <- jobNumber + toI - fromI
        }
    }
    
    jobNumber <- as.integer(jobNumber) - 1
    return(jobNumber)
    
}
