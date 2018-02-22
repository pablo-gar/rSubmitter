#' getJobNumber
#' 
#' Returns the number of existing SLURM jobs of a given user
#'
#' @param user Character - username, if blank it is gotten from sytem info
#' @return Integer - the current number of SLURM jobs in queue
#' @export
getJobNumber <- function(user = Sys.info()["user"]) {
    
    jobNumber <- systemSubmit(paste0("squeue -u ", user, " | wc -l"), wait = rSubmitterOpts$TIME_WAIT_FAILED_CMD, intern = TRUE, ignore.stderr = TRUE, ignore.stdout = FALSE)
    jobNumber <- as.integer(jobNumber) - 1
    return(jobNumber)
    
}
