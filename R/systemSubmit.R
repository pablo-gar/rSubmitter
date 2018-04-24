#' systemSubmit
#' 
#' Internal function for the rSubmitter package
#' Tries executing a command up to n times while the execution returns a
#' non-zero exit status.
#' Useful when trying to automatically submmit a job and the scheduler has problems,
#' then this will try several times if submission fails for any reason
#'
#' @param command Character - system command to execute
#' @param n Integer - number of times to try executing command in case it returns a non-zero exit status
#' @param wait Integer - time in seconds to wait before trying executing the command again
#' @param ignore.stdout Logical - if TRUE it won't return the standard output of execution
#' @param ignore.stderr Logical - if TRUE it won't return the standard error of execution
#' @param intern Logical - see ?system
#' @param stopIfFailed Logical, if TRUE throws error after all tries have failed, else throws a warning
#' @param ... to be passed to system()
#' 
#' @return system return object - see ?system
systemSubmit <- function(command, n = 5, wait = 5, ignore.stdout = TRUE, ignore.stderr = F, intern = TRUE, stopIfFailed = TRUE, verbose = F, ...) {
    stopifnot(is.character(command), length(command) == 1)
    
    count = 0
    while(count < n) {
        
        commandResult <- suppressWarnings(system(command, intern = intern, ignore.stdout = ignore.stdout, ignore.stderr = ignore.stderr, ...))
        exitStatus <- attr(commandResult, "status")
        
        if(is.null(exitStatus))
            break
        if(exitStatus == 0)
            break
        
        count = count + 1
        if(verbose) {
            cat("\nSomething went wrong with the system execution, trying again in ", wait, " seconds\n")
            cat("    Error observed: ", exitStatus, "\n\n\n")
        }
        Sys.sleep(wait)
    }
    
    if(count == n) {
        errorString <- paste0("\n\nFailed to execute after ", n, " tries:\n", 
                              "   Command: ", command, "\n",
                              "   Last output: ", commandResult, "\n",
                              "   Last exit code: ", exitStatus, "\n"
                             )
        if(stopIfFailed) { 
            stop(errorString)
        } else {
            warning(errorString)
        }
    }
    
    return(commandResult)
    
}
