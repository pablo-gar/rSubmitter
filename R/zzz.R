#' option enviroment
rSubmitterOpts <- new.env(parent = emptyenv())

#' .onLoad - rSubmitter
#' 
#' Series of steps needed to initialize rSubmitter
.onAttach <- function(libname, pkgname) {
    
    
    # Numeric options
    numericOptions <- c("MAX_JOBS_ALLOWED", "TIME_WAIT_MAX_JOBS", "TIME_WAIT_FAILED_CMD", "TIME_WAIT_JOB_STATUS", "MAX_JOB_ARRAY_LENGTH")
    editableOptions <- c(numericOptions) # Add more options here
    
    
    # Get options
    assign("USERNAME", Sys.info()["user"], envir = rSubmitterOpts)
    assign("MAX_JOBS_ALLOWED", 1999, envir = rSubmitterOpts)
    assign("TIME_WAIT_MAX_JOBS", 60, envir = rSubmitterOpts)
    assign("TIME_WAIT_FAILED_CMD", 5, envir = rSubmitterOpts)
    assign("TIME_WAIT_JOB_STATUS", 5, envir = rSubmitterOpts)
    assign("MAX_JOB_ARRAY_LENGTH", getMaxJobArrayLength(), envir = rSubmitterOpts)
    
    # Look for profile file and overwrite options
    profileFile <- path.expand("~/.rSubmitter")
    if(file.exists(profileFile)) {
        cat("rSubmitter profile file found in:\n   ", profileFile, "\n\n")
        
        profileTable <- read.table(profileFile, sep = ":", stringsAsFactors = F, header = F)
        profileTable <- profileTable[profileTable[,1] %in% editableOptions, ]
        colnames(profileTable) <- c("Option", "Value")
        
        if(nrow(profileTable) == 0) {
            warning("No valid options found in profile file")
        } else {
            for (i in 1:nrow(profileTable)) {
                key <- profileTable[i, 1]
                value <- profileTable[i, 2]
                if(key %in% numericOptions)
                    value <- suppressWarnings(as.numeric(value))
                if(is.na(value))
                    stop("Invalid value for: ", key)
                
                if(key == "MAX_JOB_ARRAY_LENGTH" & value > rSubmitterOpts$MAX_JOB_ARRAY_LENGTH){
                    cat("\nWARNING!\n", 
                        "   MAX_JOB_ARRAY_LENGTH specified in ~/.rSubmitter is greater than maximum\n", 
                        "allowed by your SLURM system(", rSubmitterOpts$MAX_JOB_ARRAY_LENGTH, 
                        "), this can bring problems for JobArray objects\n", 
                        "   Strongly consider changing MAX_JOB_ARRAY_LENGTH to ", rSubmitterOpts$MAX_JOB_ARRAY_LENGTH, "\n\n")
                }
                assign(key, value, envir = rSubmitterOpts)
            }
            cat("Options updated\n")
            print(profileTable)
        }
    }
        
}

#' getMaxJobArrayLength
#' Internal function for .onAttach that get the maximum length for a Job array
#' from the SLURM config file
getMaxJobArrayLength <- function() {
    maxArrayLength <- system("scontrol show config | grep MaxArraySize", intern = T)
    maxArrayLength <- gsub("\\s*", "", maxArrayLength)
    maxArrayLength <- gsub("\\w+=(\\d+)", "\\1", maxArrayLength)
    if(nchar(maxArrayLength) > 0) {
        maxArrayLength <- as.numeric(maxArrayLength) - 1
    } else {
        cat("Warning: Could not retrieve MaxArraySize from SLRUM config. Setting MAX_JOB_ARRAY_LENGTH to 500, you can change this value in ~/.rSubmitter")
        maxArrayLength <- 500
    }
    
    return(maxArrayLength)
}
