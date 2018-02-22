#' option enviroment
rSubmitterOpts <- new.env(parent = emptyenv())

#' .onLoad - rSubmitter
#' 
#' Series of steps needed to initialize rSubmitter
.onAttach <- function(libname, pkgname) {
    
    
    # Numeric options
    numericOptions <- c("MAX_JOBS_ALLOWED", "TIME_WAIT_MAX_JOBS", "TIME_WAIT_FAILED_CMD")
    editableOptions <- c(numericOptions) # Add more options here
    
    
    # Get options
    assign("USERNAME", Sys.info()["user"], envir = rSubmitterOpts)
    assign("MAX_JOBS_ALLOWED", NULL, envir = rSubmitterOpts)
    assign("TIME_WAIT_MAX_JOBS", 60, envir = rSubmitterOpts)
    assign("TIME_WAIT_FAILED_CMD", 5, envir = rSubmitterOpts)
    
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
                assign(key, value, envir = rSubmitterOpts)
            }
            cat("Options updated\n")
            print(profileTable)
        }
    }
        
}
