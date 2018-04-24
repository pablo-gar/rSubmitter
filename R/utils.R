#' printTime
#'
#' Prints to console the current date and time followed by
#' the message(s) in x
#'
#' @param x String
#' @param carriageReturn Logical, if TRUE print a carriage return at the end of line
#' @examples
#' printTime(": is right now")
printTime <- function(..., carriageReturn = F) {
	
    x <- paste0(..., collapse = "")
	first = ""
	if(carriageReturn) first = "\r"
	
	flush.console()
	cat(first, as.character(Sys.time()), x)
}

#' isWriteable
#'
#' Returns TRUE if the path is writeable
#' @param x String - Path
isWriteable <- function(x){
    tryCatch({
        suppressWarnings(write.table(1, x))
    }, error=function(e) FALSE)
}

#' parseArg
#' 
#' Takes a string an creates a vector using sep as the separator
#' Prints to console the current date and time followed by
#' the message(s) in x
#'
#' @param x String - if a vector only first element will be considred
#' @param sep String - if vector only first element will be considered
#' @param trim Logical - if TRUE white spaces will be eliminated
#' @return Character vector - with elements as characters after splitting by sep
#' @examples
#' parseArg("A, B, C", sep = ",")
parseArg <- function(x, sep, trim = T) {
		
		
	if(!is.character(x))
		stop ("x has to be a string")
	if(!is.character(sep))
		stop ("x has to be a string")
	
	x <- x[1]
	sep <- sep[1]
	
	if(trim)
		x <- trim(x)
	
	return (unlist(strsplit(x,sep)))
	
}

#' trim
#' 
#' Trims white space from a character vector
#' @param x String or character vector
#' @return Returns x without any white spaces
#' @examples
#' trim("A B")
trim <- function(x) { 
	
	if(!is.character(x))
		stop("x has to be string or a character vector")

	return(gsub("\\s+", "", x))
	
}

#' grepTempFile
#'
#' Creates a new file based of x only with lines containing
#' the specified pattern(s)
#' WARNING: uses unix grep
#'   
#' @param x String - path to file
#' @param pattern Character vector - patterns to select in file
#' @examples
#' grepTempFile("path/to/file", c("patternA", "patternB"))
grepTempFile <- function(x, pattern, tempLocation = "."){
	        

	patternArg <- paste0("-e ", paste0(pattern, collapse = " -e "))
	outFile <- file.path(tempLocation, paste0(basename(x), ".temp", sample(1:1000, 1)))

	system(paste("grep", patternArg, x, ">", outFile))

	return(outFile)

}

#' getMaxJobArrayLength
#' Internal function for .onAttach that gets the maximum length for a Job array
#' from the SLURM config file
getMaxJobArrayLength <- function() {
    maxArrayLength <- systemSubmit("scontrol show config | grep MaxArraySize", ignore.stdout = F, ignore.stderr = T)
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

#' shellCmdExists 
#' Internal function for .onAttach Check if shell command exists
shellCmdExists <- function(cmd) {
    
    exitStatus <- system(paste("type", cmd), ignore.stdout = T, ignore.stderr = T)
    
    if(exitStatus == 0) {
        return (TRUE)
    } else {
        return(FALSE)
    }
}
