#' supperApply, it mimics the functionality of lapply but implemented
#' in a way that each iteration of the apply is submmitted as an individual
#' job to a SLURM cluster. Hence emulating a parellel behaivor in apply
#'
#' Each job batch, err, out, and script files are stored in a temporary folder. Once
#' all jobs have been submmitted, the function waits for them to finish. Once the jobs
#' are done it compiles all the results into list and returns them, therefore fully
#' mimicking the apply behaivor.
#' Author: Pablo Garcia
#' 
#' @export
# TODO
# - Wrap JobArray arguents into a list for better addition of future arguments


superApply <- function(x, FUN, ..., tasks = 1, workingDir, packages = NULL, sources = NULL, extraBashLines = "", extraScriptLines = "", clean = F, partition = NULL, time = NULL, mem = NULL){

    # Main function that emulates apply but with parallel processing behaivor.
    # It first divides elements of x into buckets of length(x)/tasks, then
    # uses each bucket as individual elements where apply() will be used with FUN 
    # as individual SLURM submissions.
    # The submission process goes as follow for each bucket:
    #   - Saves each bucket data as and RData file with an associated id name
    #   - Creates an R script that will load the bucket RData and execute apply() and FUN. 
    #     It will save the results as separate result RData file
    #   - Creates a batch script that submmits the R script
    #   - Submmits the job using the same id
    # 
    # Oncer all Jobs have been submmitted and finished it will compile all the result RData files
    # from the inidividual jobs into a single list.
    # This is the list to be returned
    #
    # ARGS:
    # x - vector/list - FUN will be applied to the elements of this. If x is and integer of length one, FUN will be executed x times with pars "..."
    # FUN - function - function to be applied to each element of x. 
    # ... - further arguments of FUN
    # tasks - integer - number of individual parallel jobs to execute
    # workingDir - string - path to folder that will contain all the temporary files needed for submission, execution,
    #                       and compilation of inidivudal jobs
    # extraScriptLines - string - extra code to be added to all of the individual parallel jobs
    #                             IMPORTANT: if FUN requires any library they have to be included here (e.g. extraScriptLines = "library(reshape); library(GenomicRanges)"
    # time - string - time allocated to each individual job, format "hh:mm:ss"
    # qos - string - SLURM qos
    # mem - string - memory allocated to each individual job, e.g. "10G", "10000"
    #
    # Return - list - results of FUN applied to each element in x
    
    SAP_PREFIX <- "sAp_"
    
    workingDir <- path.expand(workingDir)
    FUN <- match.fun(FUN)
    
    # Organizing JobArray parameters
    JobArrayPars <- list(outDir = workingDir, partition = partition, time = time, mem = mem)
    
    # Getting indeces to partition X into different tasks (i.e. individual jobs)
    partitionIndeces<- getPartitionIndeces(x, tasks = tasks)
    
    # Constructiong paralleleJobs
    printTime("Constructing parallel Jobs\n")
    jobArray <- getJobArray(x, FUN, ..., partitionIndeces = partitionIndeces, idPrefix = SAP_PREFIX, workingDir = workingDir, extraScriptLines = extraScriptLines, extraBashLines = extraBashLines, JobArrayPars = JobArrayPars, packages = packages, sources = sources)
    
    # Submmiting and waitng for jobs
    printTime("Submmiting parallel Jobs\n")
    jobArray$submit()
    jobArray$wait(stopIfFailed = T)
    
    # Merging output jobs
    printTime("Merging parellel results\n")
    
    jobNames <- jobArray$getJobNames()
    expectedOutFiles <- paste0(jobNames, ".outRData") 
    expectedOutVariables <- paste0("output_", jobNames)
    
    supperApplyResults <- mergeListDir (expectedOutFiles, expectedOutVariables, workingDir)
    printTime("Merge done\n")
    
    # Removing jobs files if desired
    if(clean)
        system(paste0("rm ", file.path(workingDir, "*")))   
    
    return(supperApplyResults)
        
}



getPartitionIndeces <- function(x, tasks = tasks) {
    
    # Helper of superApply
    # Creates a list  with slots, containing the start and end indeces 
    # corresponding to the partitions of x required to run the number of parallel tasks
    #
    # Parsing x, is it vector, list? or is it number of repetitions (i.e. x is just a number)?
    # This just to calculate the number of times the FUN has to be executed
    if(!is.vector(x)){
        x <- as.list(x)
        times <- length(x)
    }else{
        if(length(x) == 1 & is.numeric(x)){ # This will make apply ignore x and will execute FUN x times with ... arguments.
            times <- x          # It requires a FUN that does nothing with its first argument
            ignoreX <- TRUE
        }else{
            times <- length(x)
        }
    }
    
    # Creates indexes to partition data for parallel processing
    jobsPerTask <- ceiling(times/tasks)
    iStart <- seq(1, times, jobsPerTask)
    iEnd <- seq (jobsPerTask, times, jobsPerTask) 
    if(iEnd[length(iEnd)] < times) 
        iEnd <- c(iEnd,times)
    
    # Returns partition indices
    result <- list(iStart = iStart, iEnd = iEnd)
    return(result)

}   

getJobArray<- function(x, FUN, ..., idPrefix, partitionIndeces, workingDir, extraScriptLines, extraBashLines, JobArrayPars, packages, sources) {
    
    # Helper of superApply
    # Submits multiple jobs from the partions of x created in get Partition Indeces
    #
    # x - list/vector - data to be partition
    # FUN - function - function to be applied to each element of x
    # partitionIndeces - list - output of getPartitionIndeces()
    #
    # RETURNS a data.frame of two columns, jobName and jobId
    
    
    # Cleaning and or creating workind dir for submission
    dir.create(workingDir, showWarnings = F, recursive = T)
    
    # Making unique ids for each submission
    idPrefix <- paste0(c(idPrefix, sample(letters, size=3), sample(0:9,size=1)), collapse = "")
    system(paste0("rm ", file.path(workingDir, paste0(idPrefix, "*"))), ignore.stdout = T, ignore.stderr = T)
    
    iStart <- partitionIndeces$iStart
    iEnd <- partitionIndeces$iEnd
    
    # Creating individual scripts for each submission
    jobScripts <- createJobScriptsData(x, FUN = FUN, ..., idPrefix = idPrefix, iStart = iStart, iEnd = iEnd, workingDir = workingDir, extraScriptLines = extraScriptLines, extraBashLines = extraBashLines, packages = packages, sources = sources)
    
    JobArrayPars <- c(list(commandList = jobScripts, jobName = idPrefix), JobArrayPars)
    jobArray <- do.call(JobArray$new, JobArrayPars)
    return(jobArray)
}

#' creatJobScriptsData
#' Helper of superApply
#' Takes a vector/list x, a function FUN and extra paramaters (...) and submits an Rscript
#' that executes lappy in x using FUN, saves the scripts, results and slurm fil;e in workingDir  
#' 
#' x - vector/list - data to which lapply will be executed
#' FUN - function - function to be applied to x
#' ... - extra paramaters passed to FUN
#' extraScriptLines - string - lines to be added at the beginning of the Rscript before lapply (useful to load packages)
#' partition - string - partition in SLURM for job submission
#' time - string - estimated time for lapply to finish, format "hh:mm:ss"
#' qos - string - SLURM qos
#' mem - string - estimated memory requiered by lapply execution, format e.g "10G"

createJobScriptsData <- function(x, FUN, ..., idPrefix, iStart, iEnd, workingDir, extraScriptLines = "", extraBashLines = "", packages = NULL, sources = NULL) {
    
    cmds <- list()
    
    FUN <- match.fun(FUN)
    
    # Checking if I need to load current packages or if user-defined packages can be loaded
    if(is.null(packages)) {
        packages <- createStringFunction ("library", getUserPackages())
    } else {
        packages <- createStringFunction ("library", packages)
        eval(parse(text = paste(packages, collapse = ";")))
    }
    
    # Checking if I can source user-defined paths
    if(!is.null(sources)) {
        tempEnv <- new.env()
        sources <- paste0('"', sources, '"')
        sourcesLocal <- createStringFunction ("source", paste(sources, ", chdir = T, local = tempEnv"))
        sources <- createStringFunction ("source", paste(sources, ", chdir = T"))
        eval(parse(text = paste(sourcesLocal, collapse = ";")))
        rm(tempEnv)
    }
    
    for(i in 1:length(iStart)) {
        
        id <- paste0(idPrefix, "_", i)
        
        xCurrent <- x[iStart[i]:iEnd[i]]
        flush.console()
        # Setting file and var names
        outDataFile <- file.path(workingDir, paste0(id, ".outRData"))
        dataFile <- file.path(workingDir, paste0(id, ".applyRData"))
        
        #Saving RData files used in script
        pars <- list(...)
        save(xCurrent, FUN, pars, list = getUserFunctions(), file = dataFile)
        rm(xCurrent)
        gc()
        
        #Making script to be submmited
            
        tempScript <- c(
                        extraScriptLines,
                        packages,
                        sources,
                        paste0("load('",  dataFile, "')"),                  
                        paste0("output_", id, " <- do.call( lapply, c(list(X = xCurrent, FUN = FUN), pars))" ),
                        paste0("save(output_", id, ", file='", outDataFile, "')")
                        )
        
        RscriptFile <- file.path(workingDir, paste0(id, ".Rscript"))
        writeLines (tempScript, RscriptFile)
        
        # Submitting job
        cmds <- c(cmds, list(c(extraBashLines, paste0("Rscript --vanilla ", RscriptFile))))
    }
    
    return(cmds)
    
}

mergeListDir <- function(files, varNames, workingDir){
    
    finishedFiles <- files %in% list.files(workingDir)
    if(!all(finishedFiles))
        stop("Not all of the individual task's outputs were found. Uknown error")
       
    files <- files [finishedFiles]
    varNames <- varNames[finishedFiles]
    
    finalF <- list()
    for (i in 1:length(files)){
        load(file.path(workingDir,files[i]))
        finalF <- c(finalF, eval(parse(text = varNames[i])))
    }
    return(finalF)
}

#' getUserFunctions
#' Helper of superApply
#'
#' Retunrs a character vector with the names of the functions
#' in the global enviroment

getUserFunctions <- function() {
    return(c(lsf.str(globalenv())))
}

#' getUserPackages
#' Helper of superApply
#'
#' Returns a character vector with the names of the functions
#' in the global enviroment

getUserPackages <- function() {
    return(names(sessionInfo()$otherPkgs))
}

#' Helper of superApply
#'
#' Takes a function name and a character vector to be put inside
#' independent calls of the function. 
#'
#'
#' @param fun character - the fucntion name as a string
#' @param inside character vector - the items to put inside function
#'
#' @return character vector - of the form [1]"fun(inside[1])" ... [n]"fun(inside[n]))" where n is the lengh of inside. If inside is NULL it returns an empty string
#'
#' @examples 
#' createStringFunction("library", c("ggplot2", "dyplr")) 
#' #[1] "library(ggplot2)" "library(dyplr)"
#'
#' createStringFunction("library") 
#' #[1] ""

createStringFunction <- function(fun, inside = NULL) {
    
    if(is.null(inside))
        return("")
    
    inside <- paste0("(", inside, ")")
    
    return(paste0(fun, inside))
}

#mergeListDir <- function(files){
#   finalF <- list()
#   for (i in 1:length(files)){
#       flush.console()
#       cat(i, "\n")
#       load(file.path(files[i]))
#       varName <- paste0("output", "_", gsub(".outRData", "", files[i]))
#       #aaa <- lapply(eval(parse(text = varName)), function(x) x$all$eqtls[,c("snps", "pvalue")])
#       finalF <- c(finalF, aaa)
#       rm(list = c(varName, "aaa"))
#       gc()
#   }
#   return(finalF)
#}


checkFiles <- function (x,workingDir){
    
    applyFiles <- list.files(workingDir)[grep(SAP_PREFIX, list.files(workingDir))]
    remaining <- sum( !x %in% applyFiles) 
    
    finished <- ifelse(remaining == 0, TRUE, FALSE)
    total <- length(x)
    
    
    return(list(finished = finished, remaining = remaining, total = total))
}

