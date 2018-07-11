#' Parallel lapply
#'
#' An easy-to-use form of lapply that emulates parallelization using a SLURM cluster.
#'
#' Mimics the functionality of lapply but implemented
#' in a way that iterations can be submmitted as one or more individual
#' jobs to a SLURM cluster. 
#' Each job batch, err, out, and script files are stored in a temporary folder. Once
#' all jobs have been submmitted, the function waits for them to finish. When they
#' are done executing, all results from individual jobs will be compiled into a single list.
#' 
#' @param x  vector/list - FUN will be applied to the elements of this
#' @param FUN  function - function to be applied to each element of x
#' @param ...  further arguments of FUN
#' @param tasks  integer - number of individual parallel jobs to execute
#' @param workingDir  string - path to folder that will contain all the temporary files needed for submission, execution, and compilation of inidivudal jobs
#' @param packages character vector - package names to be loaded in individual tasks
#' @param sources character vector - paths to R code to be loaded in individual tasks
#' @param extraBashLines character vector - each element will be added as a line to the inidividual task execution bash script before R gets executed. For instance, here you may want to load R if is not in your system by default
#' @param extraScriptLines character vector - each element will be added as a line to the individual task execution R script before starting lapply
#' @param clean logical - if TRUE all files created in workingDir will be deleted
#' @param partition character - Partition to use. Equivalent to \code{--partition} of SLURM sbatch
#' @param time character - Time requested for job execution, one accepted format is "HH:MM:SS". Equivalent to \code{--time} of SLURM sbatch
#' @param mem character - Memory requested for job execution, one accepted format is "xG" or "xMB". Equivalent to \code{--mem} of SLURM sbatch
#' @param proc integer - Number of processors requested per task. Equivalent to \code{--cpus-per-task} of SLURM sbatch
#' @param totalProc integer - Number of tasks requested for job. Equivalent to \code{--ntasks} of SLURM sbatch
#' @param nodes integer - Number of nodes requested for job. Equivalent to \code{--nodes} of SLURM sbatch
#' @param email character - email address to send info when job is done. Equivalent to \code{--nodes} of SLURM sbatch
#'
#' @return list - results of FUN applied to each element in x
#' @export
superApply <- function(x, FUN, ..., tasks = 1, workingDir = getwd(), packages = NULL, sources = NULL, extraBashLines = NULL, extraScriptLines = "", clean = T, partition = NULL, time = NULL, mem = NULL, proc = NULL, totalProc = NULL, nodes = NULL, email = NULL){
    
    if(!is.list(x) & !is.vector(x))
        stop("x hast to be a list of a vector")
    
    if(!is.numeric(tasks))
        stop("tasks has to be numerical")
    
    if(!is.null(extraBashLines) & !is.character(extraBashLines))
        stop("extraBashLines has to be character or NULL")
    
    if(length(tasks) > 1)
        stop("tasks has to be of length 1")
    
    #if()
    #    stop("")
    #if()
    #    stop("")
    #if()
    #    stop("")
    #if()
    #    stop("")

    
    SAP_PREFIX <- "sAp_"
    idPrefix <- paste0(c(SAP_PREFIX, sample(letters, size=3), sample(0:9,size=1)), collapse = "")
    
    workingDir <- path.expand(workingDir)
    FUN <- match.fun(FUN)
    
    # Organizing JobArray parameters
    JobArrayPars <- list(outDir = workingDir, partition = partition, time = time, mem = mem, proc = proc, totalProc = totalProc, nodes = nodes, email = email)
    
    # Getting indeces to partition X into different tasks (i.e. individual jobs)
    partitionIndeces<- getPartitionIndeces(x, tasks = tasks)
    
    # Constructiong paralleleJobs
    printTime("Partitioning function calls\n")
    jobArray <- getJobArray(x, FUN, ..., partitionIndeces = partitionIndeces, idPrefix = idPrefix, workingDir = workingDir, extraScriptLines = extraScriptLines, extraBashLines = extraBashLines, JobArrayPars = JobArrayPars, packages = packages, sources = sources)
    
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
    if(clean) {
        printTime("Cleaning partitioned data\n")
        system(paste0("rm ", file.path(workingDir, paste0(idPrefix, "*"))), ignore.stdout = T, ignore.stderr = T, wait = F)
        printTime("Cleaning done\n")
    }
    
    return(supperApplyResults)
        
}


#' Helper of superApply
#' Creates a list  with slots, containing the start and end indeces 
#' corresponding to the partitions of x required to run the number of parallel tasks
#'
#' Parsing x, is it vector, list? or is it number of repetitions (i.e. x is just a number)?
#' This just to calculate the number of times the FUN has to be executed
getPartitionIndeces <- function(x, tasks = tasks) {
    
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

#' Helper of superApply
#' Submits multiple jobs from the partions of x created in get Partition Indeces
#'
#' @param x list/vector - data to be partition
#' @param FUN function - function to be applied to each element of x
#' @param ... - further arguments of FUN
#' @param idPrefix character - prefix for job names
#' @param partitionIndeces list - output of getPartitionIndeces()
#' @return a JobArray object
getJobArray<- function(x, FUN, ..., idPrefix, partitionIndeces, workingDir, extraScriptLines, extraBashLines, JobArrayPars, packages, sources) {
    
    
    
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

#' Helper of superApply
#' Takes a vector/list x, a function FUN and extra paramaters (...) and creates a Rscript
#' that executes lappy in x using FUN. Scripts are saved in workingDir
#' 
#' @param x - vector/list - data to which lapply will be executed
#' @param FUN - function - function to be applied to x
#' @param ... - extra paramaters passed to FUN
#' @param idPrefix character - prefix for job names
#' @param iStart numeric vector - start indeces where partitions were done on x
#' @param iEnd numeric vector - end indeces where partitions were done on x
createJobScriptsData <- function(x, FUN, ..., idPrefix, iStart, iEnd, workingDir, extraScriptLines = "", extraBashLines = "", packages = NULL, sources = NULL) {
    
    cmds <- list()
    
    FUN <- match.fun(FUN)
    
    # Checking if I need to load current packages or if user-defined packages can be loaded
    if(is.null(packages)) {
        packages <- createStringFunction ("library", getUserPackages())
    } else if (packages == "") {
        packages <- packages
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
        if(!is.null(extraBashLines)) {
            cmds <- c(cmds, list(c(extraBashLines, paste0("Rscript --vanilla ", RscriptFile))))
        } else {
            cmds <- c(cmds, list(c(paste0("Rscript --vanilla ", RscriptFile))))
        }
            
    }
    
    return(cmds)
    
}

#' Helper of superApply
#' merges the result of independ jobs after completion of parallel lapply executions
#' @param files character vector - files to be merged
#' @param varNames character vector - varnames associated to each file to be merged
#' @param workingDir character - working directory
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

#' Helper of superApply
#'
#' @return a character vector with the names of the functions in the global enviroment
getUserFunctions <- function() {
    return(c(lsf.str(globalenv())))
}

#' Helper of superApply
#'
#' @return Returns a character vector with the names of the packages in the global enviroment
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


