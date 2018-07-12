#' Parent class of Job and JobArray
#' 
#' R6 Class providing an essential platform to process SLURM jobs.
#' This class should not be explicitly used by the user.
#' To create, manipulate, and submit SLURM jobs please refer to the Job and JobArray classes
#'
#' @section Usage:
#' \code{
#' x <- JobInfo$new(jobName = NULL, outDir = NULL, partition = NULL, time = NULL, mem = NULL, proc = NULL, totalProc = NULL, nodes = NULL, email = NULL) \cr
#' x$wait(stopIfFailed = F, verbose = T) \cr
#' x$cancel() \cr
#' x$getState(simplify = F) \cr
#' x$clean() \cr
#' }
#'
#' @docType class
#' @format R6 class
#' @export
#'
#' @section Method description:
#' \enumerate{
#'  \item{\strong{Initialize}}{
#'      \cr
#'      \code{x <- JobInfo$new(jobName = NULL, outDir = NULL, partition = NULL, time = NULL, mem = NULL, proc = NULL, totalProc = NULL, nodes = NULL, email = NULL)}
#'      \cr Parameters:
#'       \itemize{
#'          \item{jobName} {: character - Name of job, if NULL one will be generated of the form rSubmitter_job_[random_alphanumeric]. Equivalent to \code{--job-name} of SLURM sbatch. Most output files use it as a suffix}
#'          \item{outDir} {: character - writeable path for the sabtch script as well as the SLRUM STDERR and STDOUT files. If NULL the current working directory will be used}
#'          \item{partition} {: character - Partition to use. Equivalent to \code{--partition} of SLURM sbatch}
#'          \item{time} {: character - Time requested for job execution, one accepted format is "HH:MM:SS". Equivalent to \code{--time} of SLURM sbatch}
#'          \item{mem} {: character - Memory requested for job execution, one accepted format is "xG" or "xMB". Equivalent to \code{--mem} of SLURM sbatch}
#'          \item{proc} {: integer - Number of processors requested per task. Equivalent to \code{--cpus-per-task} of SLURM sbatch}
#'          \item{totalProc} {: integer - Number of tasks requested for job. Equivalent to \code{--ntasks} of SLURM sbatch}
#'          \item{nodes} {: integer - Number of nodes requested for job. Equivalent to \code{--nodes} of SLURM sbatch}
#'          \item{email} {: character - email address to send info when job is done. Equivalent to \code{--nodes} of SLURM sbatch}
#'          }
#'      \cr Return: \cr object of class \code{Job}
#'      }
#'      
#'  \item{\strong{Wait for job(s) to finish}}{
#'      \cr
#'      \code{x$wait(stopIfFailed = F, verbose = T)}
#'      \cr Time between each job state check is defined in the entry TIME_WAIT_JOB_STATUS:seconds in the config file located at ~/.rSubmitter
#'      \cr Parameters:
#'       \itemize{
#'          \item{stopIfFailed} {: logical -  if TRUE stops when one job has failed (only useful for JobArray) it then cancels the 
#'                                  rest of the pending and running jobs. If FALSE and one or more Jobs failed it raises a warning for each failed job}
#'          \item{verbose} {: logical -  if TRUE prints the job state(s) at every check}
#'          }
#'      \cr Return: \cr self - for method concatenation
#'      }
#'
#'  \item{\strong{Cancel job(s)}}{
#'      \cr
#'      \code{x$cancel()}
#'      \cr Return: \cr self - for method concatenation
#'      }
#' 
#'  \item{\strong{Get job(s) state}}{
#'      \cr
#'      \code{x$getState(simplify = F)}
#'      \cr Parameters:
#'       \itemize{
#'          \item{simplify} {: logical - if TRUE returns a freqeuncy data.frame of job states, otherwise returns individual jobs and their associated job names, job ids, and states}
#'          }
#'      \cr Return: \cr data.frame - With SLURM states
#'      }
#'
#'  \item{\strong{Remove SLURM-associated files}}{
#'      \cr
#'      \code{x$clean(script = TRUE, out = TRUE, err = TRUE)}
#'      \cr Parameters:
#'       \itemize{
#'          \item{script} {: logical - if TRUE deletes sbatch submission script(s) associated to this object}
#'          \item{out} {: logical - if TRUE deletes STDOUT file(s) from SLURM associated to this object} 
#'          \item{err} {: logical - if TRUE deletes STDERR file(s) from SLURM associated to this object}
#'          }
#'      \cr Return: \cr self - for method concatenation
#'      }
#'  }
#'
#'
#' @return \code{\link{R6Class}} with methods and fields for SLURM job manipulation
#' @examples
#' \dontrun{
#' jobInfo <- JobInfo$new()
#' }
JobInfo <- R6::R6Class(classname = "JobInfo",
                       public = list(
                                     #Instances
                                     
                                     #Methods
                                     initialize = function(jobName = NULL, outDir = NULL, partition = NULL, time = NULL, mem = NULL, proc = NULL, 
                                                           totalProc = NULL, nodes = NULL, email = NULL){
                                         #Set defaults
                                         if(is.null(outDir))
                                             outDir <- getwd()
                                         if(is.null(jobName))
                                             jobName <- paste(c("rSubmitter_job_", as.character(sample(0:9, 10, T))), collapse="")
                                         
                                         #Validate args
                                         if(!is.character(jobName))
                                             stop("jobName argument has to be character")
                                         if(!is.character(outDir) | !dir.exists(outDir))
                                             stop("outDir argument has to be character and a valid path")
                                         if(!is.character(partition) & !is.null(partition))
                                             stop("partition argument has to be character or NULL")
                                         if(!is.character(time) & !is.null(time))
                                             stop("time argument has to be character or NULL")
                                         if(!is.character(time) & !is.null(time))
                                             stop("time argument has to be character or NULL")
                                         if(!is.character(mem) & !is.null(mem))
                                             stop("mem argument has to be character or NULL")
                                         if(!is.numeric(proc) & !is.null(proc))
                                             stop("proc argument has to be numeric or NULL")
                                         if(!is.numeric(totalProc) & !is.null(totalProc))
                                             stop("totalProc argument has to be numeric or NULL")
                                         if(!is.numeric(nodes) & !is.null(nodes))
                                             stop("nodes argument has to be numeric or NULL")
                                         if(!is.character(email) & !is.null(email))
                                             stop("email argument has to be character or NULL")
                                         
                                         
                                         outDir <- path.expand(outDir)
                                         
                                         #Set instances
                                         private$jobName <- jobName
                                         private$outDir <- outDir
                                         private$partition <- partition
                                         private$time <- time
                                         private$mem <- mem
                                         private$proc <- proc
                                         private$totalProc <- totalProc
                                         private$nodes <- nodes
                                         private$email <- email
                                         private$maxJobs <- as.numeric(rSubmitterOpts$MAX_JOBS_ALLOWED)
                                         private$timeWaitMaxjobs <- as.numeric(rSubmitterOpts$TIME_WAIT_MAX_JOBS)
                                         private$username <- rSubmitterOpts$USERNAME
                                         private$scriptPath <- file.path(outDir, paste0(jobName , ".batch"))
                                         private$outPath <- file.path(outDir, paste0(jobName , ".out"))
                                         private$errPath <- file.path(outDir, paste0(jobName , ".err"))
                                         private$waitForCompletion <- rSubmitterOpts$TIME_WAIT_JOB_STATUS
                                         
                                         private$dateCreation <- format(Sys.time() - 1,"%m/%d/%y-%H:%M:%S")
                                     },
                                      
                                     printOptions = function() {
                                         cat(paste0(private$scriptVector, collapse = "\n"), "\n")
                                     },
                                     
                                     
                                     # getState (simplify = F)
                                     # Gets a state table as a data.frame with 3 columns
                                     # jobId, jobName, and jobState
                                     #
                                     # If simplify is TRUE it returns a frequency table of unique
                                     # state
                                     #
                                     # @param simplify - Logical, if true it returns a frequncy table
                                     # @return data.frame or table object
                                     getState = function(simplify = F) {
                                         if(is.null(private$jobId))
                                             stop("Not job id associated yet, make sure to submit job first, i.e. x$submit()")
                                         
                                         jobInfo <- getJobState(private$jobId, dateJob = private$dateCreation)
                                         
                                         if(simplify) {
                                             if(private$isJobArray)
                                                 jobInfo <- private$expandJobArrayStates(jobInfo)
                                             jobInfo <- table(jobInfo$jobState)
                                         }
                                         
                                         return(jobInfo)
                                     }, 
                                     
                                     # clean(script = TRUE, out = TRUE, err = TRUE)
                                     # Eliminate one or more of the job-associated files, i.e. sbatch script, erro and output from SLURM
                                     # @param script  Logical - if TRUE deletes sbatch submission script 
                                     # @param out  Logical - if TRUE deletes output from SLURM
                                     # @param err Logical - if TRUE deletes error file(s) from SLURM
                                     clean = function(script = TRUE, out = TRUE, err = TRUE) {
                                         
                                         if(!private$areAllDone())
                                             stop("Cannot remove files now because jobs are not finished. Consider doing x$wait() and then x$removeJobFiles()")
                                         
                                         if(script) 
                                             file.remove(private$scriptPath)
                                         
                                         if(out) {
                                             toRemove <- private$outPath
                                             if(private$isJobArray)
                                                 toRemove <- gsub("_%A_%a", "*", toRemove)
                                             file.remove(Sys.glob(toRemove))
                                         }
                                         
                                         if(err) {
                                             toRemove <- private$errPath
                                             if(private$isJobArray)
                                                 toRemove <- gsub("_%A_%a", "*", toRemove)
                                             file.remove(Sys.glob(toRemove))
                                         }
                                         
                                         return(invisible(self))
                                     },
                                     
                                     # cancel()
                                     # Cancel a job
                                     cancel = function() {
                                         
                                         if(!private$isSubmitted)
                                            stop("Can't cancel. Job(s) has not been submitted. Consider doing x$submit()")
                                         
                                         stateTable <- self$getState()
                                         jobsToCancel <- stateTable[stateTable$jobState %in% private$running, "jobId"]
                                         if(private$isJobArray) {
                                             nJobs <- sum(private$expandJobArrayStates(stateTable)$jobState %in% private$running)
                                         } else {
                                             nJobs <- length(jobsToCancel)
                                         }
                                         
                                         if(length(jobsToCancel) == 0) {
                                             warning("\nNo jobs to cancel, potential reasons:\n",
                                                     "   1. Jobs are still in the process of submission\n",
                                                     "   2. All jobs were completed already\n",
                                                     "   3. There are no existing jobs\n",
                                                     "To troubleshoot do x$getState()\n")
                                         } else {
                                             printTime("Cancelling ", nJobs, " job(s)\n")
                                             cancelJob(jobsToCancel)
                                             printTime("Finished sending cancel signal\n")
                                         }
                                         
                                         return(invisible(self))
                                     },
                                     
                                     # wait(stopIfFailed = F, verbose = T)
                                     # Waits for the submmited job(s) to be completed
                                     # Time in between each status check can be set in the config file
                                     # located at ~/.rSubmitter in the entry TIME_WAIT_JOB_STATUS:seconds
                                     #
                                     # @param stopIfFailed Logical - if TRUE stops waiting when one job has failed (only useful for 
                                     #                        JobArray), it then cancels the rests of the pending and running jobs.
                                     #                        If FALSE and one or more Jobs failed it raises warnings for each failed
                                     #                        job.
                                     # @param verbose Logical - if TRUE prints the job state(s) at every check.
                                     # @return self
                                     wait = function(stopIfFailed = F, verbose = T) {
                                         
                                         nodeFailedTrials = 5
                                         
                                         while(TRUE) {
                                             Sys.sleep(private$waitForCompletion)
                                             stateTable <- self$getState() 
                                             
                                             if(private$isJobArray) 
                                                 stateTable <- private$expandJobArrayStates(stateTable)
                                             
                                             state <- stateTable$jobState
                                             
                                             if(verbose) {
                                                 jobStatesFreq <- table(state)
                                                 
                                                 cat("\33[2K")
                                                 printTime("--- Cluster Status |", carriageReturn = T )
                                                 for(s in names(jobStatesFreq))
                                                     cat(" ", s, "=", jobStatesFreq[s], "|")
                                                 
                                             }
                                                 
                                             if(any(state %in% private$failed)) {
                                                 
                                                 if(any(state %in% private$nodeFailed) & nodeFailedTrials > 0 ) {
                                                     
                                                     cat("\n")
                                                     printTime("Failed to communicate with node, trying again (", nodeFailedTrials, ")\n")
                                                     
                                                     nodeFailedTrials = nodeFailedTrials - 1
                                                     next
                                                 }
                                                 
                                                 failed <- stateTable[state %in% private$failed,]
                                                 if(private$isJobArray) {
                                                     failedPaths <- file.path(dirname(private$errPath), paste0(failed$jobName, "_", failed$jobId, ".[err|out]"))
                                                 } else {
                                                     failedPaths <- file.path(dirname(private$errPath), paste0(failed$jobName, ".[err|out]"))
                                                 }
                                                     
                                                 
                                                 if(stopIfFailed) {
                                                     if(verbose)
                                                         cat("\n")
                                                     self$cancel()
                                                     stop("\nOne or more jobs failed. All jobs have now been cancelled. Failed jobs SLURM files:\n", paste(failedPaths, collapse = "\n"))
                                                 }                                                  
                                                 
                                             }
                                             
                                             if(all(state %in% c(private$completed, private$failed)))
                                                 break
                                         }
                                     
                                         if(verbose)
                                             cat("\n")
                                         
                                         if(!stopIfFailed & any(state %in% private$failed))
                                             warning("\nOne or more jobs failed. Failed jobs SLURM files:\n", paste(failedPaths, collapse = "\n"))
                                         
                                         return(invisible(self))
                                      }

                                      
                                     ),
                       
                       private = list(
                                      
                                      #Instances
                                      scriptVector = vector(),
                                      jobName = NULL,
                                      isJobArray = FALSE,
                                      isSubmitted = FALSE,
                                      outDir = NULL,
                                      partition = NULL,
                                      time = NULL,
                                      mem = NULL,
                                      proc = NULL,
                                      totalProc = NULL,
                                      nodes = NULL,
                                      email = NULL,
                                      maxJobs = NULL,
                                      timeWaitMaxjobs = NULL,
                                      username = NULL,
                                      scriptPath = NULL,
                                      outPath = NULL,
                                      errPath = NULL,
                                      jobId = NULL,
                                      waitForCompletion = NULL,
                                      dateCreation = NULL,
                                      completed = "COMPLETED",
                                      failed = c("FAILED", "TIMEOUT", "CANCELLED", "NODE_FAIL", "BOOT_FAIL", "DEADLINE", "OUT_OF_MEMORY"),
                                      nodeFailed = "NODE_FAIL",
                                      running = c("CONFIGURING", "PENDING", "RUNNING", "RESIZING", "SUSPENDED", "PREEMPTED", "COMPLETING"),
                                      notAvail = c("NOTAVAIL"),
                                      
                                      #Methods
                                      # createScriptVector
                                      #
                                      # creates a private vector where each element is one line for
                                      # a submission script containing the options of the script
                                      createScriptVector = function() {
                                          arg <- "#SBATCH "
                                          args <- "#!/bin/bash\n"
                                          args <- c(args, paste0(arg , "--job-name=" , private$jobName))
                                          args <- c(args, paste0(arg , "--output=" , private$outPath))
                                          args <- c(args, paste0(arg , "--error=" , private$errPath))
                                          if (!is.null(private$partition)) 
                                              args <- c(args, paste0(arg , "--partition=" , private$partition))
                                          if (!is.null(private$time)) 
                                              args <- c(args, paste0(arg , "--time=" , private$time))
                                          if (!is.null(private$nodes)) 
                                              args <- c(args, paste0(arg , "--nodes=" , as.character(private$nodes)))
                                          if (!is.null(private$mem)) 
                                              args <- c(args, paste0(arg , "--mem=" , private$mem))
                                          if (!is.null(private$email)){ 
                                              args <- c(args, paste0(arg , "--mail-type=END"))
                                              args <- c(args, paste0(arg , "--mail-user=" , private$email))
                                          }
                                          if (!is.null(private$proc)) 
                                              args <- c(args, paste0(arg , "--cpus-per-task=" , as.character(private$proc)))
                                          if (!is.null(private$totalProc))    
                                              args <- c(args, paste0(arg , "--ntasks=" , as.character(private$totalProc)))
                                          
                                          
                                          private$scriptVector <- args
                                      },
                                      
                                      # Logical functions to assess job states
                                      areAllDone = function() {
                                          return(all(self$getState()$jobState %in% c(private$completed, private$failed)))
                                      },
                                      
                                      areAllCompleted = function() {
                                          return(all(self$getState()$jobState %in% private$completed))
                                      },
                                      
                                      areAllFailed = function() {
                                          return(all(self$getState()$jobState %in% private$failed))
                                      },
                                      
                                      areAllRunning = function() {
                                          return(all(self$getState()$jobState %in% private$running))
                                      },
                                      
                                      areAllNotAvail = function() {
                                          return(all(self$getState()$jobState %in% private$notAvail))
                                      },
                                      
                                      
                                      isCompleted = function () {
                                          return(any(self$getState()$jobState %in% private$completed))
                                      }, 
                                      
                                      isFailed = function () {
                                          return(any(self$getState()$jobState %in% private$failed))
                                      }, 
                                      
                                      isRunning = function() {
                                          return(any(self$getState()$jobState %in% private$running))
                                      }, 
                                      
                                      isNotAvail = function() {
                                          return(any(self$getState()$jobState %in% private$notAvail))
                                      },
                                      
                                      # Functions to retrieve number of different types of jobs
                                      nRunning = function() {
                                          return(sum(self$getState()$jobState %in% private$running))
                                      },
                                      
                                      # expandJobArrayStates
                                      # from a state table obtain from public$getState()
                                      # It expands any jobs of the form jobId_[1-100] to multiple rows 
                                      # of the form jobId_1, jobId_2, ... , jobId_100
                                      expandJobArrayStates = function(jobStateTable) {
                                          
                                          toExpand <- grepl("_\\[\\d+-\\d+\\]", jobStateTable$jobId)
                                          if(any(toExpand)) {
                                              toExpandTable <- jobStateTable[toExpand,]
                                              
                                              jobIds <-  gsub("(.+)_\\[\\d+-\\d+\\]", "\\1", toExpandTable$jobId)
                                              fromI <- gsub(".+_\\[(\\d+)-\\d+\\]", "\\1", toExpandTable$jobId)
                                              toI <-  gsub(".+_\\[\\d+-(\\d+)\\]", "\\1", toExpandTable$jobId)
                                              
                                              expanded <- list()
                                              for (i in 1:nrow(toExpandTable)){
                                                  expanded[[i]] <- data.frame(jobId = paste0(jobIds[i], "_", seq(fromI[i], toI[i])), 
                                                                              jobName = toExpandTable[i,"jobName"], 
                                                                              jobState = toExpandTable[i,"jobState"])
                                              }
                                              
                                              expanded[[i+1]] <- jobStateTable[!toExpand,]
                                              jobStateTable <- do.call(rbind, expanded)
                                          }
                                          
                                          return(jobStateTable)
                                      },
                                      
                                      errorIfSubmittedRunning = function() {
                                          if(private$isSubmitted) {
                                              if(!private$areAllDone()) {
                                                  stop("Can't submit. There is current job(s) running from this object. Consider doing x$cancel()")
                                              }
                                          }
                                      }
                                      
                                      )
                       
                       )
                       

