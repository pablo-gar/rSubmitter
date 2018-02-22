#' Class providing a basic plataform to store the options of a SLURM jobs
#' Other more complex classes like Job and JobArray are children of this class
#'
#' @docType class
#' @export
#' @keywords data
#' @return Object of \code{\link{R6Class}} with methods for communication with lightning-viz server.
#' @format \code{\link{R6Class}} object.
#' @examples
#' Lightning$new("http://localhost:3000/")
#' Lightning$new("http://your-lightning.herokuapp.com/")
#' @field serveraddress Stores address of your lightning server.
#' @field sessionid Stores id of your current session on the server.
#' @field url Stores url of the last visualization created by this object.
#' @field autoopen Checks if the server is automatically opening the visualizations.
#' @field notebook Checks if the server is in the jupyter notebook mode.
#' #' @section Methods:
#' \describe{
#'   \item{Documentation}{For full documentation of each method go to https://github.com/lightning-viz/lightining-r/}
#'   \item{\code{new(serveraddress)}}{This method is used to create object of this class with \code{serveraddress} as address of the server object is connecting to.}
#'
#'   \item{\code{sethost(serveraddress)}}{This method changes server that you are contacting with to \code{serveraddress}.}

JobInfo <- R6::R6Class(classname = "JobInfo",
                       public = list(
                                     #Instances
                                     
                                     #Methods
                                      #' createScriptVector
                                      #'
                                      #' creates a private vector where each element is one line for
                                      #' a submission script containing the options of the script
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
                                      
                                      printScript = function() {
                                          cat(paste0(private$scriptVector, collapse = "\n"), "\n")
                                      },
                                      
                                     initialize = function(jobName = NULL, outDir = NULL, partition = NULL, time = NULL, mem = NULL, proc = NULL, 
                                                           totalProc = NULL, nodes = NULL, email = NULL, maxJobs = NULL){
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
                                         if(!is.numeric(maxJobs) & !is.null(maxJobs))
                                             stop("maxJobs argument has to be numeric or NULL")
                                         
                                         
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
                                         private$maxJobs <- rSubmitterOpts$MAX_JOBS_ALLOWED
                                         private$timeWaitMaxjobs <- rSubmitterOpts$TIME_WAIT_MAX_JOBS
                                         private$username <- rSubmitterOpts$USERNAME
                                         private$scriptPath <- file.path(outDir, paste0(jobName , ".batch"))
                                         private$outPath <- file.path(outDir, paste0(jobName , ".out"))
                                         private$errPath <- file.path(outDir, paste0(jobName , ".err"))
                                     }
                                     ),
                       
                       private = list(
                                      
                                      #Instances
                                      scriptVector = vector(),
                                      jobName = NULL,
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
                                      waitForCompletion = 10,
                                      completed = "COMPLETED",
                                      failed = c("FAILED", "TIMEOUT", "CANCELLED", "NODE_FAIL")
                                      
                                      #Methods
                                      
                                      
                                      )
                       
                       )
                       

#Job <- R6::R6Class(classname = "Job",
#               public = list(
#                             #Instances
#                             
#                             #Methods
#                             initialize = function(commandList, jobName = NULL, outDir = NULL, partition = NULL, time = NULL, mem = NULL, proc = NULL, totalProc = NULL, nodes = NULL, email = NULL, maxJobs = NULL){
#                                 
#                                 #Set defaults
#                                 if(is.null(outDir))
#                                     outDir <- getwd()
#                                 if(is.null(jobName))
#                                     jobName <- paste(c("rSubmitter_job_", as.character(sample(0:9, 10, T))), collapse="")
#                                 
#                                 #Validate args
#                                 if(!is.character(commandList))
#                                     stop("commandList argument has to be character")
#                                 if(!is.character(jobName))
#                                     stop("jobName argument has to be character")
#                                 if(!is.character(outDir) | !dir.exists(outDir))
#                                     stop("outDir argument has to be character and a valid path")
#                                 if(!is.character(partition) & !is.null(partition))
#                                     stop("partition argument has to be character or NULL")
#                                 if(!is.character(time) & !is.null(time))
#                                     stop("time argument has to be character or NULL")
#                                 if(!is.character(time) & !is.null(time))
#                                     stop("time argument has to be character or NULL")
#                                 if(!is.character(mem) & !is.null(mem))
#                                     stop("mem argument has to be character or NULL")
#                                 if(!is.numeric(proc) & !is.null(proc))
#                                     stop("proc argument has to be numeric or NULL")
#                                 if(!is.numeric(totalProc) & !is.null(totalProc))
#                                     stop("totalProc argument has to be numeric or NULL")
#                                 if(!is.numeric(nodes) & !is.null(nodes))
#                                     stop("nodes argument has to be numeric or NULL")
#                                 if(!is.character(email) & !is.null(email))
#                                     stop("email argument has to be character or NULL")
#                                 if(!is.numeric(maxJobs) & !is.null(maxJobs))
#                                     stop("maxJobs argument has to be numeric or NULL")
#                                 
#                                 
#                                 outDir <- path.expand(outDir)
#                                 
#                                 #Set instances
#                                 private$commandList <- commandList
#                                 private$jobName <- jobName
#                                 private$outDir <- outDir
#                                 private$partition <- partition
#                                 private$time <- time
#                                 private$mem <- mem
#                                 private$proc <- proc
#                                 private$totalProc <- totalProc
#                                 private$nodes <- nodes
#                                 private$email <- email
#                                 private$maxJobs <- rSubmitterOpts$MAX_JOBS_ALLOWED
#                                 private$timeWaitMaxjobs <- rSubmitterOpts$TIME_WAIT_MAX_JOBS
#                                 private$username <- rSubmitterOpts$USERNAME
#                                 private$scriptPath <- file.path(outDir, paste0(jobName , ".batch"))
#                                 private$outPath <- file.path(outDir, paste0(jobName , ".out"))
#                                 private$errPath <- file.path(outDir, paste0(jobName , ".err"))
#
#                             },
#                             
#                             #' submit 
#                             #'
#                             #' The central method of the job class. It submits a job to a SLURM cluster
#                             #' see ?job for details
#                             #'
#                             #' @param removeScript Logical - if True the bash script use to submit thorugh sbatch is deleted 
#                             #'
#                             #' @return job - returns self invisibly for method concatenation 
#                             submit = function(removeScript = FALSE) {
#                                 
#                                 # Write submission script
#                                 private$writeSubmissionScript()
# 
#                                 # Wait if the number of Jobs allowed has been exceeded
#                                 if (!is.null(private$maxJobs)) {
#                                     while(getJobNumber() >= private$maxJobs) {
#                                         Sys.sleep(private$timeWaitMaxjob)
#                                     }
#                                 }
#                                 
#                                 # Submitting jobs
#                                 private$jobId <- systemSubmit(paste("sbatch --parsable", private$scriptPath), wait = rSubmitterOpts$TIME_WAIT_FAILED_CMD, ignore.stdout = F)
#                                
#                                 # Delete script if asked for
#                                 if (removeScript)
#                                     file.remove(private$scriptPath)
#                                 
#                                 return(invisible(self))
#                             },
#                             
#                             #' Gets state of job
#                             getState = function() {
#                                 if(is.null(private$jobId))
#                                     stop("Not job id associated yet, make sure to submit job first, i.e. x$submit()")
#                                 jobInfo <- getJobState(private$jobId)
#                                 return(jobInfo)
#                             },
#                             
#                             #' Waits for job to finish
#                             wait = function() {
#                                 
#                                 while(TRUE) {
#                                     state <- self$getState()
#                                     if(any(state %in% private$completed) | any(state %in% private$failed))
#                                         break
#                                     Sys.sleep(private$waitForCompletion)
#                                 }
#                                 
#                                 return(invisible(self))
#                             },
#                             
#                             getJobId = function() {
#                                 return(private$jobId)
#                             },
#                             
#                             getJobName = function() {
#                                 return(private$jobName)
#                             },
#                             
#                             removeSLURMfiles = function() {
#                                 file.remove(private$scriptPath, private$outPath, private$errPath)
#                                 return(invisible(self))
#                             }
#                             
#                             
#                             
#                             
#                             ),
#               
#               private = list(
#                              #Instances
#                              commandList = vector(),
#                              jobName = NULL,
#                              outDir = NULL,
#                              partition = NULL,
#                              time = NULL,
#                              mem = NULL,
#                              proc = NULL,
#                              totalProc = NULL,
#                              nodes = NULL,
#                              email = NULL,
#                              maxJobs = NULL,
#                              timeWaitMaxjobs = NULL,
#                              username = NULL,
#                              scriptPath = NULL,
#                              outPath = NULL,
#                              errPath = NULL,
#                              jobId = NULL,
#                              waitForCompletion = 10,
#                              completed = "COMPLETED",
#                              failed = c("FAILED", "TIMEOUT", "CANCELLED", "NODE_FAIL"),
#                              
#                              # Write submission script
#                              
#                              #Methods
#                              writeSubmissionScript = function() {
#                                  
#                                  arg <- "#SBATCH "
#                                  args <- "#!/bin/bash\n"
#                                  args <- c(args, paste0(arg , "--job-name=" , private$jobName))
#                                  args <- c(args, paste0(arg , "--output=" , private$outPath))
#                                  args <- c(args, paste0(arg , "--error=" , private$errPath))
#                                  if (!is.null(private$partition)) 
#                                      args <- c(args, paste0(arg , "--partition=" , private$partition))
#                                  if (!is.null(private$time)) 
#                                      args <- c(args, paste0(arg , "--time=" , private$time))
#                                  if (!is.null(private$nodes)) 
#                                      args <- c(args, paste0(arg , "--nodes=" , as.character(private$nodes)))
#                                  if (!is.null(private$mem)) 
#                                      args <- c(args, paste0(arg , "--mem=" , private$mem))
#                                  if (!is.null(private$email)){ 
#                                      args <- c(args, paste0(arg , "--mail-type=END"))
#                                      args <- c(args, paste0(arg , "--mail-user=" , private$email))
#                                  }
#                                  if (!is.null(private$proc)) 
#                                      args <- c(args, paste0(arg , "--cpus-per-task=" , as.character(private$proc)))
#                                  if (!is.null(private$totalProc))    
#                                      args <- c(args, paste0(arg , "--ntasks=" , as.character(private$totalProc)))
#                                  
#                                  args <- c(args, private$commandList)
#                                  
#                                  
#                                  # Writing submmision script for sbatch SLURM
#                                  writeLines(args, private$scriptPath)
#                                                  
#                                  return(NULL)
#                              }
#                              
#                              
#                              )
#               
#               
#               
#               )
