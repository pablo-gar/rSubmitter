#' JobArray class - inherits from JobInfo class
#' 
#' R6 Class that enables easy submission and manipulation of SLURM job arrays.
#'
#' Job arrays are quickly genearated and submitted allowing for efficient creation and execution
#' of many shell jobs in a SLURM cluster, thus facilitating parallelization when needed. This class eliminates the cumbersome task of
#' manually creating SLURM arrays, in its simplest form two lines of code are sufficient for a job array submmission.
#' Additionally, there is an added functionallity to monitor and wait for all jobs in an array once they have been submitted.
#'
#' All jobs in an job array share the same execution requirements.
#' Each element in `commandList` will be submitted as an individual jobs in the array. Elements of `commandList` should be
#' vectors of shell commands.
#'
#' Submission is achived by creating and executing an sbatch script, for more details on SLURM refer
#' to https://slurm.schedmd.com/. For job arrays refer to https://slurm.schedmd.com/job_array.html
#' Concatenation is possible for most methods.
#'
#' @usage 
#' x <- JobArray$new(commandList, jobName = NULL, outDir = NULL, partition = NULL, time = NULL, mem = NULL, proc = NULL, totalProc = NULL, nodes = NULL, email = NULL)
#' x$submit()
#' x$wait(stopIfFailed = F, verbose = T)
#' x$length()
#' x$cancel()
#' x$getState(simplify = F)
#' x$getJobNames()
#' x$clean()
#'
#' @docType class
#' @format R6 class
#' @export
#'
#' @section Method description:
#' \enumerate{
#'  \item{Initialize}{
#'      \cr
#'      \code{x <- JobInfo$new(commandList, jobName = NULL, outDir = NULL, partition = NULL, time = NULL, mem = NULL, proc = NULL, totalProc = NULL, nodes = NULL, email = NULL)}
#'      \cr Parameters:
#'       \itemize{
#'          \item{commandList} {: list of character vectors - Each element of the list should be a vector of shell commands. Each element of the list corresponds to a different job in the array.}
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
#'  \item{Submit job(s)}{
#'      \cr
#'      \code{x$submit()}
#'      \cr Creates a job array sbatch script to \code{outDir} and submits it through a system call to sbatch. The script, STDERR and STDOUT sbatch files will be written to \code{outDir}. In the case sbatch returns a non-zero status, it will try resubmitting up 12 times with a defined interval time(TIME_WAIT_JOB_STATUS option at ~/.rSubmitter). Each element of the array will have its individual STDERR and STDOUT files with the format \code{jobName_[1-Inf].[err|out]}. Important options pulled from the config file located at ~/.rSubmitter: maximum number of jobs allowed in the queue (MAX_JOBS_ALLOWED:n);  maximum length of a job array (MAX_JOB_ARRAY_LENGTH:n)
#'      \cr Return: \cr self - for method concatenation
#'      }
#'  
#'  \item{Wait for job(s) to finish}{
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
#'  \item{Get length of array}{
#'      \cr
#'      \code{x$length()}
#'      \cr Return: \cr numeric - number of individual jobs in array
#'      }

#'
#'  \item{Cancel job(s)}{
#'      \cr
#'      \code{x$cancel()}
#'      \cr Return: \cr self - for method concatenation
#'      }
#' 
#'  \item{Get job(s) state}{
#'      \cr
#'      \code{x$getState(simplify = F)}
#'      \cr Parameters:
#'       \itemize{
#'          \item{simplify} {: logical - if TRUE returns a freqeuncy data.frame of job states, otherwise returns individual jobs and their associated job names, job ids, and states}
#'          }
#'      \cr Return: \cr data.frame - With SLURM states
#'      }
#'
#'  \item{Get job name(s)}{
#'      \cr
#'      \code{x$getJobNames()}
#'      \cr Return: \cr character vector - With individual job names.
#'      }
#'
#'  \item{Remove SLURM-associated files}{
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
#' @return \code{\link{R6Class}} with methods and fields for SLURM job array manipulation
#' @examples
#' # Create and submit dummy job with random job name
#' job <- Job$new("echo hola world!")
#' job$submit()
#'
#' # Create and submit dummy job with specific job name
#' job <- Job$new("echo hola world!", jobName = "dummy")
#' job$submit()
#' 
#' # Create, submit and wait for a job to finish
#' job <- Job$new(c("echo hola world!", "sleep 60"))
#' job$submit()
#' job$wait()
#'
#' # Method concatenation
#' job <- Job$new("echo hola world!", jobName = "dummy")
#' job$submit()$wait()$clean()
#'
#' # Create, submit and cancel a Job
#' job <- Job$new(c("echo hola world!", "sleep 60"))
#' job$submit()
#' job$cancel()
#'
#' # Create and submit a memory-heavy job
#' job <- Job$new("echo this is too much memory!", mem = "16G")
#' job$submit()
#'
#' # Create and submit a requesting multiple processors
#' job <- Job$new(c("echo this is multi-processing", "nproc"), proc = 8)
#' job$submit()
#'
#' # Many options defined
#' job <- Job$new(c("echo this are all the options", "nproc"), jobName = "dummy", outDir = "~", partition = "normal", time = "4:00:00", mem = "8G", proc = 8, totalProc = 1, nodes = 1, email = my@email.com)
#' job$submit()
#'
#' # Removes script, err and output files
#' job$clean()

JobArray <- R6::R6Class(
                       
    classname = "JobArray", inherit = JobInfo,
    public = list(
                  #INSTANCES
                  
                  #METHODS
                  initialize = function(commandList, ...) {
    
                      #Validate args
                      if(!is.list(commandList))
                          stop("commanList has to be a list of character vectors")
                      if(length(commandList) < 1)
                          stop("commandList has to be of length >= 1")
                      if(length(commandList) > rSubmitterOpts$MAX_JOB_ARRAY_LENGTH)
                          stop("\nThe number of jobs (i.e. `length(commandList)`) is greater than max\n", 
                               "length of a job array specified in the rSubmitter options (", rSubmitterOpts$MAX_JOB_ARRAY_LENGTH, ").\n",
                               "Consider using less jobs or changing option MAX_JOB_ARRAY_LENGTH:n in the config file ~/.rSubmitter")
                      
                      #if(length(commandList) > getMaxJobArrayLength())
                      #    warning("\nThe number of jobs is greater than the max length of a job array\n", 
                      #            "defined by your SLURM system. The array won't likely be submitted")
                      
                      for(i in commandList)
                          if(!is.character(i) | !is.vector(i))
                              stop("All elements of commandList have to be character vectors")
    
                      #Set instances
                      private$commandList <- commandList
    
                      super$initialize(...)
                      private$isJobArray <- TRUE
                      
                      # Overwrite err and out paths 
                      private$outPath <- file.path(private$outDir, paste0(private$jobName , "_%A_%a.out"))
                      private$errPath <- file.path(private$outDir, paste0(private$jobName , "_%A_%a.err"))
                  },

                  addJob = function(x) {
                      if(!is.character(x) | !is.vector(x))
                          stop("x has to be a character vector")
                      
                      private$commandList <- c(private$commandList, list(x))
                      return(invisible(self))
                  },
                  
                  #getJob = function(i) {
                  #    private$checkIndex(i)
                  #    return(invisible(private$jobs[[i]]))
                  #},
                  
                  # Submit all jobs
                  submit = function(removeScript = F) {
                      
                      # Check if submmitted and running; throw an error in that case
                      private$errorIfSubmittedRunning()
                      
                      # Write submission script
                      private$writeSubmissionScript()
                      
                      # Trying to submit up to 4 times (in case there is latency problems between checking for max jobs and submitting)
                      timesToTry <- 4
                      for(i in seq(1, timesToTry)) {
                          # Wait if the number of Jobs allowed has been exceeded
                          if (!is.null(private$maxJobs)) {
                              
                              if(self$length() > private$maxJobs)
                                  stop("Length of array(", self$length(), ") greater than maximum number of jobs allowed(", private$maxJobs,"). Consider edditing the config file ~/.rSubmitter")
                              
                              while(getJobNumber() + self$length() >= private$maxJobs) {
                                  printTime(" Exceeded max number of jobs on queue, waiting ", private$timeWaitMaxjobs, " seconds")
                                  Sys.sleep(private$timeWaitMaxjobs)
                              }
                          }
                          
                          # Submitting jobs, in the last try if will throw an error if it fails 
                          jobId <- systemSubmit(paste("sbatch --parsable", private$scriptPath), wait = rSubmitterOpts$TIME_WAIT_FAILED_CMD, ignore.stdout = F, 
                                                stopIfFailed = ifelse(i == timesToTry, T, F) )
                          
                          # If succeeding submitting jobs
                          if(length(jobId) > 0) {
                              private$jobId <- jobId
                              break
                          }
                          
                      }
                      
                      # Constuct job ids and names for all jobs
                      private$createJobIds()
                      private$createJobNames()
                      
                      private$isSubmitted<- T

                      # Delete script if asked for
                      if (removeScript)
                          file.remove(private$scriptPath)

                      return(invisible(self))
                  },
                  
                  length = function() {
                      return(length(private$commandList))
                  },
                  
                   getJobNames = function() {
                       if(is.null(private$jobName))
                           warning("Job(s) have not yet been submmitted. Job names are meaningless before submission")
                       
                       private$createJobNames()
                       return(private$jobNameAll)
                   }
 
                  
                  ),
                        
    private = list(
                   #INSTANCES
                   commandList = NULL,
                   states = NULL,
                   jobNameAll = NULL,
                   jobIdAll = NULL,
                   
                   #METHODS
                   # Writes the submission script specific to Job arrays
                   writeSubmissionScript = function() {
    
                       private$createScriptVector()
                      
                       # Add job Array flag
                       args <- c(private$scriptVector, paste0("#SBATCH --array=1-", self$length()))
                       
                       # Creates the bash case control statement
                       case <- "case $SLURM_ARRAY_TASK_ID in"
                       for (i in 1:self$length()) {
                           case <- c(case, paste0("   ", i, " )"))
                           case <- c(case,  "      set -euo pipefail")
                           case <- c(case, paste0("      ", private$commandList[[i]]))
                           case <- c(case, paste0("      ", ";;"))
                       }
                       case <- c(case, "esac")
                       case <- c(case,  "exit 0")
                       
                       # Concatenates SBATCH option to case statement
                       args <- c(args, case)
                       
                       writeLines(args, private$scriptPath)
                   }, 
                   
                   # Creates job ids of the form masterId_1 ... masterId_n
                   createJobIds = function() {
                       private$jobIdAll <- paste0(private$jobId, "_", 1:self$length())
                   },
                   
                   # Creates job names of the form jobName_1 ... jobName_n
                   createJobNames = function() {
                       private$jobNameAll <- paste0(private$jobName, "_", 1:self$length())
                   }
                       
                   
                   # Throws error if i is out of bounds in job list
                   
                   )
)

