#' Job class
#' 
#' R6 Class that enables easy submission and manipulations of individual shell jobs to a SLURM cluster.
#' Concatenation is possible for most methods. 
#' Submission is achived by creating and executing a sbatch script, for more details on SLURM refer
#' to \url{https://slurm.schedmd.com/}
#'
#' @usage 
#' x <- Job$new(commandVector, jobName = NULL, outDir = NULL, partition = NULL, time = NULL, mem = NULL, proc = NULL, totalProc = NULL, nodes = NULL, email = NULL)
#' x$wait(stopIfFailed = F, verbose = T)
#' x$cancel()
#' x$getState(simplify = F)
#' x$clean()
#'
#' @docType class
#' @export
#'
#' @section Methods:
#' \enumerate{
#'  \item{Initialize}{
#'      \cr
#'      \code{x <- JobInfo$new(jobName = NULL, outDir = NULL, partition = NULL, time = NULL, mem = NULL, proc = NULL, totalProc = NULL, nodes = NULL, email = NULL)}
#'      \cr Parameters:
#'       \itemize{
#'          \item{jobName} {: character - Name of job. Equivalent to \code{--job-name} of SLURM sbatch. Most output files use it as a suffix}
#'          \item{outDir} {: character - writeable path for sabtch script as well as  SLRUM err and out files}
#'          \item{partition} {: character - Partition to use. Equivalent to \code{--partition} of SLURM sbatch}
#'          \item{time} {: character - Time requested for job execution, one accepted format is "HH:MM:SS". Equivalent to \code{--time} of SLURM sbatch}
#'          \item{mem} {: character - Memory requested for job execution, one accepted format is "xG" or "xMB". Equivalent to \code{--mem} of SLURM sbatch}
#'          \item{proc} {: integer - Number of processors requested per task. Equivalent to \code{--cpus-per-task} of SLURM sbatch}
#'          \item{totalProc} {: integer - Number of nodes requested for job. Equivalent to \code{--nodes} of SLURM sbatch}
#'          \item{email} {: character - email address to send info when job is done. Equivalent to \code{--nodes} of SLURM sbatch}
#'          }
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
#'      \cr Return: data.frame - With SLURM states
#'      }
#'  
#'
#'  \item{Cancel job(s)}{
#'      \cr
#'      \code{x$cancel()}
#'      }
#' 
#'  \item{Get job(s) state}{
#'      \cr
#'      \code{x$getState(simplify = F)}
#'      \cr Parameters:
#'       \itemize{
#'          \item{simplify} {: logical - if TRUE returns a freqeuncy data.frame of job states, otherwise returns individual jobs and their associated job names, job ids, and states}
#'          }
#'      \cr Return: data.frame - With SLURM states
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
#'      }
#'  }
#'
#'
#' @return \code{\link{R6Class}} with methods and fields for SLURM job manipulation
#' @format \code{\link{R6Class}} object.
#' @examples
#' jobInfo <- JobInfo$new()
#'


Job <- R6::R6Class(classname = "Job", inherit = JobInfo,
               public = list(
                             #Instances
                             
                             #Methods
                             initialize = function(commandVector, ...) {
                                 
                                 #Validate args
                                 if(!is.character(commandVector))
                                     stop("commandVector argument has to be character")
                                 
                                 #Set instances
                                 private$commandVector <- commandVector
                                 
                                 super$initialize(...)
                                 
                             },
                             
                             #' submit 
                             #'
                             #' The central method of the job class. It submits a job to a SLURM cluster
                             #' see ?job for details
                             #'
                             #' @param removeScript Logical - if True the bash script use to submit thorugh sbatch is deleted 
                             #'
                             #' @return job - returns self invisibly for method concatenation 
                             submit = function(removeScript = FALSE) {
                                 
                                 # Write submission script
                                 private$writeSubmissionScript()
                                 
                                 # Wait if the number of Jobs allowed has been exceeded
                                 if (!is.null(private$maxJobs)) {
                                     while(getJobNumber() >= private$maxJobs) {
                                         printTime(" Exceeded max number of jobs on queue, waiting ", private$timeWaitMaxjobs, " seconds")
                                         Sys.sleep(private$timeWaitMaxjobs)
                                     }
                                 }
                                 
                                 # Check if submmitted and running; throw an error in that case
                                 private$errorIfSubmittedRunning()
                                 
                                 # Submitting jobs
                                 private$jobId <- systemSubmit(paste("sbatch --parsable", private$scriptPath), wait = rSubmitterOpts$TIME_WAIT_FAILED_CMD, ignore.stdout = F)
                                 
                                 private$isSubmitted<- T
                                
                                 # Delete script if asked for
                                 if (removeScript)
                                     file.remove(private$scriptPath)
                                 
                                 return(invisible(self))
                             }
                             ),
               
               private = list(
                              #Instances
                              commandVector = vector(),
                              
                              # Write submission script
                              
                              #Methods
                              writeSubmissionScript = function() {
                                  
                                  private$createScriptVector()
                                  secureLabels <- "set -euo pipefail"
                                  args <- c(private$scriptVector, secureLabels, private$commandVector)
                                  
                                  writeLines(args, private$scriptPath)
                                                  
                                  return(NULL)
                              }
                              
               
               
               
               )
               )
