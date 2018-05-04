#' Job class - inherits from JobInfo class
#' 
#' R6 Class that enables easy submission and manipulation of individual shell jobs to a SLURM cluster.
#'
#' Submission is achived by creating and executing an sbatch script, for more details on SLURM refer
#' to \url{https://slurm.schedmd.com/}
#' Concatenation is possible for most methods.
#'
#' @usage 
#' x <- Job$new(commandVector, jobName = NULL, outDir = NULL, partition = NULL, time = NULL, mem = NULL, proc = NULL, totalProc = NULL, nodes = NULL, email = NULL)
#' x$submit()
#' x$wait(stopIfFailed = F, verbose = T)
#' x$cancel()
#' x$getState(simplify = F)
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
#'      \code{x <- JobInfo$new(commandVector, jobName = NULL, outDir = NULL, partition = NULL, time = NULL, mem = NULL, proc = NULL, totalProc = NULL, nodes = NULL, email = NULL)}
#'      \cr Parameters:
#'       \itemize{
#'          \item{commandVector} {: character vector - Each element should be an independent shell command to be included in the current job. This is the only required argument}
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
#'      \cr Creates sbatch script to \code{outDir} and submits it through a system call to sbatch. The script, STDERR and STDOUT sbatch files will be written to \code{outDir}.In the case sbatch returns a non-zero status, it will try resubmitting up 12 times with a defined interval time(TIME_WAIT_JOB_STATUS option at ~/.rSubmitter)
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
#' @return \code{\link{R6Class}} with methods and fields for SLURM job manipulation
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
#' # Create and submit requesting multiple processors
#' job <- Job$new(c("echo this is multi-processing", "nproc"), proc = 8)
#' job$submit()
#'
#' # Many options defined
#' job <- Job$new(c("echo this are all the options", "nproc"), jobName = "dummy", outDir = "~", partition = "normal", time = "4:00:00", mem = "8G", proc = 8, totalProc = 1, nodes = 1, email = my@email.com)
#' job$submit()
#'
#' # Removes script, err and output files
#' job$clean()


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
                             
                             # submit 
                             #
                             # The central method of the job class. It submits a job to a SLURM cluster
                             #
                             # @return job - returns self invisibly for method concatenation 
                             submit = function() {
                                 
                                 # Check if submmitted and running; throw an error in that case
                                 private$errorIfSubmittedRunning()
                                 
                                 # Write submission script
                                 private$writeSubmissionScript()
                                 
                                 # Wait if the number of Jobs allowed has been exceeded
                                 if (!is.null(private$maxJobs)) {
                                     while(getJobNumber() >= private$maxJobs) {
                                         printTime(" Exceeded max number of jobs on queue, waiting ", private$timeWaitMaxjobs, " seconds")
                                         Sys.sleep(private$timeWaitMaxjobs)
                                     }
                                 }
                                 
                                 # Submitting jobs
                                 private$jobId <- systemSubmit(paste("sbatch --parsable", private$scriptPath), wait = rSubmitterOpts$TIME_WAIT_FAILED_CMD, ignore.stdout = F)
                                 
                                 private$isSubmitted<- T
                                
                                 
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
