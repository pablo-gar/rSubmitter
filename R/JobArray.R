#' Array of Job
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
                  
                  #' Submit all jobs
                  submit = function(removeScript = F) {
                      
                      # Write submission script
                      private$writeSubmissionScript()

                      # Wait if the number of Jobs allowed has been exceeded
                      if (!is.null(private$maxJobs)) {
                          
                          if(self$length() > private$maxJobs)
                              stop("Length of array(", self$length(), ") greater than maximum number of jobs allowed(", private$maxJobs,"). Consider edditing the config file ~/.rSubmitter")
                          
                          while(getJobNumber() + self$length() >= private$maxJobs) {
                              printTime(" Exceeded max number of jobs on queue, waiting ", private$timeWaitMaxjobs, " seconds")
                              Sys.sleep(private$timeWaitMaxjobs)
                          }
                      }
                      
                      # Check if submmitted and running; throw an error in that case
                      private$errorIfSubmittedRunning()
                      
                      # Submitting jobs
                      private$jobId <- systemSubmit(paste("sbatch --parsable", private$scriptPath), wait = rSubmitterOpts$TIME_WAIT_FAILED_CMD, ignore.stdout = F)
                      
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
                  
                  getCommandList = function() {
                      return(private$commandList)
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
                   #' Writes the submission script specific to Job arrays
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
                       
                       # Concatenates SBATCH option to case statement
                       args <- c(args, case)
                       
                       writeLines(args, private$scriptPath)
                   }, 
                   
                   #' Creates job ids of the form masterId_1 ... masterId_n
                   createJobIds = function() {
                       private$jobIdAll <- paste0(private$jobId, "_", 1:self$length())
                   },
                   
                   #' Creates job names of the form jobName_1 ... jobName_n
                   createJobNames = function() {
                       private$jobNameAll <- paste0(private$jobName, "_", 1:self$length())
                   }
                       
                   
                   #' Throws error if i is out of bounds in job list
                   
                   )
)

