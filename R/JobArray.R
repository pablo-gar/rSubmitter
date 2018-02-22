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
                       
    classname = "Job",
    public = list(
                  #INSTANCES
                  
                  #METHODS
                  initialize = function() {
                      return()
                  },
                  
                  addJob = function(...) {
                      private$jobs = c(private$jobs, Job$new(...))
                      return(invisible(self))
                  },
                  
                  getJob = function(i) {
                      private$checkIndex(i)
                      return(invisible(private$jobs[[i]]))
                  },
                  
                  #' Submit one job by index
                  submit = function(i) {
                      private$checkIndex(i) 
                      
                      private$jobs[[i]]$submit()
                      return(invisible(self))
                  },
                  
                  #' Submit all jobs
                  submitAll = function() {
                      return()
                  },
                  
                  #' Wait for one job by index
                  waitForJob = function(i) {
                      return()
                  },
                  
                  #' Wait for all jobs
                  waitForAll = function(i) {
                      return()
                  },
                  
                  #' Get job id by index
                  getJobId = function(i) {
                      return()
                  },
                  
                  #' Get all job ids
                  getAllJobIds = function(i) {
                      return()
                  },
                  
                  #' Get job name by index
                  getJobName = function(i) {
                      return()
                  },
                  
                  #' Get all job names
                  getAllJotNames = function(i) {
                      return()
                  }
                  
                  
                  ),
    private = list(
                   #INSTANCES
                   jobs = list(),
                   states = vector(),
                   jobNames = vector(),
                   jobIds = vector(),
                   
                   #METHODS
                   
                   #' Throws error if i is out of bounds in job list
                   checkIndex = function(i) {
                       if(i > length(private$jobs))
                           stop("Index out of bonds in job array")
                   }
                   
                   )
)

