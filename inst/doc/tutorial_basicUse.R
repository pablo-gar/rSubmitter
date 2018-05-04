## ----eval=FALSE----------------------------------------------------------
#  library("rSubmitter")
#  myJob <- Job$new(commandVector = c("echo hola world!", "sleep 30"))
#  myJob$submit()
#  myJob$wait()
#  #  2018-04-23 18:22:35 --- Cluster Status |  PENDING = 1 |
#  #  2018-04-23 18:22:40 --- Cluster Status |  RUNNING = 1 |
#  #  2018-04-23 18:23:30 --- Cluster Status |  COMPLETED = 1 |

## ----eval=FALSE----------------------------------------------------------
#  library("rSubmitter")
#  commands <- list(c( # First Job
#                      "echo hola",
#                      "sleep 20"
#                     ),
#                   c( # Second Job
#                     "echo adios",
#                     "sleep 60"
#                   )
#                 )
#  
#  jobArray <- JobArray$new(commandList = commands)
#  jobArray$submit()
#  jobArray$wait()
#  #   2018-04-25 17:49:30 --- Cluster Status |  PENDING = 2 |
#  #   2018-04-25 17:49:45 --- Cluster Status |  RUNNING = 2 |
#  #   2018-04-25 17:50:50 --- Cluster Status |  COMPLETED = 2 |

## ----eval=FALSE----------------------------------------------------------
#  x <- lapply(1:4, as.character)

## ----eval=FALSE----------------------------------------------------------
#  x <- list()
#  for(i in 1:4)
#      x[[i]] <- as.character(i)

## ----eval=FALSE----------------------------------------------------------
#  myFun <- function(x) {
#      return(rep(x, 3))
#  }
#  
#  dir.create("~/testSap")
#  x <- superApply(1:100, myFun, tasks = 4)
#  #   2018-05-04 15:29:34 Partitioning function calls
#  #   2018-05-04 15:29:35 Submmiting parallel Jobs
#  #   2018-05-04 15:29:40 --- Cluster Status |  PENDING = 4 |
#  #   2018-05-04 15:29:45 --- Cluster Status |  COMPLETED = 4 |
#  #   2018-05-04 15:29:45 Merging parellel results
#  #   2018-05-04 15:29:45 Merge done
#  #   2018-05-04 15:29:46 Cleaning partitioned data
#  #   2018-05-04 15:29:47 Cleaning done

## ----eval=FALSE----------------------------------------------------------
#  myJob$submit()
#  myJob$wait()
#    2018-04-23 18:22:35 --- Cluster Status |  PENDING = 1 |
#    2018-04-23 18:22:40 --- Cluster Status |  RUNNING = 1 |
#    2018-04-23 18:23:30 --- Cluster Status |  COMPLETED = 1 |

## ----eval=FALSE----------------------------------------------------------
#  myJob$submit()$wait()

## ----eval=FALSE----------------------------------------------------------
#  myJob$submit()
#  myJob$cancel()
#    2018-04-23 19:17:30 Cancelling 1 job(s)
#    2018-04-23 19:17:32 Finished sending cancel signal

## ----eval=FALSE----------------------------------------------------------
#  myJob <- Job$new(commandVector = c("echo hola world!", "sleep 30"), jobName = "testJob", outDir = "/tmp/")
#  myJob$submit()$wait()
#  myJob$clean()

