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

