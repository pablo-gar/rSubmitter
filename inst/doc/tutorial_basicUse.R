## ----eval=FALSE----------------------------------------------------------
#  library("rSubmmiter")
#  myJob <- Job$new(commandVector = c("echo hola world!", "sleep 30"))

## ----eval=FALSE----------------------------------------------------------
#  myJob$submit()
#  myJob$wait()
#   2018-04-23 18:22:35 --- Cluster Status |  PENDING = 1 |
#   2018-04-23 18:22:40 --- Cluster Status |  RUNNING = 1 |
#   2018-04-23 18:23:30 --- Cluster Status |  COMPLETED = 1 |

