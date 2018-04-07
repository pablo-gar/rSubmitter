commands <- list()

for(i in 1:800) commands[[i]] <- c("echo adios","sleep 40")
                 
# Normal pipeline
jobArray <- JobArray$new(commands, jobName = "test", outDir = "~", mem = "1G", time = "02:00", proc = 1)
jobArray$submit()
jobArray$getState()
jobArray$wait()

commands <- list()

for(i in 1:1100) commands[[i]] <- c("echo adios","sleep 40")
jobArray <- JobArray$new(commands, jobName = "test", outDir = "~", mem = "1G", time = "02:00", proc = 1)
jobArray$submit()
jobArray$getState()
jobArray$wait(stopIfFailed = T)



