commands <- list(c( # Fist job -- will fail
                      "foo",
                      "echo This should not print",
                      "sleep 20" 
                   ),
                 c( # Second job -- will fail too
                    "foo2",
                    "sleep 20"
                   ),
                 c( # Third job
                    "echo hola",
                    "sleep 20"
                   ),
                 c( # Fourth Job
                   "echo adios",
                   "sleep 60"
                 )
               )
                 
# Normal pipeline - it should run but throw 2 warnings
jobArray <- JobArray$new(commands, jobName = "test", outDir = "~", mem = "1G", time = "02:00", proc = 1)
jobArray$submit()
jobArray$getState()
jobArray$wait()
jobArray$clean()

# Should stop because two job fail
jobArray <- JobArray$new(commands, jobName = "test", outDir = "~", mem = "1G", time = "02:00", proc = 1)
jobArray$submit()
jobArray$getState()
jobArray$wait(stopIfFailed = T)



