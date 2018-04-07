job <- Job$new(c("echo hola", "sleep 30"), jobName = "test", outDir = "~", time = "01:00", mem = "1G", proc = 1)
job$submit()
