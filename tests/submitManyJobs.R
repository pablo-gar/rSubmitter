jobs <- list()
for(i in 1:200) {
    
    flush.console()
    print(i)
    
    jobs[[i]] <- Job$new(c("echo hola", "sleep 60"), jobName = paste0("test", i), outDir = "~", time = "02:00", mem = "1G", proc = 1)
    jobs[[i]]$submit()
}
