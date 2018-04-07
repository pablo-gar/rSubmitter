myFun <- function(x) {
    #Sys.sleep(10)
    return(rep(x, 3))
}

dir.create("~/testSap")
sapOut <- superApply(1:100, FUN = myFun, tasks = 4, workingDir = "~/testSap", extraBashLines = "module load R/3.4.0", time = "60", mem = "1G")
sapOut <- superApply(1:100, FUN = myFun, tasks = 100, workingDir = "~/testSap", extraBashLines = "module load R/3.4.0", time = "60", mem = "1G", partition = "hbfraser,owners,normal,hns")
