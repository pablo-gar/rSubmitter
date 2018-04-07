customRep <- function(x) {
    return(paste("customFunction", rep(x, 3)))
}

myFun <- function(x) {
    return(customRep(x))
}

dir.create("~/testSap")
sapOut <- superApply(1:100, FUN = myFun, tasks = 4, workingDir = "~/testSap", extraBashLines = "module load R/3.4.0", time = "60", mem = "1G")

