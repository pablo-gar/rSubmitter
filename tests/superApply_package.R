myFun <- function(x) {
    return(ggplot(data.frame(x = 1:100, y = (1:100)*x), aes(x = x, y = y )) + geom_point() + ylim(0, 1e4))
}

dir.create("~/testSap")
sapOut <- superApply(1:100, FUN = myFun, tasks = 4, workingDir = "~/testSap", packages = "ggplot2", extraBashLines = "module load R/3.4.0", time = "60", mem = "1G")

