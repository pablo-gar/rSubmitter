# rSubmitter 
### An interface between R and a SLURM cluster

rSubmmiter is a package that allows simple communication between R and a SLURM cluster to achieve three main tasks:
1. Easy submission, monitoring and management of individual cluster jobs.
2. Easy and fast submission of many jobs, by implementing SLURM arrays.
3. Seamlessly lapply (loop) parallelization.

### Installation
Currently rSubmitter can only be installed via the R devtools package:
```r
library("devtools")
install_github("pablo-gar/rSubmitter")
```

### Usage
Online documentation can be found [here], or you can find a **quick-start** guide [here](https://pablo-gar.github.io/rSubmitter/inst/doc/tutorial_basicUse.html)

You can also access the full documention of the main `rSubmitter` functions from R:
```r
library("rSubmitter")
?Job
?JobArray
?superApply
```
