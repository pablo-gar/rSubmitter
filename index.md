# rSubmitter - R and SLURM!
### An interface between R and a SLURM cluster

rSubmmiter is a package that allows simple communication between R and a SLURM cluster to achieve three main tasks:
1. Easy submission, monitoring and management of individual cluster jobs.
2. Easy and fast submission of many jobs, by implementing SLURM arrays.
3. Seamlessly lapply (loop) parallelization.

### Installation
Currently you can only install rSubmitter via the R devtools package:
```r
library("devtools")
install_github("pablo-gar/rSubmitter")
```

### Usage

- [**quick-start** guide](https://pablo-gar.github.io/rSubmitter/articles/quick_start.html)

- Documention of the main `rSubmitter` functions [online](https://pablo-gar.github.io/rSubmitter/reference/index.html#section-main-functions):
   [Job]("https://pablo-gar.github.io/rSubmitter/reference/Job.html")
   [JobArray]("https://pablo-gar.github.io/rSubmitter/reference/JobArray.html")
   [superApply]("https://pablo-gar.github.io/rSubmitter/reference/SuperApply.html")

- Documention of the main `rSubmitter` functions from R:
```r
library("rSubmitter")
?Job
?JobArray
?superApply
```
