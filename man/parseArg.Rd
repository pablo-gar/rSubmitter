% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{parseArg}
\alias{parseArg}
\title{Converts string into vector using a separator}
\usage{
parseArg(x, sep, trim = T)
}
\arguments{
\item{x}{String - if a vector only first element will be considred}

\item{sep}{String - if vector only first element will be considered}

\item{trim}{Logical - if TRUE white spaces will be eliminated}
}
\value{
Character vector - with elements as characters after splitting by sep
}
\description{
Takes a string an creates a vector using sep as the separator
}
\examples{
parseArg("A, B, C", sep = ",")
}
