% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aux_obspyeseis.R
\name{aux_obspyeseis}
\alias{aux_obspyeseis}
\title{Convert ObsPy object to eseis object}
\usage{
aux_obspyeseis(data, simplify = TRUE)
}
\arguments{
\item{data}{\code{obspy} stream object, \code{list} element, created by 
running ObsPy through R using the package 'reticulate'.}

\item{simplify}{\code{Logical} value, option to simplify output
when possible. This basically means that if there is only trace object 
in the ObsPy stream, the list object will have one level less. Default is 
\code{TRUE}.}
}
\value{
\code{eseis} object.
}
\description{
The function converts an ObsPy stream object to an eseis object. The 
functionality is mainly useful when running ObsPy through R using the 
package 'reticulate'.
}
\details{
Initiation of the reticulate-based python toolbox support can be 
cumbersome. The following suggestions from Guthub 
(https://github.com/rstudio/reticulate/issues/578) helped in a case 
study:

\code{library(reticulate)}
\code{use_condaenv("r-reticulate")}
\code{py_install("obspy", pip = TRUE)}
}
\examples{

\dontrun{

## load ObsPy library with package 'reticulate'
## (requires ObsPy to be installed on the computer)
obspy <- reticulate::import("obspy")

## set seismic data directory
dir_data <- paste0(system.file("extdata", package="eseis"), "/")

## read miniseed file to stream object via ObsPy
x <- obspy$read(pathname_or_url = "dir_data/2017/99/RUEG1.17.99.00.00.00.BHZ.SAC")

## convert ObsPy stream object to eseis object
y <- aux_obspyeseis(data = x)

## plot eseis object
plot_signal(y)
}

}
\author{
Michael Dietze
}
\keyword{eseis}
