% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eseis-package.R
\docType{data}
\name{earthquake}
\alias{earthquake}
\alias{s}
\alias{t}
\title{Seismic traces of a small earthquake}
\format{
The format is: 
List of 3
 $ BHE: num [1:8001] -3.95e-07 ...
 $ BHN: num [1:8001] -2.02e-07 ...
 $ BHZ: num [1:8001] -1.65e-07 ...

The format is: POSIXct[1:98400], format: "2015-04-06 13:16:54" ...
}
\usage{
s

t
}
\description{
The dataset comprises the seismic signal (all three components) of 
a small earthquake. The data have been recorded at 200 Hz sampling 
frequency with an Omnirecs Cube ext 3 data logger.

The dataset comprises the time vector associated with the data set
\code{earthquake}.
}
\examples{

## load example data set
data(earthquake)

## plot signal vector
plot(x = t, y = s$BHZ, type = "l")


## load example data set
data(earthquake)

## show range of time vector
range(t)

}
\keyword{datasets}
