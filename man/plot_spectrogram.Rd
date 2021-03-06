% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_spectrogram.R
\name{plot_spectrogram}
\alias{plot_spectrogram}
\title{Plot spectrograms (power spectral density estimates)}
\usage{
plot_spectrogram(data, legend = FALSE, keep_par = FALSE, agg = c(1, 1), ...)
}
\arguments{
\item{data}{\code{List} object, spectrogram to be plotted. Must be output
of \code{signal_spectrogram} or of equivalent structure.}

\item{legend}{\code{Logical} value, option to add colour bar legend. Legend
label can be changed by \code{zlab}.}

\item{keep_par}{\code{Logical} value, option to omit resetting plot 
parameters after function execution. Useful for adding further data to the 
PSD plot. Default is \code{FALSE} (parameters are reset to original values).}

\item{agg}{\code{Integer} vector of length two, factors of image 
aggregation, i.e. in time and frequency dimension. Useful to decrease 
image size. Default is \code{c(1, 1)} (no aggregation).}

\item{\dots}{Additional arguments passed to the plot function.}
}
\value{
Graphic output of a spectrogram.
}
\description{
This function plots spectrograms of seismic signals. It uses the output 
of \code{signal_spectrogram}.
}
\examples{

## load example data set
data(rockfall)

## deconvolve signal
rockfall <- signal_deconvolve(data = rockfall_eseis)

## calculate spectrogram
PSD <- signal_spectrogram(data = rockfall)

## plot spectrogram
plot_spectrogram(data = PSD)

## plot spectrogram with legend and labels in rainbow colours
plot_spectrogram(data = PSD, 
                 xlab = "Time (min)", 
                 ylab = "f (Hz)", 
                 main = "Power spectral density estimate", 
                 legend = TRUE, 
                 zlim = c(-220, -70),
                 col = rainbow(100))

                     
}
\seealso{
\code{\link{signal_spectrogram}}
}
\author{
Michael Dietze
}
\keyword{eseis}
