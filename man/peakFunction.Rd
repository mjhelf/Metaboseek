% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Functions_extract_EICs.R
\name{peakFunction}
\alias{peakFunction}
\title{peakFunction}
\usage{
peakFunction(x, theta)
}
\arguments{
\item{x}{numeric vector}

\item{theta}{a numeric(4), see \code{details}}
}
\value{
a series of values approximately representing a peak shape along x
}
\description{
Function to represent a peak in peakFitter
}
\details{
:
\itemize{
\item \code{theta[1]}: numeric value of the expected/initial position of the peak apex (in range of x)
\item \code{theta[2]}: numeric value of the expected/initial peak width. Should not be 0!
\item \code{theta[3]}: y-scale factor
\item \code{theta[4]}: shift along y-axis
}
}
