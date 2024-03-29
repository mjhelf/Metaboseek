% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Functions_plotting_genericData.R
\name{assignColor}
\alias{assignColor}
\title{assignColor}
\usage{
assignColor(
  datarange,
  colscale,
  center = NULL,
  manualRange = NULL,
  NAcolor = "#FFFFFF"
)
}
\arguments{
\item{datarange}{the data range for the legend}

\item{colscale}{character vector of colors}

\item{center}{force the middle of the color vector to correspond to this value}

\item{manualRange}{enter a range to fit the color range to, if not range(datarange)}

\item{NAcolor}{color to assign to NA values}
}
\value{
a character vector of same length as \code{datarange} with color values
}
\description{
Assign a color from a range of colors to all values in a numeric vector (datarange)
}
