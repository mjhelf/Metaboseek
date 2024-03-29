% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getset_featureTables.R
\name{FeatureTable}
\alias{FeatureTable}
\alias{FeatureTable.reactivevalues}
\alias{FeatureTable<-}
\alias{FeatureTable<-.data.frame}
\alias{FeatureTable<-.MseekFT}
\title{FeatureTable}
\usage{
FeatureTable(x, ...)

\method{FeatureTable}{reactivevalues}(x, update = F, tableID = NULL)

FeatureTable(x, ...) <- value

\method{FeatureTable}{data.frame}(x, replace = F, update = T, tableID = NULL) <- value

\method{FeatureTable}{MseekFT}(x, replace = T, tableID = NULL) <- value
}
\arguments{
\item{x}{the \code{values} object}

\item{...}{additional arguments passed to S3 methods
Getter and setter methods to retrieve or modify a Feature Table 
(\code{MseekFT} object) from \code{values}}

\item{update}{if TRUE, update table via \code{\link{updateFT}()} before 
getting its values}

\item{tableID}{table ID to retrieve. will use currently active Feature Table if not set.
For setter, will create a new Feature Table under this ID if ID does not exist, 
otherwise will override or update the selected Feature Table}

\item{value}{an \code{MseekFT} or \code{data.frame} object}

\item{replace}{replace the current Feature table with the given tableID? 
If FALSE, will update instead.#' @return An \code{MseekFT} object}
}
\description{
FeatureTable
}
