\name{set.Globals}
\alias{set.Globals}
\alias{set_globals}
\title{Set global values for other functions.}
\description{Sets global values for other C functions to use. }
\usage{
set.Globals(obj, nburn = 1000, nslice = 2000, NS = 2, UNFOLD = 1, NMISSING = 7)
}
\arguments{
  \item{obj}{Output from \code{bu.Preprocess}.}
  \item{nburn}{Number of burnin iterations.}
  \item{nslice}{Number of saved iterations from the slice sampler (total iterations is \code{nburn}+\code{nslice})}
  \item{NS}{Number of Dimensions}
  \item{UNFOLD}{Not sure.}
  \item{NMISSING}{Number of non-missing observations required for fitting}
}
