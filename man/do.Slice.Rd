\name{do.Slice}
\alias{do.Slice}
\alias{do_sliceu}
\title{Slice Sampler for Bayesian Unfolding}
\description{
Implements a slice sampler for the Bayesian multidimensional unfolding algorithm.  }
\usage{
do.Slice(obj, nburn = 1000, nslice = 2000, NS = 2, UNFOLD = 1, NMISSING = 7, rotmat = NULL)
}
\arguments{
  \item{obj}{Output from \code{do.LBFGS}.}
  \item{nburn}{Number of burnin iterations.}
  \item{nslice}{Number of saved iterations from the slice sampler (total iterations is \code{nburn}+\code{nslice})}
  \item{NS}{Number of Dimensions}
  \item{UNFOLD}{Not sure.}
  \item{NMISSING}{Number of non-missing observations required for fitting}
  \item{rotmat}{An optional rotation matrix to multiply the L-BFGS coordinates by before passing them to the slice sampler.}
}
