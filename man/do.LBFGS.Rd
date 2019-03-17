\name{do.LBFGS}
\alias{do.LBFGS}
\alias{do_lbfgs}
\title{L-BFGS Optimization for Unfolding}
\description{Performs unfolding with L-BFGS algorithm }
\usage{
do.LBFGS(obj)
}
\arguments{
  \item{obj}{The output from \code{bu.Preprocess}.}
}
\value{
  A list of values that can be handed to the \code{do.Slice} function. 
}
