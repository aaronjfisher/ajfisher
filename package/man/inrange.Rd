\name{inrange}
\alias{inrange}
\title{Check if x is an interval}
\usage{
  inrange(x, l = 0, u = 1, strict_lower = FALSE,
    strict_upper = FALSE)
}
\arguments{
  \item{x}{value of interest}

  \item{l}{lower bound of the interval}

  \item{u}{upper bound of the interval}

  \item{strict_lower}{if \code{TRUE}, x==l will result in x
  being classified as not in the interval}

  \item{strict_upper}{if \code{TRUE}, x==u will result in x
  being classified as not in the interval}
}
\description{
  Check if x is an interval
}
\examples{
inrange(5,l=5,u=8)
inrange(5,l=5,u=8,strict_lower=TRUE)
}

