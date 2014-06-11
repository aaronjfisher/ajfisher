\name{fast.color.legend}
\alias{fast.color.legend}
\title{Get quick and dirty color legend near the x axis}
\usage{
fast.color.legend(x, y, z, col = mappal(z, rev(diverge_hcl(50)), interp_x =
  TRUE), lab = "", stretch = 1, spacing = 0.05, digits = 2, ...)
}
\arguments{
  \item{x}{variable on horizontal axis.}

  \item{y}{variable on virtical axis.}

  \item{z}{variable mapped to color.}

  \item{stretch}{factor to expand the height of the
  legend.}

  \item{spacing}{distance from legend to the right border
  of plot.}

  \item{digits}{number of significant digits used in legend
  labels.}

  \item{...}{passed to \code{\link{color.legend}}.}
}
\description{
Get quick and dirty color legend near the x axis
}

