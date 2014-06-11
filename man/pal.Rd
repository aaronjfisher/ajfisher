\name{pal}
\alias{pal}
\title{Show a color palette preview (from colorspace package vignette)}
\usage{
pal(col, border = "light gray", ...)
}
\arguments{
  \item{col}{a color palette to plot.}

  \item{border}{color of border separating elements of the
  palette.}

  \item{...}{passed to \code{link{plot}}.}
}
\description{
Copy pasted from HCL-Based Color Palettes in R
http://cran.r-project.org/web/packages/colorspace/vignettes/hcl-colors.pdf
}
\examples{
pal(rainbow_hcl(50,c=100,l=80))
pal(sequential_hcl(50))
}

