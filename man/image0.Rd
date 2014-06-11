\name{image0}
\alias{image0}
\title{Reorients an image for plotting}
\usage{
image0(x, col = mappal(x, col_pal = rev(diverge_hcl(50)), type = "div",
  interp_x = TRUE), autoLegend = FALSE, ...)
}
\arguments{
  \item{x}{matrix to be plotted}

  \item{col}{passed to image}

  \item{autoLegend}{tells whether an automatic legend bar
  for color should be generated. If the matrix plotted is
  faily high dimensional, \code{autoLegend=TRUE} will
  generally work well. If set to FALSE, it can be useful to
  type \code{image0} into the console to quickly see a
  sample code entry for \code{color.legend}.}

  \item{...}{passed to \code{image} and
  \code{color.legend}}
}
\value{
By default, \code{image0()} uses a red and blue diverging
color palette from the \code{colorspace} package, and
builds a pallete using \code{mappal}.
}
\description{
Reorients an image for plotting
}
\examples{
#Setup
palBw<-sequential_hcl(50,c.=c(0,0))#black and white color palette

#fun Mario example
data(marioMat)

par(mfrow=c(1,2))
image(marioMat,col=palBw)  #wrong orientation
image0(marioMat,col=palBw,main="It's-a Me!",autoLegend=TRUE) #right orientation

#simple example
x<-matrix(1:4,2,2)
x

par(mfrow=c(1,2))
image(x,col=palBw)  #wrong orientation
image0(x,col=palBw) #right orientation.

#with default colors
image0(x)
}

