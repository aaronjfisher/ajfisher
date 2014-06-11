\name{mappal}
\alias{mappal}
\title{Mapping color palettes to values}
\usage{
mappal(x, col_pal = rev(sequential_hcl(50)), type = "div",
  max_abs_x = max(abs(x)), interp_x = FALSE)
}
\arguments{
  \item{x}{vector of values for which color should be
  mapped to. Can also be a matrix, if using image(). In
  this case, see the \code{interp_x} parameter.}

  \item{col_pal}{the color palette to be mapped to the
  vector. The head of the vector should correspond to lower
  values of x, and the tail of the vector should correspond
  to higher values of x. If a diverging color palette is
  being used, the "zero" color should be the middle element
  of this vector.}

  \item{type}{describes the \code{col_pal} vector entered.
  Can be either 'div' for diverging, 'qual' for
  qualitative, or 'seq' for sequential.}

  \item{max_abs_x}{for use if several plots are being
  created, and color scales should be consistent across all
  plots. Set max_abs_x to the maximum absolute value of all
  elements being plotted. See example below.}

  \item{interp_x}{for use in \code{image()} style plots,
  where the color vector required is not a direct mapping.
  If set to FALSE, the color is mapped directly to the x
  elements. See examples below.}
}
\value{
a color vector where the maximum absolute value of \code{x}
corresponds to the highest intensity color, and all other
colors are proportional. If a diverging palette is used and
\code{abs(max(x[x>0]))} >> \code{abs(min(x[x<0]))}, then no
negative entry will be as intensely colored as the maximum
positive value of \code{x} (and visa versa).
}
\description{
Best used with color palettes generated from the colorspace
package, using \code{sequential_hcl, diverge_hcl, or
rainbow_hcl}.
}
\examples{
palQual<-rainbow_hcl(5)
palSeq<-sequential_hcl(40)[40:1]
palDiv<-diverge_hcl(40)[40:1]
par(mfrow=c(3,1))
pal(palQual); pal(palSeq); pal(palDiv)

library(MASS)

n<-600
set.seed(900)
Sigma<-matrix(c(
	1,0,.5,
	0,1,.8,
	.5,.8,2), nrow=3)
X<-mvrnorm(n,mu=c(0,1.2,0),Sigma=Sigma)

par(mfrow=c(1,3))
plot(X[,1],X[,2],pch=19,col=mappal(X[,1],palDiv),main='X[,1] mapped to color')
plot(X[,1],X[,2],pch=19,col=mappal(X[,2],palDiv),main='X[,2] mapped to color')
plot(X[,1],X[,2],pch=19,col=mappal(X[,3],palDiv),main='X[,3] mapped to color')

plot(X[,1],X[,2],pch=19,col=mappal(X[,3],palQual))
plot(X[,1],X[,2],pch=19,col=mappal(X[,3],palSeq))
plot(X[,1],X[,2],pch=19,col=mappal(X[,3],palDiv))

#########
# image() example

#sample generating code adapted from pp() function at:
#http://docs.ggplot2.org/0.9.3.1/geom_tile.html
ppMat <- function (n,r=4) {
 x <- seq(-r*pi, r*pi, len=n)
 mat<-matrix(NA,n,n)
 for(i in 1:n){
 	for(j in 1:n){
 		r <- sqrt(x[i]^2 + x[j]^2)
 		mat[i,j]<-cos(r^2)*exp(-r/6)
 }}
 mat
}

x<-ppMat(100)+.25

par(mfrow=c(1,2),mar=c(3,2,3,4))

#with mappal()
mappedColImage<-mappal(x,palDiv,interp_x=TRUE)
image(x,col=mappedColImage,main='With mappal(),\\ngray=zero')
color.legend(xl=1.02, yb=0,xr=1.07,yt=1,legend=signif(range(x),digits=2), rect.col=mappedColImage,  align='rb', gradient='y')

#without mappal()
image(x,col=palDiv,main='Without mappal(),\\nred=zero, gray>zero')
color.legend(xl=1.02, yb=0,xr=1.07,yt=1,legend=signif(range(x),digits=2), rect.col=palDiv,  align='rb', gradient='y')
}

