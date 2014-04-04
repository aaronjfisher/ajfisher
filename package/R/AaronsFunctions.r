#' Picture of Mario doing an SVD with only his head
#'
#' A sample image for testing out image0 plotting. The jpeg is from 
#' (http://aaronjfisher.github.io).
#' At the time of this write-up, this image is located under the CV page (http://aaronjfisher.github.io/pages/cv.html) 
#'
#' @name marioMat.rda
#' @docType data
#' @keywords data
NULL


#' Make a new window for plotting (on mac OR windows)
#' @export
disp<-function(){ #make new window, cross mac v pc
  if(.Platform$OS.type=="windows") windows()
  else quartz()
}



#' Get number of unique elements
#'
#' @param x object of interest
#' @return \code{length(unique(x))}
#' @export
#' @examples
#' x<-c(1,2,3,3)
#' lenUni(x)
lenUni<-function(x) length(unique(x))

#' Check if x is an interval
#'
#' @param x value of interest
#' @param l lower bound of the interval
#' @param u upper bound of the interval
#' @param strict_lower if \code{TRUE}, x==l will result in x being classified as not in the interval
#' @param strict_upper if \code{TRUE}, x==u will result in x being classified as not in the interval
#' @export
#' @examples
#' inrange(5,l=5,u=8)
#' inrange(5,l=5,u=8,strict_lower=TRUE)
inrange<-function(x,l=0,u=1,strict_lower=FALSE,strict_upper=FALSE){
	inval<-FALSE
	if(x<=u & x>=l) inval<-TRUE
	if(x==u & strict_upper) inval<-FALSE
	if(x==l & strict_lower) inval<-FALSE
	return(inval)
}


#' Code snippet for making progress bars
#'
#' I always forget the functions for making progress bars in loops. This function reminds me!
#' @return sample code for writing a progress bar
#' @export
#' @examples
#' progCode()
#' #type "progCode" to see the code itself
progCode<-function(){
	maxIndex<-100 
	pb<-txtProgressBar(min = 1, max = maxIndex,  char = "=", style = 3) 
	for(i in 1:maxIndex){ 
		setTxtProgressBar(pb,i)
	}
}



#' Reorients an image for plotting
#'
#' @param x matrix to be plotted
#' @param col passed to image
#' @param colIsPal if TRUE, color is taken as a pallete that should be adjusted with mappal().
#' @param autoLegend tells whether an automatic legend bar for color should be generated. If the matrix plotted is faily high dimensional, \code{autoLegend=TRUE} will generally work well. If set to FALSE, it can be useful to type \code{image0} into the console to quickly see a sample code entry for \code{color.legend}.
#' @param ... passed to \code{image} and \code{color.legend}
#' @return By default, \code{image0()} uses a red and blue diverging color palette from the \code{colorspace} package, and builds a pallete using \code{mappal}.
#' @export
#'
#' @examples
#' #Setup
#'palBw<-sequential_hcl(50,c.=c(0,0))#black and white color palette
#'
#' #fun Mario example
#' data(marioMat)
#'
#' par(mfrow=c(1,2))
#' image(marioMat,col=palBw)  #wrong orientation
#' image0(marioMat,col=palBw,main="It's-a Me!",autoLegend=TRUE) #right orientation
#'
#' #simple example
#' x<-matrix(1:4,2,2)
#' x
#'
#' par(mfrow=c(1,2))
#' image(x,col=palBw)  #wrong orientation
#' image0(x,col=palBw) #right orientation. 
#'
#' #with default colors
#' image0(x)
image0<-function(x,col=mappal(x,pal_vec=rev(diverge_hcl(50)),type='div',interp_x=TRUE),autoLegend=FALSE,...){
	image(t(x[nrow(x):1,]),col=col,...)
	if(autoLegend){
		color.legend(xl=1.02, yb=0,xr=1.07,yt=1,legend=signif(range(x),digits=2), rect.col=col, align='rb',gradient='y',...)
	}
}
#!!!??? passing ... twice is making some stuff weird? It always passes everything to everything?





#' Show a color palette preview (from colorspace package vignette)
#'
#' Copy pasted from HCL-Based Color Palettes in R
#' http://cran.r-project.org/web/packages/colorspace/vignettes/hcl-colors.pdf
#' @export
#' @examples
#' pal(rainbow_hcl(50,c=100,l=80))
#' pal(sequential_hcl(50))
pal <- function(col, border = "light gray", ...)
 {
 n <- length(col)
 plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1),
 axes = FALSE, xlab = "", ylab = "", ...)
 rect(0:(n-1)/n, 0, 1:n/n, 1, col = col, border = border)
 }





#' helper function for \code{mappal}
#' 
#' This is primarily a helper function for \code{mappal}. Users should directly call \code{mappal} rather than calling \code{mappal_abs}.
mappal_abs<-function(x,pal_vec,max_abs_x=max(abs(x)),type='div'){
	n<-length(pal_vec)
	x<-abs(x)
	xCut<-cut(c(max_abs_x,x),breaks=n)[-1]
	return(pal_vec[xCut])
}
#palette should be entered going from "zero" to "max" color intensity for x entries with highest abs values
#use max_abs_x input so that the max_abs_x *would* be colored with the most extreme color in the palette, if that max_abs_x is attained.





#what if I want constant coloring across multiple plots? like ggplot?
	#use max_abs_x
#for using image, set interp_x=TRUE
#maps a variable (with pos or neg values) to a possibly diverging color palette
# for diverging pallets, the "zero" color should be in the middle, neg at the head, pos at the tail

#' Mapping color palettes to values
#'
#' Best used with color palettes generated from the colorspace package, using \code{sequential_hcl, diverge_hcl, or rainbow_hcl}.
#' @param x vector of values for which color should be mapped to. Can also be a matrix, if using image(). In this case, see the \code{interp_x} parameter.
#' @param pal_vec the color palette to be mapped to the vector. The head of the vector should correspond to lower values of x, and the tail of the vector should correspond to higher values of x. If a diverging color palette is being used, the "zero" color should be the middle element of this vector.
#' @param type describes the \code{pal_vec} vector entered. Can be either 'div' for diverging, 'qual' for qualitative, or 'seq' for sequential.
#' max_abs_x for use if several plots are being created, and color scales should be consistent across all plots. Set max_abs_x to the maximum absolute value of all elements being plotted. See example below.
#' @param interp_x for use in \code{image()} style plots, where the color vector required is not a direct mapping. If set to FALSE, the color is mapped directly to the x elements. See examples below.
#'
#' @return a color vector where the maximum absolute value of \code{x} corresponds to the highest intensity color, and all other colors are proportional. If a diverging palette is used and \code{abs(max(x[x>0]))} >> \code{abs(min(x[x<0]))}, then no negative entry will be as intensely colored as the maximum positive value of \code{x} (and visa versa).
#'
#' @export
#'
#' @examples
#' palQual<-rainbow_hcl(5)
#' palSeq<-sequential_hcl(40)[40:1]
#' palDiv<-diverge_hcl(40)[40:1]
#' par(mfrow=c(3,1))
#' pal(palQual); pal(palSeq); pal(palDiv)
#'
#' library(MASS)
#'
#' n<-600
#' set.seed(900)
#' Sigma<-matrix(c(
#' 	1,0,.5,
#' 	0,1,.8,
#' 	.5,.8,2), nrow=3)
#' X<-mvrnorm(n,mu=c(0,1.2,0),Sigma=Sigma)
#'
#' par(mfrow=c(1,3))
#' plot(X[,1],X[,2],pch=19,col=mappal(X[,1],palDiv),main='X[,1] mapped to color')
#' plot(X[,1],X[,2],pch=19,col=mappal(X[,2],palDiv),main='X[,2] mapped to color')
#' plot(X[,1],X[,2],pch=19,col=mappal(X[,3],palDiv),main='X[,3] mapped to color')
#'
#' plot(X[,1],X[,2],pch=19,col=mappal(X[,3],palQual))
#' plot(X[,1],X[,2],pch=19,col=mappal(X[,3],palSeq))
#' plot(X[,1],X[,2],pch=19,col=mappal(X[,3],palDiv))
#'
#' #########
#' # image() example
#'
#' #sample generating code adapted from pp() function at:
#' #http://docs.ggplot2.org/0.9.3.1/geom_tile.html
#' ppMat <- function (n,r=4) {
#'  x <- seq(-r*pi, r*pi, len=n)
#'  mat<-matrix(NA,n,n)
#'  for(i in 1:n){
#'  	for(j in 1:n){
#'  		r <- sqrt(x[i]^2 + x[j]^2)
#'  		mat[i,j]<-cos(r^2)*exp(-r/6)
#'  }}
#'  mat
#' }
#'
#' x<-ppMat(100)+.25
#' image(volcano,mappal(x))
mappal<-function(x,pal_vec=rev(sequential_hcl(50)),type='div',max_abs_x=max(abs(x)),interp_x=FALSE){
	
	if(interp_x) x<-seq(min(x),max(x),len=100)

	if(type=='qual'){
		return(pal_vec[cut(x,breaks=length(pal_vec))])
	}

	if(type=='seq') return(mappal_abs(x,pal_vec,max_abs_x=max_abs_x))

	#Else, if type=='div' (rest of this function)

	xNegInd <- which(x<0)
	xPosInd <- which(x>=0)
	lPal<-length(pal_vec)
	zeroInd<-ceiling(lPal/2)

	#object to store output
	#color the pos and negative parts separately, but for each, use max(abs(all of x)) as the most intense a color can be.
	out<-rep(NA,length(x))	

	if(length(xNegInd)>0)
		out[xNegInd]<-mappal_abs(x[xNegInd],pal_vec[zeroInd:1],max_abs_x=max_abs_x) #reverse order so it goes from low to high intesity (from zero color to high color)
	if(length(xPosInd)>0)
		out[xPosInd]<-mappal_abs(x[xPosInd],pal_vec[zeroInd:lPal],max_abs_x=max_abs_x)

	return(out)
}



ci<-function(model,Bnumber){
	summary(model)$coef[Bnumber,1]+c(-1,1)*1.96*summary(model)$coef[Bnumber,2]
}



#              _      _       _           _ 
#             | |    | |     | |         | |
#   ___  _   _| |_ __| | __ _| |_ ___  __| |
#  / _ \| | | | __/ _` |/ _` | __/ _ \/ _` |
# | (_) | |_| | || (_| | (_| | ||  __/ (_| |
#  \___/ \__,_|\__\__,_|\__,_|\__\___|\__,_|
                                          
                                          

#NOW USING mappal instead!
c.r.hcl<-function(x,n=1000, ...){ #cut rainbow_hcl
	xCut<-cut(x,breaks=n)
	colors<-rainbow_hcl(n=n, ...)
	out<-colors[xCut]
	return(out)
}
c.s.hcl<-function(x,n=1000,only.up.to=n, ...){ #cut sequential_hcl
	xCut<-1 #these two lines guard against errors caused when this function is piped into c.d.hcl
	if(only.up.to>1) xCut <-cut(x,breaks=only.up.to)
	colors<-sequential_hcl(n=n, ...)[n:(n-only.up.to)]#reverse the order
	#if you don't want the full sequence, this will chop it short!
	out<-colors[xCut]
	return(out)
}



c.d.hcl<-function(x,n=1000, h=c(0,260),c=80,...){ #cut divergent_hcl
#c is the max chroma, must be zero where they meet in the middle
#hues: 0 is low, 260 is the higher.
	xNegInd <- which(x<0)
	xPosInd <- which(x>=0)
	
	nNeg <- length(xNegInd) #so we only get more faded parts on the side of the spectrum that doesn't have the same magnitude
	nPos <- length(xPosInd)
	biggerN<-max(nPos,nNeg)
	
	out<-rep(0,length(x))	
	if(!length(xNegInd)==0){
		xNegCol<-c.s.hcl(abs(x[xNegInd]),n=biggerN,only.up.to=nNeg,h=h[1],c=c(c,0),...)
		out[xNegInd]<-xNegCol
	}
	if(!length(xPosInd)==0){
		xPosCol<-c.s.hcl(x[xPosInd],n=biggerN,only.up.to=nPos,h=h[2],c=c(c,0),...)
		out[xPosInd]<-xPosCol
	}

	return(out)
}




splitpun<-function(words.in){ #splits text up by most punctuation terms, and then by spaces.
	words.pre1<-unlist(strsplit(words.in,split=c('\\.')))
	words.pre2<-unlist(strsplit(words.pre1,split=c('\\,')))
	words.pre3<-unlist(strsplit(words.pre2,split=c('!')))
	words.pre4<-unlist(strsplit(words.pre3,split=c('\\?')))
	words.pre5<-unlist(strsplit(words.pre4,split=c('\\"')))
	words.pre6<-unlist(strsplit(words.pre5,split=c(':')))
	words.pre7<-unlist(strsplit(words.pre6,split=c(';')))
	
	words.out1<-unlist(strsplit(words.pre7,split=' '))
	words.out2<-words.out1[nchar(words.out1)>0]
	return(words.out2)
}

#setwd('/Users/aaronfisher/Documents')
#save.image(file='AaronsFunctions.RData')






#    _             _    
#   (_)           | |   
#    _ _   _ _ __ | | __
#   | | | | | '_ \| |/ /
#   | | |_| | | | |   < 
#   | |\__,_|_| |_|_|\_\
#  _/ |                 
# |__/                  

# colmixer<-function(s){#sequence of values (numeric)
	# mag<-max(s)-min(s)
	# scaled<-(s-min(s))*4/(mag)
	# red<-c(); green<-c(); blue<-c() #will hold weights for the rgb function
	# for (j in 1:length(s)){
		# if (scaled[j]<=1) {red[j]<-1; green[j]<-0;blue[j]<-scaled[j]}
		# else if (scaled[j]<=2) {red[j] <- 1-(scaled[j]-1); green[j]<-0; blue[j]<-1} #red goes down
		# else if (scaled[j]<=3) {red[j] <- 0; green[j]<- .85*(scaled[j]-2); blue[j]<-1} #green goes up
		# else if (scaled[j]>3) {red[j] <- 0; green[j]<- .85; blue[j]<- 1-(scaled[j]-3)} #blue goes down
	# }
	# return(rgb(red,green,blue))
# }

# plot_colmixer_legend<-function(s, ...){
# plot(seq(min(s),max(s),length=500),rep(0,times=500),cex=2,yaxt='n',ylab='',col=colmixer(seq(min(s),max(s),length=500)), ...)
# }


