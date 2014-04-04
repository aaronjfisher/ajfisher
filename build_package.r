####!!!!! questions for Leo
# sub/helper functions? how to handle best?
# passing ... to multiple sub functions OK?

#Create the package:
library('roxygen2')
library('devtools')
#set the working directory
setwd("/Users/aaronfisher/Documents/JH/aaronjfisher_package")


#create('hdboot') #only do this once


 
## Create the documentation fresh
document("aaronjfisher", clean=TRUE)
## Install the package
install("aaronjfisher")
 
## Check the help
library(aaronjfisher)
help(package=aaronjfisher)
 
## Check the doc, pkg
check_doc("aaronjfisher")
system.time (check("aaronjfisher"))










##### save RDA data for image0
library(jpeg)
marioMat<-readJPEG('marioSVD.jpg')[,150:353,1]
save(list='marioMat',file='aaronjfisher/data/marioMat.rda')

palBw<-sequential_hcl(50,c=c(0,0))#black and white palette
pal(palBw)
par(mfrow=c(1,2))
image(x,col=palBw)
image0(x,col=palBw)

