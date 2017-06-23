library(R6)
library(flowViz)

#qdapTools
source ("~/git/R/tools/sourceTools.R")


x<-read.FCS("/projects/AML/data/AML/AML 1/AML 1 De Novo Panel_Tube_001.fcs", transformation = FALSE)

#hyperbolic arcsin (implemented in flowcore), boxcox, logical transforms
#c("biexp", "log", "linear")
#flowFramePlus$new(x,normlist=list("linear"=c(1:4),"biexp"=c(5:12)))

flowFramePlus <- R6Class("flowFramePlus",
                         public = list(
                           ffOrig = NULL,
                           ff = NULL,
                           normlist = NULL,
                           norm_fac = NULL,
                           plist = NULL,
                           plotScales = NULL,
                           initialize = function(ff = NA, normlist = NA, plist = c("FSC-H","SSC-H")) {
                             self$ffOrig <- ff
                             
                             #the columns to plot
                             self$plist <- plist
                             
                             if(is.na(normlist)){
                               #apply normalization to conventions on scatter and fluorescence columns
                               #scatter signals are kept linear
                               #flourescence log-ish apply biexp
                               scatterCols<-which(grepl("FSC|SSC",colnames(ff)))
                               fluorCols<-which(!(grepl("Time",colnames(ff)) | grepl("FSC|SSC",colnames(ff))))
                               normlist<-list("linear"=scatterCols,"biexp"=fluorCols)
                               #there should be no overlap
                               assertthat::are_equal(length(Reduce(intersect,normlist)),0)
                             }
                             lapply(names(normlist),function(name){
                               #actually transform the data
                               self$ff<<-doTransform(ff,cols=normlist[[name]],method=name)
                             })
                             #the normalizations to apply
                             self$normlist<-normlist
                             
                             
                             #find the index of the columns to plot
                             plotCols<-sapply(plist,function(x){which(grepl(x,colnames(ff)))})
                             #find the scale of the columns to plot based on their transformations e.g. 1:linear 2:biexp
                             #we also lookup the name of the scale here
                             plotScales<-sapply(qdapTools::lookup(plotCols,normlist),self$lookupPlotScaleByNormScale)
                             names(plotScales)<-c("x","y")
                             self$plotScales<-as.list(plotScales)
                             

                           },
                           chooseTransforms = function(plist){
                             sapply(plist,function(x){ifelse(grepl("FSC|SSC",x),"linear","biexp")})
                           },
                           lookupPlotScaleByNormScale = function(ns){
                             #if there is some lookup that needs to occur it should be here
                             #right now let's map by identity
                             return(ns)
                           },
                           plot = function (plist = self$plist, blueBackground=FALSE, showZero=TRUE, nbin=501, bandwidth=0.001, cr=blob_color(blueBackground=FALSE), col = "black", nrpoints=0, instrument=c("diva", "influx"), tx=self$plotScales$x, ty=self$plotScales$y, plotaxt = T,  ticksize=1,...) {
                             require ("fields")
                             if(!is.null(cr)){
                               suppressWarnings (plot (self$ff, plist, colramp=cr, nbin=nbin, band=bandwidth, nrpoints=nrpoints, axes=FALSE, ...))
                             }
                             else{
                               suppressWarnings(plot (exprs(self$ff)[,plist[1]],exprs(self$ff)[,plist[2]], pch=20, col=col,cex =.2,  axes=F,xlab=plist[1],ylab=plist[2], ...))
                               box()
                             }
                             
                             if(plotaxt==TRUE){
                               suppressWarnings (ax (1, instrument=instrument, type=tx, ticksize=ticksize))
                               suppressWarnings (ax (2, instrument=instrument, type=ty,  ticksize=ticksize))
                             }	
                             if (showZero) {
                               xline (0, lty='dotdash')
                               yline (0, lty='dotdash')
                             }
                           }
                         )
)