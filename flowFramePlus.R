library(R6)
source ("~/git/R/tools/sourceTools.R")


x<-read.FCS("/projects/AML/data/AML/AML 1/AML 1 De Novo Panel_Tube_001.fcs", transformation = FALSE)


#flowFramePlus$new(x,normlist=list("linear"=c(1:4),"biexp"=c(5:12)))

flowFramePlus <- R6Class("flowFramePlus",
                         public = list(
                           ff = NULL,
                           normlist = NULL,
                           norm_fac = NULL,
                           plist_default = NULL,
                           initialize = function(ff = NA, normalization = NA) {
                             self$ffOrig <- ff

                             if(is.null(normlist)){
                               #apply normalization to conventions on scatter and fluorescence columns
                               scatterCols<-which(grepl("FSC|SSC",colnames(ff)))
                               fluorCols<-which(!(grepl("Time",colnames(ff)) | grepl("FSC|SSC",colnames(ff))))
                               normlist<-list("linear"=scatterCols,"biexp"=fluorCols)
                             }
                             lapply(names(normlist),function(name){self$ff<<-doTransform(ff,cols=normlist[[name]],method=name)})
                             #self$flowFrame <- doTransform(flowFrame,cols=norm_cols,method=norm_method, fac=norm_fac)
                             self$plot()
                             self$plist_default = c("FSC-H","SSC-H")
                           },
                           chooseTransforms = function(plist){
                             sapply(plist,function(x){ifelse(grepl("FSC|SSC",x),"linear","biexp")})
                           },
                           choosePlotAxes = function(x_trans,y_trans){
                             #scatter signals are kept linear
                               #flourescence log-ish apply biexp, plot biexp (this runs "AX")
                               #hyperbolic arcsin (implemented in flowcore), boxcox, logical transforms
                               #tx = 'linear', 
                             #ty = 'linear', 
                           },
                           plot = function (plist = self$plist_default, blueBackground=FALSE, showZero=TRUE, nbin=501, bandwidth=0.001, cr=blob_color(blueBackground=FALSE), col = "black", nrpoints=0, instrument=c("diva", "influx"), tx=c("biexp", "log", "linear"), ty=c("biexp", "log", "linear"), plotaxt = T,  ticksize=1,...) {
                             require ("fields")
                             if(!is.null(cr)){
                               suppressWarnings (plot (self$flowFrame, plist, colramp=cr, nbin=nbin, band=bandwidth, nrpoints=nrpoints, axes=FALSE, ...))
                             }
                             else{
                               suppressWarnings(plot (exprs(self$flowFrame)[,plist[1]],exprs(self$flowFrame)[,plist[2]], pch=20, col=col,cex =.2,  axes=F,xlab=plist[1],ylab=plist[2], ...))
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