#' Plot a flowFramePlus use base R (pplot)
#'
#' @importFrom flowCore plot
#' @param self this
#' @param private this$private
#' @param plist Which columns to plot on the x and y axes, can override settings made at class instantiation
#' @param tx Scale to apply to x axis
#' @param ty Scale to apply to y axis
#'
#' @keywords internal
#'
ffp_plot <- function (self, private, plist = self$plist, blueBackground=FALSE, showZero=TRUE, nbin=501, bandwidth=0.001, cr=blob_color(blueBackground=FALSE), col = "black", nrpoints=0, instrument=c("diva", "influx"), tx=self$plotScales$x, ty=self$plotScales$y, plotaxt = T,  ticksize=1,...) {
  if(!is.null(cr)){
    suppressWarnings (flowCore::plot (self$ffTxed, plist, colramp=cr, nbin=nbin, band=bandwidth, nrpoints=nrpoints, axes=FALSE, ...))
  }
  else{
    suppressWarnings(plot (exprs(self$ffTxed)[,plist[1]],exprs(self$ffTxed)[,plist[2]], pch=20, col=col,cex =.2,  axes=F,xlab=plist[1],ylab=plist[2], ...))
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

ffp_lookupPlotScale = function(self, private, ns){
  #if there is some lookup that needs to occur it should be here
  #right now let's map by identity
  return(ns)
}
