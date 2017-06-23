flow_frame_basic_plot <- function (self, private, plist = self$plist, blueBackground=FALSE, showZero=TRUE, nbin=501, bandwidth=0.001, cr=blob_color(blueBackground=FALSE), col = "black", nrpoints=0, instrument=c("diva", "influx"), tx=self$plotScales$x, ty=self$plotScales$y, plotaxt = T,  ticksize=1,...) {
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
