#' Member function performs biexponential transform on all parameters except SSC-W
#'
#' @importFrom flowCore colnames transform transformList linearTransform logTransform
#' @param private this$private
#' @param f flowFrame
#' @param cols Scale to apply to x axis
#' @param method transformation method
#' @details This function performs biexponential transform on all parameters except SSC-W
#' @details Jitter is set to false for biexponential to get reproducible results (otherwise would result in random #)
#' @return a transformed flowFrame
#'
#' @keywords internal
#'
ffp_transformCols <- function (self,private,cols,method) {
  self$ffTxed<-ffp_doTransform(f=self$ffTxed,cols=cols,method=method)
}

#' Static function to performs biexponential transform on all parameters except SSC-W
#' This is verbatim what Wade was using originally
ffp_doTransform <- function (f,cols=c(1:5,7:13),method=c("biexp","log","linear"), fac=5.4/262143) {
  if (is(f,"flowFrame")){
    method=match.arg(method)
    if(method=="boxcox"){
      bx <- boxcoxTransform(jitter=F)
      bxlist <- transformList (colnames(f)[cols], bx)
      return(transform (f, bxlist))
    }
    if(method=="biexp"){
      bx <- biexpTransform(jitter=F)
      bxlist <- transformList (colnames(f)[cols], bx)
      return(transform (f, bxlist))
    }
    if(method=="log"){
      lx <- logTransform()
      lxlist <- transformList (colnames(f)[cols], lx)
      return(transform (f, lxlist))
    }
    if(method=="linear"){
      lx <- linearTransform(a=fac)
      lxlist <- transformList (colnames(f)[cols], lx)
      return(transform (f, lxlist))
    }
  }
  else if (is(f,"flowSet")){
    for(i in 1:length(f)){
      method=match.arg(method)
      if(method=="boxcox"){
        bx <- biexpTransform(jitter=F)
        bxlist <- transformList (colnames(f[[i]])[cols], bx)
        f[[i]] = (transform (f[[i]], bxlist))
      }
      if(method=="biexp"){
        bx <- biexpTransform(jitter=F)
        bxlist <- transformList (colnames(f[[i]])[cols], bx)
        f[[i]] = (transform (f[[i]], bxlist))
      }
      if(method=="log"){
        lx <- logTransform()
        lxlist <- transformList (colnames(f[[i]])[cols], lx)
        f[[i]] = (transform (f[[i]], lxlist))
      }
      if(method=="linear"){
        lx <- linearTransform(a=fac)
        lxlist <- transformList (colnames(f[[i]])[cols], lx)
        f[[i]] = (transform (f[[i]], lxlist))
      }
    }
  }
  return (f)

}
