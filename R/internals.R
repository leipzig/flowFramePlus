#' Construct a flowFramePlus
#' @importFrom stats rnorm
#' @importFrom assertthat assert_that are_equal
#' @param self this
#' @param private this$private
#' @param ff flowFrame object or FCS file path
#' @param txlist Which columns to transform and how
#' @param plist Which columns to plot on the x and y axis
#'
#' @keywords internal
#'
ffp_initialize = function(self, private, ff = NA, txlist = NA, plist = c("FSC-H","SSC-H")) {
  if(class(ff)=='flowFrame'){
    self$ffOrig <-ff
  }else{
    if(file.exists(ff)){
      self$ffFile<-ff
      self$ffOrig <-flowCore::read.FCS(ff, transformation = FALSE)
    }else{
      stop("flowFramePlus requires either a flowFrame object or a filename")
    }
  }
  self$ffTxed<-self$ffOrig
  assertthat::assert_that(class(self$ffTxed)=='flowFrame')
  #the columns to plot
  self$plist <- plist

  if(is.na(txlist)){
    #apply transformalization to conventions on scatter and fluorescence columns
    #scatter signals are kept linear
    #flourescence log-ish apply biexp
    scatterCols<-which(grepl("FSC|SSC",flowCore::colnames(self$ffTxed)))
    fluorCols<-which(!(grepl("Time",flowCore::colnames(self$ffTxed)) | grepl("FSC|SSC",flowCore::colnames(self$ffTxed))))
    assertthat::assert_that(length(fluorCols)>0)
    txlist<-list("linear"=scatterCols,"biexp"=fluorCols)
    #there should be no overlap
    assertthat::are_equal(length(Reduce(intersect,txlist)),0)
  }
  for(name in names(txlist)){
    #actually transform the data
    self$ffTxed<-ffp_doTransform(f=self$ffTxed,cols=txlist[[name]],method=name)
  }
  #the transformalizations to apply
  self$txlist<-txlist

  #find the index of the columns to plot
  plotCols<-sapply(plist,function(x){which(grepl(x,colnames(self$ffTxed)))})
  #find the scale of the columns to plot based on their transformations e.g. 1:linear 2:biexp
  #we also lookup the name of the scale here
  plotScales<-sapply(qdapTools::lookup(plotCols,txlist),self$lookupPlotScale)
  names(plotScales)<-c("x","y")
  self$plotScales<-as.list(plotScales)
}

