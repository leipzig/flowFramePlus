flowframeplus_initialize = function(self, private, ff = NA, transformlist = NA, plist = c("FSC-H","SSC-H")) {
  if(class(ff)=='flowFrame'){
    self$ffTxedOrig <-ff
  }else{
    if(file.exists(ff)){
      self$ffTxedFile<-ff
      self$ffTxedOrig <-read.FCS(ff, transformation = FALSE)
    }else{
      stop("flowFramePlus requires either a flowFrame object or a filename")
    }
  }
  self$ffTxed<-self$ffTxedOrig
  assertthat::assert_that(class(self$ffTxed)=='flowFrame')
  #the columns to plot
  self$plist <- plist

  if(is.na(transformlist)){
    #apply transformalization to conventions on scatter and fluorescence columns
    #scatter signals are kept linear
    #flourescence log-ish apply biexp
    scatterCols<-which(grepl("FSC|SSC",colnames(self$ffTxed)))
    fluorCols<-which(!(grepl("Time",colnames(self$ffTxed)) | grepl("FSC|SSC",colnames(self$ffTxed))))
    transformlist<-list("linear"=scatterCols,"biexp"=fluorCols)
    #there should be no overlap
    assertthat::are_equal(length(Reduce(intersect,transformlist)),0)
  }
  lapply(names(transformlist),function(name){
    #actually transform the data
    self$ffTxed<<-doTransform(self$ffTxed,cols=transformlist[[name]],method=name)
  })
  #the transformalizations to apply
  self$transformlist<-transformlist

  #find the index of the columns to plot
  plotCols<-sapply(plist,function(x){which(grepl(x,colnames(self$ffTxed)))})
  #find the scale of the columns to plot based on their transformations e.g. 1:linear 2:biexp
  #we also lookup the name of the scale here
  plotScales<-sapply(qdapTools::lookup(plotCols,transformlist),self$lookupPlotScaleBytransformScale)
  names(plotScales)<-c("x","y")
  self$plotScales<-as.list(plotScales)
}

