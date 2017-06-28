library(R6)
library(flowViz)
library(flowCore)


#ffs<-flowFramePlus$new("/projects/AML/data/AML/AML 1/AML 1 De Novo Panel_Tube_001.fcs")
#hyperbolic arcsin (implemented in flowCore), boxcox, logical transforms
#c("biexp", "log", "linear")
#flowFramePlus$new(x,txlist=list("linear"=c(1:4),"biexp"=c(5:12)))

#' flowFramePlus
#'
#' flowFramePlus holds transformalization and scale attributes. It is essentially a small
#' wrapper around \code{flowFrame} with some logical defaults.
#'
#' @section Usage:
#' \preformatted{p <- flowFramePlus$new("/projects/AML/data/AML/AML 1/AML 1 De Novo Panel_Tube_001.fcs", plist = c("FSC-H","SSC-H"))}
#' \preformatted{p <- flowFramePlus$plot()}
#'
#' @section Arguments:
#' \describe{
#'   \item{ff}{A \code{flowFrame} object or filepath of an FCS file.}
#'   \item{txlist}{List, specify transformation to be used on each column index.
#'   For example \code{txlist=list("linear"=c(1:4),"biexp"=c(5:12))}}
#' }
#'
#' @section Details:
#' \code{$new()} starts a new flowFramePlus object, either from an existing flowFrame object or
#' an FCS filepath
#'
#' You can specify which columns to transform using the following syntax:
#' \preformatted{p <- flowFramePlus$new(x,txlist=list("linear"=c(1:4),"biexp"=c(5:12)))}
#'
#' @importFrom R6 R6Class
#' @name flowFramePlus
#' @examples
#' ffp<-flowFramePlus$new("/projects/AML/data/AML/AML 1/AML 1 De Novo Panel_Tube_001.fcs")
#' ffp$plot()
NULL

#' @export
flowFramePlus <- R6Class("flowFramePlus",
                         public = list(
                           ffOrig = NULL,
                           ffTxed = NULL,
                           ffFile = NULL,
                           txlist = NULL,
                           txfac = NULL,
                           plist = NULL,
                           plotScales = NULL,
                           initialize = function(...)
                             ffp_initialize(self, private, ...),
                           plot = function(...)
                             ffp_plot(self, private, ...),
                           lookupPlotScale = function(...)
                             ffp_lookupPlotScale(self, private, ...),
                           transformCols = function(...)
                             ffp_transformCols(self, private, ...)
                         )
)

#' Construct a flowFramePlus
#' @importFrom stats rnorm
#' @importFrom assertthat assert_that are_equal
#' @param self this
#' @param private this$private
#' @param ff flowFrame object or FCS file path
#' @param txlist Which columns to transform and how
#' @param plist Which columns to plot on the x and y axis by default (optional)
#'
#' @keywords internal
#'
ffp_initialize = function(self,
                          private,
                          ff = NA,
                          txlist = NA,
                          plist = c("FSC-H", "SSC-H")) {
  if (is(ff, 'flowFramePlus'))
  {
    self = clone(ff)
  } else{
    if (is(ff, 'flowFrame')) {
      self$ffOrig <- ff
    } else{
      if (is(ff, 'character')) {
        if (file.exists(ff)) {
          self$ffFile <- ff
          self$ffOrig <-
            flowCore::read.FCS(ff, transformation = FALSE)
        } else{
          stop(paste("can't find file", ff))
        }
      } else{
        stop("flowFramePlus requires either a flowFrame object or a filename")
      }
    }
  }
  self$ffTxed <- self$ffOrig
  assertthat::assert_that(class(self$ffTxed) == 'flowFrame')
  #the columns to plot
  self$plist <- plist

  if (is.na(txlist)) {
    #apply transformalization to conventions on scatter and fluorescence columns
    #scatter signals are kept linear
    #flourescence log-ish apply biexp
    scatterCols <-
      which(grepl("FSC|SSC", flowCore::colnames(self$ffTxed)))
    fluorCols <-
      which(!(
        grepl("Time", flowCore::colnames(self$ffTxed)) |
          grepl("FSC|SSC", flowCore::colnames(self$ffTxed))
      ))
    assertthat::assert_that(length(fluorCols) > 0)
    txlist <- list("linear" = scatterCols, "biexp" = fluorCols)
    #there should be no overlap
    assertthat::are_equal(length(Reduce(intersect, txlist)), 0)
  }
  for (name in names(txlist)) {
    #actually transform the data in self$ffTxed
    self$transformCols(cols = txlist[[name]], method = name)
  }
  #the transformalizations to apply
  self$txlist <- txlist

  #find the index of the columns to plot
  plotCols <-
    sapply(plist, function(x) {
      which(grepl(x, colnames(self$ffTxed)))
    })
  #find the scale of the columns to plot based on their transformations e.g. 1:linear 2:biexp
  #we also lookup the name of the scale here
  plotScales <-
    sapply(qdapTools::lookup(plotCols, txlist), self$lookupPlotScale)
  names(plotScales) <- c("x", "y")
  self$plotScales <- as.list(plotScales)
}

