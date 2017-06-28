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

