library(R6)
library(flowViz)
source ("~/git/R/tools/sourceTools.R")


#ffs<-flowFramePlus$new("/projects/AML/data/AML/AML 1/AML 1 De Novo Panel_Tube_001.fcs")
#hyperbolic arcsin (implemented in flowcore), boxcox, logical transforms
#c("biexp", "log", "linear")
#flowFramePlus$new(x,transformlist=list("linear"=c(1:4),"biexp"=c(5:12)))

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
#'   \item{p}{A \code{process} object.}
#'   \item{command}{Character scalar, the command to run. It will be
#'     escaped via \code{\link[base]{shQuote}}.}
#'
#' @section Details:
#' \code{$new()} starts a new process, it uses \code{\link[base]{pipe}}.
#' R does \emph{not} wait for the process to finish, but returns
#' immediately.
#'
#' You can specify which columns to transform using the following syntax:
#' \preformatted{p <- flowFramePlus$new(x,transformlist=list("linear"=c(1:4),"biexp"=c(5:12)))}
#'
#' @importFrom R6 R6Class
#' @name process
#' @examples
#' p <- process$new("sleep", "2")
#' p$is_alive()
#' p
NULL

#' @export
flowFramePlus <- R6Class("flowFramePlus",
                         public = list(
                           ffOrig = NULL,
                           ffTxed = NULL,
                           ffFile = NULL,
                           transformlist = NULL,
                           transform_fac = NULL,
                           plist = NULL,
                           plotScales = NULL,
                           initialize = function(...)
                             flowframeplus_initialize(self, private, ...),
                           plot = function(...)
                             flow_frame_basic_plot(self, private, ...)
                         ),
                         private = list(
                           lookupPlotScaleBytransformScale = function(ns){
                             #if there is some lookup that needs to occur it should be here
                             #right now let's map by identity
                             return(ns)
                           }
                         )
)

