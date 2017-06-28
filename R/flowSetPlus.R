library(R6)

#' flowSetPlus
#'
#' flowSetPlus holds one for more \code{flowFramePlus} objects with some logical defaults.
#'
#' @section Usage:
#' \preformatted{p <- flowSetPlus$new("/projects/AML/data/AML/AML 1/AML 1 De Novo Panel_Tube_001.fcs", plist = c("FSC-H","SSC-H"))}
#' \preformatted{p <- flowSetPlus$plot()}
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
#' files<-dir(path="/projects/AML/data/AML/",pattern = "*.fcs",recursive = TRUE,include.dirs = TRUE,full.names = TRUE)
#' fsp<-flowSetPlus$new(files)
#' fsp$plot(plist=c("FSC-A","Horizon V500-A"))
NULL

#' @export
flowSetPlus <- R6Class("flowSetPlus",
                       public = list(
                         frames = NULL,
                         initialize = function(...)
                           fsp_initialize(self, private, ...),
                         plot = function(...)
                           fsp_plot(self, private, ...)
                )
)

#' Construct a flowSetPlus
#' @importFrom stats rnorm
#' @importFrom assertthat assert_that are_equal
#' @param self this
#' @param private this$private
#' @param ff flowFrame object(s) or FCS file path(s)
#' @param txlist Which columns to transform and how
#' @param plist Which columns to plot on the x and y axis by default (optional)
#'
#' @keywords internal
#'
fsp_initialize = function(self,
                          private,
                          ffs = NA,
                          txlist = NA,
                          plist = c("FSC-H", "SSC-H")) {
  if (length(ffs) > 1) {
    self$frames <-
      as.list(sapply(ffs, function(ff) {
        flowFramePlus$new(ff, txlist, plist)
      }))
  } else{
    self$frames <- list(flowFramePlus$new(ff, txlist, plist))
  }
}
