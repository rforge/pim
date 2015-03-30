#' @include pim-class.R

# There is no help page for coef here, as it behaves as shown on the
# help page of coef in the stats package

setGeneric('vcov')

vcov.pim <- function(object,...){
  object@vcov
}

setMethod('vcov',
          'pim',
          vcov.pim)