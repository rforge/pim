#' @include pim-class.R 
#' @export

# There is no help page for vcov here, as it behaves as shown on the
# help page of \code{\link[stats]{vcov}} in the stats package.

setGeneric('vcov')

vcov.pim <- function(object,...){
  object@vcov
}

setMethod('vcov',
          'pim',
          vcov.pim)