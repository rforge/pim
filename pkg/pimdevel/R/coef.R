#' @include pim-class.R

# There is no help page for coef here, as it behaves as shown on the
# help page of coef in the stats package

setGeneric('coef')

coef.pim <- function(object,...){
  object@coef
}

setMethod('coef',
          'pim',
          coef.pim)