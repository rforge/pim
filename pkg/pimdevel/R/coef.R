#' @include pim-class.R pim.summary-class.R

# There is no help page for coef here, as it behaves as shown on the
# help page of coef in the stats package

setGeneric('coef')

coef.pim <- coef.pim.summary <- function(object,...){
  object@coef
}

setMethod('coef',
          'pim',
          coef.pim)

setMethod('coef',
          'pim.summary',
          coef.pim.summary)