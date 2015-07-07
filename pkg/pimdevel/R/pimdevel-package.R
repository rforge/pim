#' Probabilistic Index Models
#' 
#' Fit a probabilistic index model. This package is the latest
#' version of the pim package, and will end up being pim 2.0. It's a beta
#' currently under development. There's a pim fitting engine with
#' rudimentary functionality, but don't expect any back-compatibility
#' with the previous versions of pim.
#' 
#' @name pimdevel-package
#' @aliases pimdevel-package
#' @docType package
#' @author Joris Meys \email{Joris.Meys@@UGent.be} Nick Sabbe \email{Nick.Sabbe@@UGent.be} Jan De Neve \email{JanR.DeNeve@@UGent.be} 
#' @references [PENDING]
#' @keywords package
#' @importFrom stats4 nobs
#' @import methods
NULL

# All things needed for S4 definitions.
setOldClass("terms")
setOldClass("formula")

# Generics for multiple places
setGeneric('print')



