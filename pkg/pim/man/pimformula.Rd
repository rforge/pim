\name{pimformula}
\alias{pim.fit.prep}
\alias{pimfitdata}
\alias{pimfitdata-class}
\alias{pimformula}
\alias{pimformula-class}
\title{Convert formula to pim formula}
\usage{
  pimformula(formula, data,
    interpretation = c("difference", "regular", "marginal", "symmetric"),
    verbosity = 0, leftsuffix = "_L", rightsuffix = "_R",
    extra.variables = character(),
    lhs = c("PO", "<", "<="),
    rhsreplacers = list(F = Freplacetext, O = Oreplacetext, L = Lreplacetext, R = Rreplacetext),
    lhsreplacer = LHSreplacetext,
    interactions.difference = (interpretation != "marginal"),
    extra.nicenames = data.frame(org = character(), nice = character(), stringsAsFactors = FALSE))

  pim.fit.prep(formula, data,
    blocking.variables = character(),
    poset = t(combn(nrow(data), 2)), leftsuffix = "_L",
    rightsuffix = "_R",
    interpretation = c("difference", "regular", "marginal", "symmetric"),
    na.action = na.fail, lhs = c("PO", "<", "<="),
    verbosity = 0, nicenames = TRUE,
    interactions.difference = (interpretation != "marginal"),
    extra.nicenames = data.frame(org = character(), nice = character(), stringsAsFactors = FALSE))
}
\arguments{
  \item{formula}{Original formula}

  \item{data}{Context where the formula \code{formula} is
  to be interpreted}

  \item{interpretation}{If \code{"marginal"} (not the
  default) parts of the formula are converted to imply
  marginal pim modeling (see e.g.
  \code{\link{Mainreplacetext}}). If it is
  \code{"difference"}, then the design matrix of the PIM is
  the difference of the design matrices of each part of the
  pseudo-observations. The default option is
  \code{"regular"}, which will interpret unaltered columns
  as differences. A new option is \code{"symmetric"}, which
  works the same as \code{"regular"}, but will enforce the
  symmetry condition by making the sign switch when
  changing the order (typically, this is achieved by
  subtracting the inverse for each dummy).}

  \item{verbosity}{The higher this value, the more levels
  of progress and debug information is displayed (note: in
  R for Windows, turn off buffered output)}

  \item{leftsuffix,rightsuffix}{Suffixes that will be added
  to the 'left' and 'right' observation's column name in
  the pseudo-observation. Note: no checking is done that
  these suffixes are safe, so the wrong suffixes may lead
  to unexpected behaviour.}

  \item{extra.variables}{Character vector of column names
  you want to force present in the pseudo-observations}

  \item{lhs}{\code{"PO"}, \code{"<"} or \code{"<="}:
  Unequality used for the lefthandside of the formula. The
  default (\code{"PO"}) is the normal probabilistic index.}

  \item{rhsreplacers}{List of functions (see
  \code{\link{Lreplacetext}} and others) that will be used
  to process the right hand side of the formula. Each
  function should have the same signature as
  \code{\link{Lreplacetext}}.}

  \item{lhsreplacer}{Function like
  \code{\link{LHSreplacetext}} that will be used to
  reformat the left hand side of the formula}

  \item{interactions.difference}{If \code{TRUE} (note that
  the default is \code{interpretation!="marginal"})
  interaction terms will be interpreted as the differences
  of the onesided interaction terms (if this is possible at
  all). This is unsupported if \code{unsupported if} is
  \code{"marginal"}. Some special interaction terms with
  calculated columns may lead to unexpected behaviour.}

  \item{extra.nicenames}{Should be a \code{data.frame}
  containing two character columns: \code{org} and
  \code{nice}. For each "constructed" column name, provide
  a nicer one, that will make the results more readable.
  You may also use parts of constructed column names. Note:
  make sure to use \code{stringsAsFactor=FALSE} when
  creating the \code{data.frame}.}

  \item{blocking.variables}{Character vector holding column
  names that hold blocking variables.}

  \item{poset}{Matrix of two columns indicating what the
  original observation number is for the left and right
  real observation in the pseudo-observation.}

  \item{na.action}{Defaults to \code{\link{na.fail}}:
  handles missing data in \code{data}.}

  \item{nicenames}{Defaults to \code{TRUE}: try to make the
  column names more readable.}
}
\value{
  For \code{pimformula}: an object of class "pimformula".
  The items in this object are: \item{newformula}{The
  formula containing all suffixed variable names}
  \item{left.variables }{\code{\link{data.frame}}
  containing one row for each variable pertained in the
  "left" observations, and two columns: \code{org} and
  \code{fixed}, containing the original name and the
  suffixed name of each variable.} \item{right.variables
  }{\code{\link{data.frame}} containing one row for each
  variable pertained in the "right" observations, and two
  columns: \code{org} and \code{fixed}, containing the
  original name and the suffixed name of each variable.}
  \item{names }{Character vector holding the names for each
  individual term in the right hand side of the formula.
  Note: currently this is in no way cleaned up!}
  \item{full.colnames }{Character vector holding the
  constructed parts in the formula. Should have the same
  length as \code{nice.colnames}} \item{nice.colnames
  }{Character vector holding nicer names the constructed
  parts in the formula.  Should have the same length as
  \code{full.colnames}}

  For \code{pim.fit.prep}: an object of class "pimfitdata".
  The items in this object are: \item{X}{The design matrix
  in pseudo-observation space} \item{Y }{The
  pseudo-observations} \item{poset }{Matrix of two columns
  indicating what the original observation number is for
  the left and right real observation in the
  pseudo-observation. Note: in some cases this is not the
  passed in \code{poset}, eg when blocks were present.}
  \item{intercept }{Holds \code{TRUE} if the formula
  contains an intercept.} \item{pimformula }{Result of
  \code{pimformula} function.} \item{original.colnames }{If
  \code{nicenames} was \code{TRUE}, this will hold the
  column names before "nicing up".}
}
\description{
  Convert formula to pim formula (incorporating L/R and
  poset)
}
\details{
  Main function, doing the actual work. The idea is to
  convert the formula to text and replace 4(+) kind of
  "spiced" variables: O(var) gets replaced with
  I(var_R<var_L) (see \code{\link{Oreplacetext}} for exact
  formulation) F(var) gets replaced with Sum
  I(var_R=i)I(var_L=j) (see \code{\link{Freplacetext}} for
  exact formulation) L(var) gets replaced with var_L R(var)
  gets replaced with var_R var not in any of the above
  cases gets replaced by either var_R-varL
  (interpretation!="marginal") or by var_L
  (interpretation=="marginal")

  Some sanity checks are already performed, but not all of
  them (I guess)
}
\note{
  TODO: Should probably disallow using intercept in some
  cases Also have to consider whether passing in contrasts
  is relevant/possible
}
\examples{
set.seed(1)
iris$out<-factor(sample(2, nrow(iris), replace=TRUE))
iris$xord<-as.ordered(iris$Species)
pimformula(out~Sepal.Length, data=iris)
pimformula(out~I((R(Sepal.Length) - L(Sepal.Length))/sqrt(R(Sepal.Length) * L(Sepal.Length)) ), data=iris, interpretation="regular")
pimformula(out~O(xord), data=iris, interpretation="regular")
pimformula(out~F(Species), data=iris, interpretation="regular")
set.seed(1)
iris$out<-factor(sample(2, nrow(iris), replace=TRUE))
iris$xord<-as.ordered(iris$Species)
pim.fit.prep(out~Sepal.Length, data=iris)
pim.fit.prep(out~I((R(Sepal.Length) - L(Sepal.Length))/sqrt(R(Sepal.Length) * L(Sepal.Length)) ), data=iris, interpretation="regular")
pim.fit.prep(out~O(xord), data=iris, interpretation="regular")
pim.fit.prep(out~F(Species), data=iris, interpretation="regular")
}
\seealso{
  \code{\link{Lreplacetext}}

  \code{\link{Lreplacetext}}
}
\keyword{formula}
\keyword{pim}

