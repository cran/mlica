\name{mlicaMAIN}
\alias{mlicaMAIN}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Main engine function that implements the fixed point algorithm
  for maximum likelihood inference of ICA modes.}
\description{
 See references for detailed description.
}
\usage{
mlicaMAIN(prNCP, tol = 1e-04, maxit = 300, mu = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{prNCP}{The output object of \code{proposeNCP}.}
  \item{tol}{Tolerance level for convergence.}
  \item{maxit}{Maximum number of iterations to allow for convergence.}
  \item{mu}{Learning paramter for fixed point algorithm. This has
    already been optimised.}
}
\value{
  A list with following components:
  \item{A}{Estimate of the mixing matrix.}
  \item{B}{Estimate of the inverse mixing matrix.}
  \item{S}{Estimate of the source matrix.}
  \item{X}{Normalised data matrix.}
  \item{ncp}{Number of independent components.}
  \item{NC}{Binary number specifying whether best run converged,0, or
    not,1.}
  \item{LL}{Log likelihood value of best run.}
}
\author{Andrew Teschendorff \email{aet21@cam.ac.uk}}
\references{
  \item{1}{Hyvaerinen A., Karhunen J., and Oja E.: \emph{Independent Component
    Analysis}, John Wiley and Sons, New York, (2001).}
  \item{2}{Kreil D. and MacKay D. (2003): \emph{Reproducibility Assessment of
    Independent Component Analysis of Expression Ratios from DNA
    microarrays}, Comparative and Functional Genomics \bold{4} (3),300--317.}
  \item{3}{Liebermeister W. (2002): \emph{Linear Modes of gene
      expression determined by independent component analysis}, Bioinformatics \bold{18}, no.1, 51--60.}
}
\examples{%See documentation for mlica.R
}
\keyword{internal}% __ONLY ONE__ keyword per line
