\name{CheckStability}
\alias{CheckStability}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Tests stability of inferred ICA modes.}
\description{
 Does a correlation test to see which of the inferred ICA modes are reproducible.
}
\usage{
CheckStability(a.best.l, corr.th)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a.best.l}{List of \code{a.best} objects from \code{mlica} runs.}
  \item{corr.th}{Correlation threshold to use to decide whether a mode
    is reproducible.}
}

\value{
  A list with the following components
  \item{stabM}{Vector of length \code{ncp} giving fraction of elements in
    \code{a.best.l}, for which correlation of the mode is larger than \code{corr.th}.}
  \item{nI}{Number of elements in \code{a.best.l}.}
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
\examples{
%See documentation of mlica
}
\keyword{cluster}% at least one, from doc/KEYWORDS

