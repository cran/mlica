\name{SortModes}
\alias{SortModes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Sorting of ICA modes}
\description{
 Sorts inferred ICA modes using two criteria: Relative data power or the
 Liebermeister criterion, which is based on a measure that is a weighted
 linear combination of non-gaussianity and data variance measures.
}
\usage{
SortModes(a.best,c.val = 0.25)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a.best}{The output object of \code{mlica}.}
  \item{c.val}{A parameter to control the relative weight of the two
    measures when using the Liebermeister criterion. Should be between 0
    (pure data variance measure) and 1 (pure non-gaussianity).
    }
}
\value{
  A list with components:
  \item{a.best}{The output of \code{mlica}.}
  \item{rdp}{The relative data power values obtained for each
    independent component.}
  \item{lbm}{The Liebermeister contrast value for each component.}
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
%See documentation for mlica.
}
\keyword{cluster}% at least one, from doc/KEYWORDS

