\name{proposeNCP}
\alias{proposeNCP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Number of independent components proposal function}
\description{
 This function takes the output of \code{PriorNormPCA} and returns for
 a given threshold the number of components to be inferred for
 subsequent ICA.
}
\usage{
proposeNCP(prPCA, thresh = 0.1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{prPCA}{The output object from \code{PriorNormPCA}.}
  \item{thresh}{Threshold on eigenvalues.}
}
\value{A list with following components:
  \item{X}{Normalised data matrix.}
  \item{x}{Normalised data matrix projected onto selected subspace.}
  \item{pEx}{Selected eigenvectors defining subspace for projection.}
  \item{pCorr}{Projected correlation matrix.}
  \item{ncp}{The dimension of the selected subspace(=number of
    independent components to be inferred with subsequent ICA).}
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
\examples{%See documentation of mlica.R
}
\keyword{cluster}% at least one, from doc/KEYWORDS

