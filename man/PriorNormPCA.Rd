\name{PriorNormPCA}
\alias{PriorNormPCA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Prior PCA analysis for threshold setting and noise removal}
\description{
  This function performs a simple PCA analysis to aid in threshold
  setting and noise removal.
}
\usage{
PriorNormPCA(X)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{ Data Matrix (need not be normalised). Subsequent ICA seeks
    independent modes as independent distributions with values "down the
    rows".}
    }
\details{This function performs a simple PCA analysis and is used prior
  to application of the main ICA algorithm. The objective of the prior
  PCA is to help determine the dimensionality of a subspace on which
  the further ICA converges. The convention used here is that the rows of \code{X} label
  the space over which independent components are sought. For a typical
  microarray application in which ICA is being used as a generative
  model for gene expression, rows should label genes and columns should
  label samples. If, however, ICA is to be used as an unsupervised
  projection pursuit algorithm, rows should label samples and columns
  genes. For the latter application, the number of genes should be less than the
  number of samples.}
\value{A list with following components:
  \item{X}{Normalised data matrix with the mean of each column set to zero.}
  \item{corr}{Correlation/Covariance data matrix.}
  \item{eig}{Eigenvalue decomposition of Correlation/Covariance matrix.}
  \item{Dx}{Eigenvalues in a diagonal matrix.}
  \item{Ex}{Eigenvectors}
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

