\name{CheckStability}
\alias{CheckStability}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Tests stability of inferred ICA modes.}
\description{
 Performs a correlation test to see which of the inferred ICA modes are
 reproducible across multiple runs using different random
 initialisations. Returns a set of consensus ICA modes and stability
 scores for each following the algorithm of Chiappetta,...et.al (2004).
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
  \item{consS}{Consensus source matrix with columns labeling the consensus ICA modes. Has same number of rows as \code{a.best$S}.}
  \item{consA}{Consensus mixing matrix with rows labeling the consensus
    ICA modes.}
  \item{stabM}{Vector of same length as \code{consM} giving the
    stability measures of each consensus ICA mode. Stability or
    reproducibility measures are given as fractions, that is, the number
    of times the ICA mode correlates with one of the other runs at
    threshold level \code{corr.th} divided by the number of runs (length of \code{a.best.l}).}
}
\author{Andrew Teschendorff \email{aet21@cam.ac.uk}}
\references{
  \item{1}{Hyvaerinen A., Karhunen J., and Oja E.: \emph{Independent Component
    Analysis}, John Wiley and Sons, New York, (2001).}
  \item{2}{Kreil D. and MacKay D. (2003): \emph{Reproducibility Assessment of
    Independent Component Analysis of Expression Ratios from DNA
    microarrays}, Comparative and Functional Genomics \bold{4} (3),300--317.}
  \item{3}{Liebermeister W. (2002): \emph{Linear Modes of gene
      expression determined by independent component analysis},
    Bioinformatics \bold{18}, no.1, 51--60.}
  \item{4}{Chiappetta P., Roubaud MC. and Torresani B.: \emph{Blind
      source separation and the analysis of microarray data},
    J. Comput. Biol. 2004; 11(6):1090--109.}
}
\examples{
%See documentation of mlica
}
\keyword{cluster}% at least one, from doc/KEYWORDS

