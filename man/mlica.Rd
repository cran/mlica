\name{mlica}
\alias{mlica}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Maximum likelihood implementation of Independent Component Analysis}
\description{
 This function performs ICA using a maximum likelihood framework and
 takes as arguments parameters to control the number of algorithm runs
 and convergence criteria.
}
\usage{
mlica(prNCP, nruns = 10, tol = 1e-04, maxit = 300, fail.th = 5, learn.mu = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{prNCP}{ The output object from \code{proposeNCP}.}
  \item{nruns}{ The number of converged algorithm runs sought (function
    returns the best solution according to the log-likelihood value). }
  \item{tol}{ Tolerance level for establishing convergence of run.}
  \item{maxit}{ Maximum number of iterations to allow per run. }
  \item{fail.th}{ A threshold on the number of consecutive runs that
    fail to converge.}
  \item{learn.mu}{ Learning parameter for fixed point algorithm (note
    that this need not be changed since it has already been optimised).}
}
\value{
  A list with following components:
  \item{A}{Estimate of the mixing matrix.}
  \item{B}{Estimate of the inverse mixing matrix.}
  \item{S}{Estimate of the source matrix.}
  \item{X}{Normalised data matrix.}
  \item{ncp}{Number of independent components.}
  \item{NC}{Binary number specifying whether best run converged or
    not.(=1 indicates convergence,=0 indicates no convergence).}
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
      expression determined by independent component analysis},
    Bioinformatics \bold{18}, no.1, 51--60.}
  \item{4}{Chiappetta P., Roubaud MC. and Torresani B.: \emph{Blind
      source separation and the analysis of microarray data},
    J. Comput. Biol. 2004; 11(6):1090--109.}
}

\examples{
% Simulate data ( Data is matrix of 60 "samples" and 1000 "genes"
% Use mixture of 5 independent components, each IC is mixture of two gaussians.)
data(simMAdata);
% Prior PCA and subspace selection
dataX <- simMAdata[[1]];
prPCA <- PriorNormPCA(dataX);
prNCP <- proposeNCP(prPCA,0.1);
% Multiple repeated runs to check stability of best inferred runs
a.best.l <- list();
for( i in 1:5){
 a.best.l[[i]] <- mlica(prNCP,nruns=5);
}
% selecting the best out of nruns runs
% checking stability of best runs.
checkICA <- CheckStability(a.best.l,0.7);
% checking inferred ICA modes
sourceS <- simMAdata[[3]];
print(cor(a.best.l[[1]]$S,sourceS));
% Sort modes
sModes <- SortModes(a.best.l[[1]],prNCP,c.val=0.5);
}
\keyword{cluster}% at least one, from doc/KEYWORDS

