"proposeNCP" <-
function(prPCA, thresh=0.1){ # START FUNCTION

  eig <- prPCA$eig ;
  X <- prPCA$X ;
  Ex <- prPCA$Ex ;
  ntp <- nrow(X);
  ndim <- ncol(X);

  print("About to find ncp");
  # project onto relevant eigendirections
  p.cpts <- eig$values[ eig$values > thresh ];
  ncp <- length(p.cpts);
  pCorr <- diag( eig$values[1:ncp] );
  pEx <- Ex[,1:ncp]; # this is Nsamples x ncp projection matrix
  
  x <- matrix(nrow=ntp,ncol=ncp); # is white
  for ( g in 1:ntp){
   for ( c in 1:ncp ){
     x[g,c] <- sum(X[g,]*Ex[,c])/sqrt(diag(pCorr)[c]) ; # projection onto c'th component
   }
  }



  return(list(X=X,x=x,pEx=pEx,pCorr=pCorr,ncp=ncp));
  

} # END FUNCTION
