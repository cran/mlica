"PriorNormPCA" <-
function(X){ # START FUNCTION

 ndim <- ncol(X);
 ntp  <- nrow(X);
 # Center column means to zero
 for( s in 1:ndim ){
  X[,s] <- X[,s] - mean( X[,s] );
 }
 # Compute correlation matrix of data
 print("Computing correlation matrix");
 Corr <- matrix(nrow=ndim,ncol=ndim);
  for ( i in 1:ndim){
   for( j in 1:ndim){
     Corr[i,j] <- sum(X[,i]*X[,j])/ntp ;
   }
  }
  print("Finished computing correlation matrix");
  print("About to enter PCA");
  # Principal Component Analysis
  eig <- eigen(Corr,symmetric=TRUE);
  Dx <- diag(eig$values);
  Ex <- eig$vectors ;
  barplot(Dx,main="PCA eigen-values");
          
  return(list(X=X,corr=Corr,eig=eig,Dx=Dx,Ex=Ex));

} # END OF FUNCTION
