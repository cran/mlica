"CheckStability" <-
function(a.best.l, corr.th){

  nIruns <- length(a.best.l) ;

  if( nIruns < 2 ){
    print("Argument must be list of at least two a.best objects");
    stop;
  }
  for( i in 2:nIruns){
    if( a.best.l[[1]]$ncp != a.best.l[[i]]$ncp ){
    print("Stopping: the objects in list must have same number of ICA modes");
    stop;
    }
  }
 ncp <- a.best.l[[1]]$ncp ;
 StabScore.v <- rep(1,ncp);

 for( j in 2:nIruns){
  Corr.m <- matrix(nrow=ncp,ncol=ncp);
  for( i in 1:ncp){# Check whether mode i is stable
    tmp.v <- vector();
    for( k in 1:ncp){
     tmp.v[k] <- abs(cor(a.best.l[[1]]$S[,i], a.best.l[[j]]$S[,k]));
    }
    if( length(which( tmp.v > corr.th )) > 0 ){
      StabScore.v[i] <- StabScore.v[i] + 1;
    }      
  }     
 }

 StabScore.v <- StabScore.v/nIruns ;

 return(list(stabM=StabScore.v, nI=nIruns));

} # END OF FUNCTION
