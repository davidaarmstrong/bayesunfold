do.Slice <-
function(obj,nburn=1000, nslice=2000, NS=2,UNFOLD=1, NMISSING=7, rotmat=NULL){
  NDIM <- NS*(nrow(obj$data)+ncol(obj$data)) - (NS-1)
  if(!is.null(rotmat)){
    theta <- theta2 <- as.vector(t(obj$lbfgs.result[[3]] %*% rotmat))
  }
  else{
    theta <- theta2 <- obj$lbfgs.result[[3]]
  }
  theta1000 <- rep(0,nslice*NDIM)
  dim(theta1000) <- c(nslice*NDIM,1)
  ssenow <- rep(0,(2*(nslice+nburn)))
  dim(ssenow) <- c((2*(nslice+nburn)),1)
  XTRUE <- theta  
  thetaL <- rep(-99.0, NDIM)
  thetaR <- rep(99.0, NDIM)
  dim(thetaL) <- dim(thetaR) <- c(NDIM, 1)
  thetaL[NDIM] <- 0.10
  thetaR[NDIM] <- 0.50
  WW <- 1.0
  PP <- 3.0
  XCOORDS <- rep(0,(nrow(obj$data)+ncol(obj$data))*NS)
  SIGMAPRIOR <- 100.0
  res <- do_sliceu(theta,theta2,theta1000,ssenow,XTRUE,thetaL,thetaR,WW,PP,XCOORDS,SIGMAPRIOR)
  sigma_squared_hat <- mean(res[[4]][2501:4000])
  sigma_squared_hat_sd <- sd(res[[4]][2501:4000])
  #
  #  SAMPLES
  #
  samples <- matrix(res[[3]], ncol=NDIM, byrow=TRUE)
  #
  stimuli <- vector("list",NS)
  for(i in 1:NS){
    stimuli[[i]] <- samples[,(seq(i, ncol(obj$data)*NS, by=NS))]
    stimuli[[i]] <- as.mcmc(stimuli[[i]])
  }
  #
  individuals <- vector("list",NS)
  for(i in 1:NS){
    individuals[[i]] <- samples[,(seq((ncol(obj$data)*NS+i), NDIM, by = NS))]
    individuals[[i]] <- as.mcmc(individuals[[i]])
  }
  out <- list(stimuli = stimuli, individuals=individuals)
  return(out)
}
