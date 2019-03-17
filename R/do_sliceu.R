do_sliceu <-
function(theta,thetanow2,theta1000,ssenow,XTRUE,thetaLeft,thetaRight,WW,PP,XCOORDS,SIGMAPRIOR){
  .C("sliceunfolding",
     as.double(theta),
     as.double(thetanow2),
     as.double(theta1000),
     as.double(ssenow),
     as.double(XTRUE),
     as.double(thetaLeft),
     as.double(thetaRight),
     as.double(WW),
     #    as.integer(PP),
     as.double(PP),
     as.double(XCOORDS),
     as.double(SIGMAPRIOR))
}
