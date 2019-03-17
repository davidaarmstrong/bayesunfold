do_lbfgs <-
function(kpnp,kpnq,yrotate,rmatrix){
  .C("mainlbfgs",
     as.integer(kpnp),
     as.integer(kpnq),
     as.double(yrotate),
     as.double(rmatrix))
}
