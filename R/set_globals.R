set_globals <-
function(nslice,nburn,nrowX,ncolX,NS,N,NDIM,UNFOLD,NMISSING,X,CONSTRAINTS) {
  res <-.C("copyFromR",
           as.integer(nslice),
           as.integer(nburn),
           as.integer(nrowX),
           as.integer(ncolX),
           as.integer(NS),
           as.integer(N),
           as.integer(NDIM),
           as.integer(UNFOLD),
           as.integer(NMISSING),
           as.double(X),
           as.double(CONSTRAINTS))
}
