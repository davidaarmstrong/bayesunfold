set.Globals <-
function(obj, nburn=1000, nslice=2000, NS=2,UNFOLD=1, NMISSING=7){
  N <- NS*(nrow(obj$data)+ncol(obj$data)) - ((NS*(NS+1))/2)
  NDIM <- NS*(nrow(obj$data)+ncol(obj$data)) - (NS-1)
  set_globals(nslice,nburn,nrow(obj$data),ncol(obj$data),NS,N,NDIM,UNFOLD,NMISSING,obj$X,obj$CONSTRAINTS)
}
