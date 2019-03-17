do.LBFGS <-
function(obj){
  lbfgs.result <- do_lbfgs(nrow(obj$data),ncol(obj$data),obj$yrotate,obj$rmatrix)  
  return(list(lbfgs.result=lbfgs.result, data=obj$data))
}
