bayesunfold <- function(input, max=100, min=0, nburn=1000, 
    nslice=2000, NS=2,UNFOLD=1, NMISSING=7, rotmat=NULL, 
    print.lbfgs="console", print.slice="console"){
    b <- bu.Preprocess(inp, max, min)
    set.Globals(b, nburn, nslice, NS, UNFOLD, NMISSING)
    if(print.lbfgs != "console")sink(print.lbfgs)
    l <- do.LBFGS(b)
    if(print.lbfgs != "console")sink()
    if(print.slice != "console")sink(print.slice)    
    s <- do.Slice(l, nburn, nslice, NS, UNFOLD, NMISSING, rotmat)
    if(print.slice != "console")sink()
    return(s)
}


