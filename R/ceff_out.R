ceff.out <- function(variable,trno=NULL,sqno=NULL,run=NULL){
    data = get.obs.pred(variable=variable,trno=trno,sqno=sqno,run=run)
    return(ceff(data))
}

