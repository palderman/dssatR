ceff.out <- function(variable,...){
    data = get.obs.pred(variable=variable,...)
    return(ceff(data))
}

