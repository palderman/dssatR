rrmse.out <- function(variable,...){
    data = get.obs.pred(variable=variable,...)
    return(rrmse(data))
}

