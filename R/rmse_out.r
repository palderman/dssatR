rmse.out <- function(variable,...){
    data = get.obs.pred(variable=variable,...)
    return(rmse(data))
}

