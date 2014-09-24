dstat.out <- function(variable,...){
    data = get.obs.pred(variable=variable,...)
    return(dstat(data))
}

