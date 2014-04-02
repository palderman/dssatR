rmse <- function(data){
    rmse=sqrt(mean((data$obs-data$pred)^2))
    rmse=signif(rmse,4)
    return(rmse)
}

