rrmse <- function(data){
    rrmse=sqrt(mean((data$obs-data$pred)^2))/mean(data$obs)
    rrmse=round(rrmse,digits=2)
    return(rrmse)
}

