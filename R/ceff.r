ceff <- function(data){
    obsmean=mean(data$obs)
    ceff=1-sum((data$obs-data$pred)^2)/sum((data$obs-obsmean)^2)
    ceff=round(ceff,2)
    return(ceff)
}

