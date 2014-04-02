dstat <- function(data){
    willmott = 1-sum((data$obs-data$pred)^2)/sum((abs(data$pred-mean(data$obs))+abs(data$obs-mean(data$obs)))^2)
    willmott = round(willmott,2)
    return(willmott)
}

