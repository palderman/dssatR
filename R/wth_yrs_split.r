wth.yrs.split <- function(wth){
    yrs = as.factor(as.POSIXlt(wth$data$DATE)$year)
    yr = levels(yrs)
    sep.yrs = lapply(yr,function(x,w){
                    w$data = w$data[yrs==x,]
                    return(w)
                },w=wth)
    return(sep.yrs)
}

