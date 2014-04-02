combine.wth.yrs <- function(wth){
    all.yrs = wth[[1]]
    all.yrs$data = do.call(rbind,lapply(wth,function(x){
            x$data[,c('DATE','SRAD','TMAX','TMIN','RAIN')]}))
    return(all.yrs)
}

