POSIXct.to.DATE <- function(x){
    x = as.POSIXlt(x)
    yr = as.character(x$year)
    yr[nchar(yr)>2] = substring(yr[nchar(yr)>2],2,3)
    doy = sprintf('%3.3i',x$yday+1)
    yrdoy = as.integer(paste(yr,doy,sep='')) 
    return(yrdoy)
}

