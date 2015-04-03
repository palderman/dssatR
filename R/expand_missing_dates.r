expand.missing.dates <- function(data,first=NULL,last=NULL){
    if(is.null(first)){
        first = paste(as.POSIXlt(data$DATE[1])$year+1900,'-01-01',sep='')
        first = as.POSIXct(first)        
    }
    if(is.null(last)){
        last = paste(as.POSIXlt(data$DATE[1])$year+1900,'-12-31',sep='')
        last = as.POSIXct(last)        
    }
    all.dates = data.frame(DATE=seq(first,last,'day'))
    data$DATE=round(data$DATE,'days')
    all.dates$DATE=round(all.dates$DATE,'days')
    data = merge(data,all.dates,all=TRUE)
    return(data)
}
