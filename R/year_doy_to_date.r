year.doy.to.date <- function(data){
    if(all(c('YEAR','DOY')%in%colnames(data))){
        year=sprintf('%4i',as.integer(data[,'YEAR']))
        doy = sprintf('%3i',as.integer(data[,'DOY']))
        yeardoy=paste(year,doy,sep='')
        data$DATE=as.POSIXct(yeardoy,format='%Y%j')
        data = data[,!colnames(data)%in%c('YEAR','DOY')]
    }else if(!'YEAR'%in%colnames(data)){
        warning('YEAR not present in data. DATE not created')
    }else if(!'DOY'%in%colnames(data)){
        warning('DOY not present in data. DATE not created')
    }
    return(invisible(data))
}
