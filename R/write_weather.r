write.weather <- function(weather,filename=NA){
    if(is.na(filename)){
        filename = paste(weather[[2]]$INSI,
            substr(as.character(as.POSIXlt(weather$data$DATE[1])$year+1900),3,4),
            '01.WTH',sep='')
    }
    tmp = vector(length=nrow(weather$data)+5,mode='character')
    tmp[1] = paste('*WEATHER DATA :',weather$title)
    tmp[2] = ''
    tmp[3] = paste(c('@',sprintf(c('%5s',rep('%9s',2),rep('%6s',6)),colnames(weather$station.info))),collapse='')
    fmt = c('%6s',rep('%9.3f',2),'%6.0f',rep('%6.1f',2),rep('%6.2f',2))
    for (i in 1:ncol(weather$station.info)){
        weather$station.info[,i] = gsub(' NA','-99',sprintf(fmt[i],weather$station.info[,i]))
    }
    tmp[4] = paste(weather$station.info[1,],collapse='')
    cnames = colnames(weather$data)
    fmt = unlist(lapply(colnames(weather$data),get.wth.fmt))
    weather$data$DATE = POSIXct.to.DATE(weather$data$DATE)
    weather$data = as.data.frame(lapply(weather$data,as.double))
    colnames(weather$data)[1] = paste('@',colnames(weather$data)[1],sep='')
    tmp[5]=''
    tmp[6] = paste(sprintf(c('%5s',rep('%6s',length(colnames(weather$data))-1)),colnames(weather$data)),collapse='')
    for (i in 1:ncol(weather$data)){
        weather$data[,i] = gsub(' NA','-99',sprintf(fmt[i],weather$data[,i]))
    }
    for (i in 1:nrow(weather$data)){
        tmp[6+i] = paste(weather$data[i,],collapse='')
    }
    write(tmp,filename)
}

