read.weather <- function(file.name,nasapower=FALSE){
    tmp = readLines(file.name)
    first.char = substr(tmp,1,1)
    title = tmp[first.char=='*']
    comments = tmp[first.char=='!']
    hlines = grep('@',tmp)
    for(i in 1:length(hlines)){
        if(i==length(hlines)){
            end = length(tmp)
        }else{
            end = hlines[i+1]-1
        }
        check = tmp[(hlines[i]+1):end]
        nrows = length(check[substr(check,1,1)!='!'&
            nchar(gsub('  *','',check))>0])
	if(nasapower){
            fmt.list = fmt.nasapower()
        }else{
            fmt.list = fmt.default()
        }
        vars = read.tier(tmp[hlines[i]],hlines[i],nrows,
            file.name=file.name,fmt.list=fmt.list)
        if('INSI'%in%colnames(vars)){
            station.info=vars
            cnames = colnames(station.info)
            if(any(c('WTHLAT','WTHLONG','WELEV')%in%cnames)){
                cnames[cnames=='WTHLAT'] = 'LAT'
                cnames[cnames=='WTHLONG'] = 'LONG'
                cnames[cnames=='WELEV'] = 'ELEV'
                colnames(station.info) = cnames
            }
        }else{
            data = vars
        }
    }
    if('DATE'%in%colnames(data)){
        data[,1] = as.integer(data[,1])
        data$DATE = as.POSIXct(sprintf('%5.5i',data$DATE),format='%y%j')
    }else if(all(c('WEYR','WEDAY')%in%colnames(data))){
        cnames = colnames(data)
        DATE = as.POSIXct(
            sprintf('%4.4i%3.3i',data$WEYR,data$WEDAY),
            format='%Y%j')
        data = data[,!cnames%in%c('WEYR','WEDAY')]
        data = data.frame(DATE=DATE,data)
    }
#    data[data < -90] = NA
    weather = list(title=title,station.info=station.info,data=data)
    return(weather)
}

