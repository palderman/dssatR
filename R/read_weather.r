read.weather <- function(file.name,nasapower=FALSE){
    tmp = readLines(file.name)
    first.char = substr(tmp,1,1)
    title = tmp[first.char=='*']
    comments = tmp[first.char=='!']
    hlines = (1:length(tmp))[first.char=='@']
    header = lapply(tmp[hlines],FUN=function(x){
                  x=strsplit(x,split='  *')[[1]]
                  x=gsub('@','',gsub('\\.','',x))
                  return(x)
              })
    for(i in 1:2){
        header[[i]]=header[[i]][header[[i]]!='']
        fmt = get.wth.fmt(header[[i]])
        widths = gsub('%-*','',fmt)
        widths = gsub('[a-z].*','',widths)
        widths = as.numeric(gsub('\\..*','',widths))
        classes = gsub('.*s.*','character',fmt)
        classes = gsub('.*f.*','numeric',classes)
        section=tmp[(hlines[i]+1):
            ifelse(i==length(hlines),length(tmp),hlines[i+1]-1)]
        section=section[section!='']
        section=section[substr(section,1,1)!='!']
        params=matrix(ncol=length(widths),nrow=length(section))
        pos1=cumsum(c(1,widths[1:(length(widths)-1)]))
        pos2=cumsum(widths)
        for(c in 1:length(widths)){
            params[,c]=substr(section,pos1[c],pos2[c])
        }
        params=as.data.frame(params)
        for(j in 1:ncol(params)){
            if(classes[j]=='numeric'){
                params[,j]=as.numeric(params[,j])
            }else{
                params[,j]=gsub(' *','',params[,j])
            }
        }
        if(i==1){
            station.info=params
            colnames(station.info)=header[[i]]
        }else{
#            data=na.omit(params)
            data = params
            colnames(data)=header[[i]]
        }
    }
    if(!nasapower){
        data[,1] = as.integer(data[,1])
        data$DATE = as.POSIXct(sprintf('%5.5i',data$DATE),format='%y%j')
    }else{
        DATE = as.POSIXct(sprintf('%4.4i%3.3i',data$WEYR,data$WEDAY),format='%Y%j')
        data = data.frame(DATE=DATE,data[,3:ncol(data)])
    }
	data[data < -90] = NA
    weather = list(title=title,station.info=station.info,data=data)
    return(weather)
}

