lyr.depths <- function(sl.prof){

}
    return(sep.yrs)
                },w=wth)
                    return(w)
                    w$data = w$data[yrs==x,]
    sep.yrs = lapply(yr,function(x,w){
    yr = levels(yrs)
    yrs = as.factor(as.POSIXlt(wth$data$DATE)$year)
wth.yrs.split <- function(wth){

}
    return(all.yrs)
            x$data[,c('DATE','SRAD','TMAX','TMIN','RAIN')]}))
    all.yrs$data = do.call(rbind,lapply(wth,function(x){
    all.yrs = wth[[1]]
combine.wth.yrs <- function(wth){

}
    return(widths)
    }
        }))
                return(width)
                        5)
                        NITROGEN=11,RESIDUES=11,HARVEST=11,
                        OUTPUTS=11,PLANTING=11,IRRIGATION=11,
                        SNAME=25,OPTIONS=11,METHODS=11,MANAGEMENT=11,
                        FERNAME=120,TNAME=120,R=2,HNAME=120,GENERAL=11,
                        ICNAME=120,PLNAME=120,I=2,IRNAME=120,
                        XCRD=15,YCRD=15,ELEV=9,AREA=17,SANAME=120,
                        ID_FIELD=8,WSTA=8,SLTX=4,ID_SOIL=10,FLNAME=120,
                        SITE=120,PAREA=6,HARM=120,INGENO=6,CNAME=120,
                        N=2,O=2,P=2,R=2,T=2,PEOPLE=120,ADDRESS=120,
                        MT=2,ME=2,MH=2,SM=2,CR=2,A=2,C=2,F=2,H=2,L=2,
                       CU=2,FL=2,SA=2,IC=2,MP=2,MI=2,MF=2,MR=2,MC=2,
                       'P1V'=6,'P1D=6',
                       'VAR#'=6,'VAR-NAME'=16,'EXP#'=6,'ECO#'=6,
            width = switch(x,
        widths = unlist(lapply(vnames,FUN=function(x){
    } else if(type=='cul'){
        }))
                return(width)
                        5)
                        NITROGEN=11,RESIDUES=11,HARVEST=11,
                        OUTPUTS=11,PLANTING=11,IRRIGATION=11,
                        SNAME=25,OPTIONS=11,METHODS=11,MANAGEMENT=11,
                        FERNAME=120,TNAME=120,R=2,HNAME=120,GENERAL=11,
                        ICNAME=120,PLNAME=120,I=2,IRNAME=120,
                        XCRD=15,YCRD=15,ELEV=9,AREA=17,SANAME=120,
                        ID_FIELD=8,WSTA=8,SLTX=4,ID_SOIL=10,FLNAME=120,
                        SITE=120,PAREA=6,HARM=120,INGENO=6,CNAME=120,
                        N=2,O=2,P=2,R=2,T=2,PEOPLE=120,ADDRESS=120,
                        MT=2,ME=2,MH=2,SM=2,CR=2,A=2,C=2,F=2,H=2,L=2,
                       CU=2,FL=2,SA=2,IC=2,MP=2,MI=2,MF=2,MR=2,MC=2,
            width = switch(x,
        widths = unlist(lapply(vnames,FUN=function(x){
    if(type=='filex'){
get.widths <- function(vnames,type='filex'){

}
    return(cnames)
    }
        cnames = gsub('\\.','',cnames)
    if(any(grepl('TIMPL',cnames))){
                                name)))
                                SNAME='SNAME....................',
                                SLAS='.SLAS',
                                FLWR='.FLWR',
                                SLEN='.SLEN',
                                AREA='.............AREA',
                                ELEV='.....ELEV',
                                YCRD='...........YCRD',
                                XCRD='...........XCRD',
                                WSTA='WSTA....',
                                TNAME='TNAME....................',
                                HARM='HARM.........',
    cnames = unlist(lapply(cnames, function(name) switch(name,
add.periods <- function(cnames){

}
         }
                write(soil.file,filename,append=append)                
                soil.file = soil.file[2:length(soil.file)]
        }else{
            write(soil.file,filename)
        if (!append) {
        soil.file=gsub(' NA','-99',soil.file)
        }
            }
                                        collapse=''),sep='')
                                                soil.profile$layer[j,18:ncol(soil.profile$layer)]),
                                        paste(sprintf(lyr.fmt[2:length(lyr.fmt)],
                                paste(sprintf(lyr.fmt[1],soil.profile$layer[j,1]),
                soil.file[10+nrow(soil.profile$layer)+j] = 
                lyr.fmt = c(fmt[1],lyr.fmt)
                lyr.fmt[soil.profile$layer[j,18:ncol(soil.profile$layer)]==-99] = '%6.0f'
                lyr.fmt = rep('%6.2f',ncol(soil.profile$layer)-17)
            for (j in 1:nrow(soil.profile$layer)){
            substr(soil.file[10+nrow(soil.profile$layer)],1,1)='@'
            soil.file[10+nrow(soil.profile$layer)] = paste(sprintf('%6s',c('SLB',colnames(soil.profile$layer[,18:ncol(soil.profile$layer)]))),collapse='')
        if(ncol(soil.profile$layer)>17){
        }
                                    collapse=''),sep='')
                                            soil.profile$layer[j,3:length(lyr.fmt)]),
                                    paste(sprintf(lyr.fmt[3:length(lyr.fmt)],
                                    sprintf(lyr.fmt[2],soil.profile$layer[j,2]),
            soil.file[8+j] = paste(sprintf(lyr.fmt[1],soil.profile$layer[j,1]),
            lyr.fmt[2] = '%6s'
            lyr.fmt[soil.profile$layer[j,1:17]==-99] = '%6.0f'
            lyr.fmt = fmt
        for (j in 1:nrow(soil.profile$layer)){
        fmt = c('%6.0f','%6s',rep('%6.3f',5),rep('%6.2f',2),rep('%6.1f',3),'%6.3f',rep('%6.1f',4))
        substr(soil.file[8],1,1)='@'
        soil.file[8] = paste(sprintf('%6s',colnames(soil.profile$layer[,1:17])),collapse='')
                            sep='')
                            sprintf('%6s',soil.profile$SMKE),
                            sprintf('%6s',soil.profile$SMPX),
                            sprintf('%6s',soil.profile$SMHB),
                            sprintf('%6.2f',soil.profile$SLPF),
                            sprintf('%6.2f',soil.profile$SLNF),
                            sprintf('%6.1f',soil.profile$SLRO),
                            sprintf('%6.2f',soil.profile$SLDR),
                            sprintf('%6.1f',soil.profile$SLU1),
                            sprintf('%6.2f',soil.profile$SALB),
        soil.file[7] = paste(sprintf('%6s',soil.profile$SCOM),
        soil.file[6] = '@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE'
                            sep='')
                            ' ',soil.profile$SCS_FAMILY,
                            sprintf('%8.3f',soil.profile$LONG),
                            sprintf('%8.3f',soil.profile$LAT),
                            paste(rep(' ',13-nchar(soil.profile$COUNTRY)),collapse=''),
                            soil.profile$COUNTRY,
                            paste(rep(' ',12-nchar(soil.profile$SITE)),collapse=''),
        soil.file[5] = paste(' ',soil.profile$SITE,
        soil.file[4] = '@SITE        COUNTRY          LAT     LONG SCS FAMILY'
                        soil.profile$SOIL_NAME,sep='')
                        sprintf('%8.0f',soil.profile$DEPTH),' ',
                        sprintf('%3s',soil.profile$TEXTURE_CLASS),
                        sprintf('%-12s',soil.profile$SOURCE),
        soil.file[3] = paste('*',soil.profile$ID_SOIL,'  ',
        soil.file[2] = ''
        soil.file[1] = '*SOILS:'
        soil.file = vector(length=nlines,mode='character')
        }
            nlines = 10+2*nrow(soil.profile$layer)
        }else{
            nlines = 8+nrow(soil.profile$layer)
        if(ncol(soil.profile$layer)<=17){
write.soil.file <- function(soil.profile,filename,append=T){

}
    return(yrdoy)
    yrdoy = as.integer(paste(yr,doy,sep='')) 
    doy = sprintf('%3.3i',x$yday+1)
    yr[nchar(yr)>2] = substring(yr[nchar(yr)>2],2,3)
    yr = as.character(x$year)
    x = as.POSIXlt(x)
POSIXct.to.DATE <- function(x){

}
    write(tmp,filename)
    }
        tmp[6+i] = paste(weather$data[i,],collapse='')
    for (i in 1:nrow(weather$data)){
    }
        weather$data[,i] = gsub(' NA','-99',sprintf(fmt[i],weather$data[,i]))
    for (i in 1:ncol(weather$data)){
    tmp[6] = paste(sprintf(c('%5s',rep('%6s',length(colnames(weather$data))-1)),colnames(weather$data)),collapse='')
    tmp[5]=''
    colnames(weather$data)[1] = paste('@',colnames(weather$data)[1],sep='')
    weather$data = as.data.frame(lapply(weather$data,as.double))
    weather$data$DATE = POSIXct.to.DATE(weather$data$DATE)
    fmt = unlist(lapply(colnames(weather$data),get.wth.fmt))
    cnames = colnames(weather$data)
    tmp[4] = paste(weather$station.info[1,],collapse='')
    }
        weather$station.info[,i] = gsub(' NA','-99',sprintf(fmt[i],weather$station.info[,i]))
    for (i in 1:ncol(weather$station.info)){
    fmt = c('%6s',rep('%9.3f',2),'%6.0f',rep('%6.1f',2),rep('%6.2f',2))
    tmp[3] = paste(c('@',sprintf(c('%5s',rep('%9s',2),rep('%6s',5)),colnames(weather$station.info))),collapse='')
    tmp[2] = ''
    tmp[1] = weather$title #paste('*WEATHER DATA :',weather$location)
    tmp = vector(length=nrow(weather$data)+5,mode='character')
    }
            '01.WTH',sep='')
            substr(as.character(as.POSIXlt(weather$data$DATE[1])$year+1900),3,4),
        filename = paste(weather[[2]]$INSI,
    if(is.na(filename)){
write.weather <- function(weather,filename=NA){

}
    return(fmt)
                ))}))
                    '%6.1f'
                    DATE='%5.5i',
                    FLAG='%6.0f',
                    ELEV='%6.0f',
                    LONG='%9.3f',
                    LAT='%9.3f',
                    INSI='%6s',
               return(switch(x,
    fmt = unlist(lapply(name,function(x){
get.wth.fmt <- function(name){

}
    return(weather)
    weather = list(title=title,station.info=station.info,data=data)
	data[data<-90] = NA
    }
        data = data.frame(DATE=DATE,data[,3:ncol(data)])
        DATE = as.POSIXct(sprintf('%4.4i%3.3i',data$WEYR,data$WEDAY),format='%Y%j')
    }else{
        data$DATE = as.POSIXct(sprintf('%5.5i',data$DATE),format='%y%j')
        data[,1] = as.integer(data[,1])
    if(!nasapower){
    }
        }
            colnames(data)=header[[i]]
            data = params
#            data=na.omit(params)
        }else{
            colnames(station.info)=header[[i]]
            station.info=params
        if(i==1){
        }
            }
                params[,j]=gsub(' *','',params[,j])
            }else{
                params[,j]=as.numeric(params[,j])
            if(classes[j]=='numeric'){
        for(j in 1:ncol(params)){
        params=as.data.frame(params)
        }
            params[,c]=substr(section,pos1[c],pos2[c])
        for(c in 1:length(widths)){
        pos2=cumsum(widths)
        pos1=cumsum(c(1,widths[1:(length(widths)-1)]))
        params=matrix(ncol=length(widths),nrow=length(section))
        section=section[substr(section,1,1)!='!']
        section=section[section!='']
            ifelse(i==length(hlines),length(tmp),hlines[i+1]-1)]
        section=tmp[(hlines[i]+1):
        classes = gsub('.*f.*','numeric',classes)
        classes = gsub('.*s.*','character',fmt)
        widths = as.numeric(gsub('\\..*','',widths))
        widths = gsub('[a-z].*','',widths)
        widths = gsub('%-*','',fmt)
        fmt = get.wth.fmt(header[[i]])
        header[[i]]=header[[i]][header[[i]]!='']
    for(i in 1:2){
              })
                  return(x)
                  x=gsub('@','',gsub('\\.','',x))
                  x=strsplit(x,split='  *')[[1]]
    header = lapply(tmp[hlines],FUN=function(x){
    hlines = (1:length(tmp))[first.char=='@']
    comments = tmp[first.char=='!']
    title = tmp[first.char=='*']
    first.char = substr(tmp,1,1)
    tmp = readLines(file.name)
read.weather <- function(file.name,nasapower=FALSE){

}
    return(soil.profile)
    if(exists('soil.profile.backup')) rm('soil.profile.backup',envir=globalenv())
    }
        }
            if(tolower(entry) != 'b') i = i + 1
            }
                if(tolower(entry) != 'b') j = j + 1
                }
                    }
                        }
                            j = ncol(soil.profile$layer)
                            i = i - 1
                        }else{
                            j = j - 1
                        if(j > 1){
                    }else{
                        }
                            soil.profile$layer[i,j] = eval(parse(text=entry))
                        }else{
                            soil.profile$layer[i,j] = entry
                        if(grepl('^[[:alpha:]]$',entry)){
                    if ( tolower(entry) != 'b'){
                    entry = readline('(Press [b] to go back)\n')
                    cat(paste('Enter ',colnames(soil.profile$layer),' for layer',i,' below:',sep=''),'\n')
                if(is.na(soil.profile$layer[i,j])|tolower(entry)=='b'){
            while(j <= ncol(soil.profile$layer)){
        while(i <= nlayer){
        i = 1; j = 1; entry = ''
        }
                                        soil.profile$layer[1:(nlayer-10),])
            soil.profile$layer = rbind(soil.profile$layer,
        }else{
            soil.profile$layer = soil.profile$layer[1:nlayer,]
        if(nlayer <= 10){
        nlayer = eval(parse(text=readline('Enter number of soil layers:\n')))
        }
            if (tolower(entry) != 'b') i = i + 1
            }
                assign('soil.profile.backup',soil.profile,envir=globalenv())
                }
                    soil.profile[[i]] = entry
                }else{
                    i = i -1
                if(tolower(entry) == 'b'){
                entry = readline('(Press [b] to go back)\n')
                cat(paste('Enter ',variable.names[i],' below:',sep=''),'\n')
            if(is.null(soil.profile[[i]])|tolower(entry)=='b'){
        while(i < length(variable.names)){
        i = 1; entry = ''
        variable.names = names(soil.profile)
    if(!empty){
    }
                        )
                                    SADC = rep(NA,10))
                                    SLHB = rep(NA,10), SCEC = rep(NA,10), 
                                    SLNI = rep(NA,10), SLHW = rep(NA,10), 
                                    SLSI = rep(NA,10), SLCF = rep(NA,10), 
                                    SLOC = rep(NA,10), SLCL = rep(NA,10), 
                                    SSKS = rep(NA,10), SBDM = rep(NA,10), 
                                    SSAT = rep(NA,10), SRGF = rep(NA,10), 
                                    SLLL = rep(NA,10), SDUL = rep(NA,10),
                                    SLB = rep(NA,10), SLMH = rep(NA,10), 
                        layer = cbind(
                        SMKE = NULL,
                        SMPX = NULL,
                        SMHB = NULL,
                        SLPF = NULL,
                        SLNF = NULL,
                        SLRO = NULL,
                        SLDR = NULL,
                        SLU1 = NULL,
                        SALB = NULL,
                        SCOM = NULL,
                        SCS_FAMILY = NULL,
                        LONG = NULL,
                        LAT = NULL,
                        COUNTRY = NULL,
                        SITE = NULL,
                        SOIL_NAME = NULL,
                        DEPTH = NULL,
                        TEXTURE_CLASS = NULL,
                        SOURCE = NULL,
        soil.profile = list(ID_SOIL = NULL,
    }else{
        soil.profile = get('soil.profile.backup')
    if (exists('soil.profile.backup')){
create.soil.profile <- function(empty=F){

}
    write(soil.file,filename,append=T)
    }
                                collapse=''),sep='')
                                        soil.profile[[i]]$layer[j,3:length(lyr.fmt)]),
                                paste(sprintf(lyr.fmt[3:length(lyr.fmt)],
                                sprintf(lyr.fmt[2],soil.profile[[i]]$layer[j,2]),
        soil.file[7+j] = paste(sprintf(lyr.fmt[1],soil.profile[[i]]$layer[j,1]),
        lyr.fmt[soil.profile$layer[j,]==-99] = '%6.0f'
        lyr.fmt = fmt
    for (j in 1:nrow(soil.profile[[i]]$layer)){
    fmt = c('%6.0f','%6s',rep('%6.3f',5),rep('%6.2f',2),rep('%6.1f',3),'%6.3f',rep('%6.1f',4))
    soil.file[7] = '@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC'
                        sep='')
                        sprintf('%6s',soil.profile[[i]]$SMKE),
                        sprintf('%6s',soil.profile[[i]]$SMPX),
                        sprintf('%6s',soil.profile[[i]]$SMHB),
                        sprintf('%6.2f',soil.profile[[i]]$SLPF),
                        sprintf('%6.2f',soil.profile[[i]]$SLNF),
                        sprintf('%6.1f',soil.profile[[i]]$SLRO),
                        sprintf('%6.2f',soil.profile[[i]]$SLDR),
                        sprintf('%6.1f',soil.profile[[i]]$SLU1),
                        sprintf('%6.2f',soil.profile[[i]]$SALB),
    soil.file[6] = paste(sprintf('%6s',soil.profile[[i]]$SCOM),
    soil.file[5] = '@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE'
                        sep='')
                        ' ',soil.profile[[i]]$SCS_FAMILY,
                        sprintf('%8.3f',soil.profile[[i]]$LONG),
                        sprintf('%8.3f',soil.profile[[i]]$LAT),
                        paste(rep(' ',13-nchar(soil.profile[[i]]$COUNTRY)),collapse=''),
                        soil.profile[[i]]$COUNTRY,
                        paste(rep(' ',12-nchar(soil.profile[[i]]$SITE)),collapse=''),
    soil.file[4] = paste(' ',soil.profile[[i]]$SITE,
    soil.file[3] = '@SITE        COUNTRY          LAT     LONG SCS FAMILY'
                    soil.profile[[i]]$SOIL_NAME,sep='')
                    sprintf('%8.0f',soil.profile[[i]]$DEPTH),' ',
                    sprintf('%3s',soil.profile[[i]]$TEXTURE_CLASS),
                    sprintf('%-12s',soil.profile[[i]]$SOURCE),
    soil.file[2] = paste('*',soil.profile[[i]]$ID_SOIL,'  ',
    soil.file[1] = ''
    soil.file = vector(length=7+nrow(soil.profile$layer),mode='character')
add.to.soil.file <- function(soil.profile,filename){

}
    return(soil.profile)
    soil.profile$layer$SLMH = as.character(soil.profile$layer$SLMH)
    }
        }
            soil.profile$layer=temp
        }else{
            soil.profile$layer=merge(soil.profile$layer,temp,by='SLB',all=T)
        if(i>1){
        colnames(temp) = cnames[cnames!='']
                                split='!'))
                                gsub('@',' ',soil[line1])),
                                gsub('  *','!',
        cnames = unlist(strsplit(
        temp[temp==-99] = NA
                widths=rep(6,nchar(soil[line1])/6))
                header=F,
                textConnection(soil[(line1+1):line2]),
        temp = read.fwf(
        }
            line2 = length(soil)
        }else{
            line2 = 5 + headlines[i+1]-1
        if (i < length(headlines)){
        line1 = 5 + headlines[i]
    for (i in 1:length(headlines)){
    headlines = grep('@',soil[6:length(soil)])
    soil.profile$SMKE = trim(substr(soil[5],55,60))
    soil.profile$SMPX = trim(substr(soil[5],49,54))
    soil.profile$SMHB = trim(substr(soil[5],43,48))
    soil.profile$SLPF = as.numeric(substr(soil[5],37,42))
    soil.profile$SLNF = as.numeric(substr(soil[5],31,36))
    soil.profile$SLRO = as.numeric(substr(soil[5],25,30))
    soil.profile$SLDR = as.numeric(substr(soil[5],19,24))
    soil.profile$SLU1 = as.numeric(substr(soil[5],13,18))
    soil.profile$SALB = as.numeric(substr(soil[5],7,12))
    soil.profile$SCOM = trim(substr(soil[5],1,6))
    soil.profile$SCS_FAMILY = trim(substr(soil[3],44,nchar(soil[3])))
    soil.profile$LONG = as.numeric(trim(substr(soil[3],36,42)))
    soil.profile$LAT = as.numeric(trim(substr(soil[3],28,34)))
    soil.profile$COUNTRY = trim(substr(soil[3],14,27))
    soil.profile$SITE = trim(substr(soil[3],1,12))
    soil.profile$SOIL_NAME = trim(substr(soil[1],38,nchar(soil[1])))
    soil.profile$DEPTH = as.integer(trim(substr(soil[1],34,36)))
    soil.profile$TEXTURE_CLASS = trim(substr(soil[1],26,28))
    soil.profile$SOURCE = trim(substr(soil[1],14,25))
    soil.profile$ID_SOIL = trim(substr(soil[1],2,11))
    soil.profile = create.soil.profile(empty=T)
    if(length(nextprof)>0) soil = soil[1:(nextprof[1]-1)]
    nextprof = grep('^\\*',soil[2:length(soil)])
    soil = soil[!soil==''&!grepl('^[[:space:]]$',soil)]
    soil = soil[!substr(soil,1,1)=='!']
    soil = soil[grep(soil_id,soil):length(soil)]
    soil = readLines(filename)
get.soil.profile <- function(soil_id,filename){

}
    return(nlines)
    nlines = nlines + length(filex)
    }
        }
            }
                }
                    }
                        nlines = nlines + nrow(filex[[i]][[j]][[k]]) + 1
                    }else if(is.data.frame(filex[[i]][[j]][[k]])){
                        nlines = nlines + 1
                    if(is.vector(filex[[i]][[j]][[k]])){
                for(k in 1:length(filex[[i]][[j]])){
            }else if(is.list(filex[[i]][[j]])){
                nlines = nlines + nrow(filex[[i]][[j]]) + 1
            }else if(is.data.frame(filex[[i]][[j]])){
                nlines = nlines + 1
            if(is.vector(filex[[i]][[j]])){
        for(j in 1:length(filex[[i]])){
    for (i in 1:length(filex)){
    nlines = 0
nlines.filex <- function(filex){

}
    return(nwwd)
    }
        number = signif(number,digits = nwwd)
        nwwd = nwwd - 1
    while(nchar(as.character(number))>width){
    nwwd = width
check.width <- function(number,width){

}
    return(sub.lines)
    }
        }
            sub.lines[k+1] = paste(subsection[k,],collapse='')
        for(k in 1:nrow(subsection)){
        }
            }
                subsection[,k] = paste(' ',subsection[,k],sep='')
            if((cnames.nchar[k]>1&k>1)|grepl('PAREA',cnames[k])){
            }
                subsection[,k] = format(subsection[,k],justify=just,width=widths[k])
            if(widths[k]<40){
            }
                subsection[,k] = paste(' ',subsection[,k],sep='')
            }else if(grepl('SANAME',cnames[k])){
                subsection[,k] = paste(' ',subsection[,k],sep='')
            }else if(grepl('ID_SOIL',cnames[k])){
                subsection[,k] = paste(paste(rep(' ',23),collapse=''),subsection[,k],sep='')
            if(grepl('PLNAME',cnames[k])){
            }
                just = 'right'
            }else{
                just = 'left'
            if(is.ljust[k]){
        for (k in 1:ncol(subsection)){
                                        return(x)})),ncol=length(cnames))
                                        x = gsub('^NA','-99',x)
                                        x = sub('^ *','',sub(' *$','',x))
        subsection = matrix(unlist(lapply(subsection,function(x){
        }
            }
                subsection[,k] = format(subsection[,k])
            }else{
                subsection[,k] = format(subsection[,k])
                }
                    }
                        subsection[l,k] = signif(subsection[l,k],digits=nwwd)
                        nwwd = check.width(subsection[l,k],widths[k])
                    for (l in 1:length(subsection[,k])){
                if(any(nchar(as.character(subsection[,k]))>widths[k])){
                subsection[,k] = signif(subsection[,k],digits=widths[k])
            }else if(class(subsection[,k])=='numeric'){
                subsection[,k] = format(subsection[,k])
                #subsection[,k] = sprintf(fmt,subsection[,k])
                #fmt = paste('%',widths[k],'.',widths[k],'i',sep='')
            if(class(subsection[,k])=='integer'){
        for(k in 1:ncol(subsection)){
        substr(sub.lines[1],1,1) = '@'
        sub.lines[1] = paste(cnames,collapse='')
        cnames[grepl('SANAME',cnames)] = '  SANAME'
        cnames[grepl('ID_SOIL',cnames)] = '  ID_SOIL   '
        cnames[grepl('PLNAME',cnames)] = paste(paste(rep(' ',24),collapse=''),'PLNAME',sep='')
        cnames = paste(' ',cnames,sep='')
        }
            }
                    justify=ifelse(cnames[k]%in%ljust,'left','right'))
                    width=ifelse(widths[k]<40,widths[k],nchar(cnames[k])),
                cnames[k] = format(cnames[k],
            if(cnames.nchar[k]>1){
        for (k in 1:length(cnames)){
        cnames = add.periods(cnames)
        }
            widths[cnames=='TNAME'] = 25
        if('TNAME'%in%cnames&'CU'%in%cnames){
        widths = get.widths(cnames)
        cnames.nchar = nchar(cnames)
        is.ljust = cnames %in% ljust
        ljust = list.ljust(cnames)
        cnames = colnames(subsection)
        sub.lines = vector(length=nrow(subsection)+1,mode='character')
    }else{
        sub.lines = as.character(subsection)
        sub.lines = vector(length=1,mode='character')
    if(is.vector(subsection)){
write.subsection <- function(subsection){

}
    return(invisible(gen.out))
    write(gen.out,file=file.name)
    }
        }
            linenum = linenum + 1
            gen.out[linenum] = gen[[2]][i]
        for(i in 1:cn){
    if(cn>0){
    }
        }
            linenum = linenum + 1
            gen.out[linenum] = paste(section[l,],collapse='')
        for(l in 1:nrow(section)){
        }
            section[,c] = sprintf(fmt[c],gen[[3]][[i]][,c])
        for(c in 1:ncol(gen[[3]][[i]])){
        section = gen[[3]][[i]]
        linenum = linenum + 1
            paste(sprintf(hfmt,colnames(gen[[3]][[i]])),collapse='')
        gen.out[linenum] = 
        hfmt[1] = gsub('%','@%',gsub(orig,adj,hfmt[1]))
        if(adj<0) adj = adj + 2
        adj = as.numeric(orig)-1
                   gsub('s','',hfmt[1]))))
        orig = gsub(' *','',gsub('%','',gsub('\\..*','',
        hfmt = gsub('\\..*','s',fmt)
        }
            fmt[colnames(gen[[3]][[i]])=='ECO#']=' %6s'
        if(type=='CUL'){
                  type=rep(type,ncol(gen[[3]][[i]])))
                  model=rep(model,ncol(gen[[3]][[i]])),
        fmt = get.fmt(colnames(gen[[3]][[i]]),
    for(i in 1:length(gen[[3]])){
    }
        linenum = linenum + 1
        gen.out[linenum] = gen[[1]][i]
    for(i in 1:tn){
    linenum = 1
    gen.out = vector(length=tn+cn+pn,mode='character')
    pn = sum(unlist(lapply(cul[[3]],nrow)))+length(cul[[3]])
    cn = length(gen[[2]])
    tn = length(gen[[1]])
    }
        type = toupper(type)
        type = substr(file.name,nchar(file.name)-2,nchar(file.name))
    if(is.na(type)){
    }
        }
            stop('Please provide model name.')
        }else{
            model = 'CERES'
        }else if(grepl('CER',file.name)){
            model = 'CROPSIM'
        }else if(grepl('CRP',file.name)){
            model='CROPGRO'
        if(grepl('GRO',file.name)){
    if(is.na(model)){
write.gen <- function(gen,file.name,model=NA,type=NA){

}
    return(gen)
    gen = list(title=title,comments=comments,parameters=parameters)
    }
            parameters[[i]][econum!='      '&!is.na(econum),]
        parameters[[i]]=
        econum=gsub('  *','',parameters[[i]]$`ECO#`)
        colnames(parameters[[i]]) = header[[i]]
        parameters[[i]]=na.omit(params)
        }
            }
                params[,j]=gsub(' *','',params[,j])
            }else{
                params[,j]=as.numeric(params[,j])
            if(classes[j]=='numeric'){
        for(j in 1:ncol(params)){
        params=as.data.frame(params)
        }
            params[,c]=substr(section,pos1[c],pos2[c])
        for(c in 1:length(widths)){
        pos2=cumsum(widths)
        pos1=cumsum(c(1,widths[1:(length(widths)-1)]))
        params=matrix(ncol=length(widths),nrow=length(section))
        section=section[substr(section,1,1)!='!']
        section=section[section!='']
                     ifelse(i==length(hlines),length(gen),hlines[i+1]-hlines[i])]
        section=gen[(hlines[i]+1):
        classes = gsub('.*d.*','numeric',classes)
        classes = gsub('.*f.*','numeric',classes)
        classes = gsub('.*s.*','character',classes)
        classes = gsub('.*e.*','numeric',classes)
        widths = as.numeric(widths)+spaces
        widths = gsub('\\..*','',widths)
        widths = gsub('[a-z].*','',widths)
        widths = gsub('%-*','',classes)
        spaces = nchar(classes)-nchar(gsub(' *','',classes))
                      type=rep(type,length(header[[i]])))
                      model=rep(model,length(header[[i]])),
        classes = get.fmt(header[[i]],
    for (i in 1:length(hlines)){
    parameters = vector(length=length(hlines),mode='list')
              })
                  return(x)
                  x=gsub('@','',gsub('\\.','',x))
                  x=strsplit(x,split='  *')[[1]]
    header = lapply(gen[hlines],FUN=function(x){
    hlines = (1:length(gen))[first.char=='@']
    comments = gen[first.char=='!']
    title = gen[first.char=='*']
    first.char = substr(gen,1,1)
    gen = readLines(file.name)
    }
        type = tolower(type)
        type = substr(file.name,nchar(file.name)-2,nchar(file.name))
    if(is.na(type)){
    }
        }
            stop('Please provide model name.')
        }else{
            model = 'CERES'
        }else if(grepl('CER',file.name)){
            model = 'CROPSIM'
        }else if(grepl('CRP',file.name)){
            model='CROPGRO'
        if(grepl('GRO',file.name)){
    if(is.na(model)){
read.gen <- function(file.name,model=NA,type=NA){

}
    return(fmt.out)
    }
            FORMAT[MODEL==model[i]&TYPE==type[i]&PNAME==prm.name[i]])
        fmt.out[i]=with(fmt,
    for(i in 1:length(prm.name)){
    fmt.out = vector(length=length(prm.name),mode='character')
"PNAME", "FORMAT", "LPOS1", "TYPE"), row.names = c(NA, -766L), class = "data.frame")
"ECO", "ECO", "ECO", "ECO", "ECO", "ECO")), .Names = c("MODEL", 
"ECO", "ECO", "ECO", "ECO", "ECO", "ECO", "ECO", "ECO", "ECO", 
"ECO", "ECO", "ECO", "ECO", "ECO", "ECO", "ECO", "ECO", "ECO", 
"CUL", "ECO", "ECO", "ECO", "ECO", "ECO", "ECO", "ECO", "ECO", 
"CUL", "CUL", "CUL", "CUL", "CUL", "CUL", "CUL", "CUL", "CUL", 
"CUL", "CUL", "CUL", "CUL", "CUL", "CUL", "CUL", "CUL", "CUL", 
"FLX", "FLX", "SOL", "SOL", "CUL", "CUL", "CUL", "CUL", "CUL", 
"ECO", "ECO", "ECO", "FLX", "FLX", "FLX", "FLX", "FLX", "FLX", 
"ECO", "ECO", "ECO", "ECO", "ECO", "ECO", "ECO", "ECO", "ECO", 
"ECO", "ECO", "ECO", "ECO", "ECO", "ECO", "ECO", "ECO", "ECO", 
"ECO", "ECO", "ECO", "ECO", "ECO", "ECO", "ECO", "ECO", "ECO", 
"CUL", "CUL", "CUL", "CUL", "CUL", "CUL", "ECO", "ECO", "ECO", 
"CUL", "CUL", "CUL", "CUL", "CUL", "CUL", "CUL", "CUL", "CUL", 
"SPE", "CUL", "CUL", "CUL", "CUL", "CUL", "CUL", "CUL", "CUL", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"ECO", "ECO", "ECO", "ECO", "ECO", "SPE", "SPE", "SPE", "SPE", 
"ECO", "ECO", "ECO", "ECO", "ECO", "ECO", "ECO", "ECO", "ECO", 
"CUL", "CUL", "CUL", "ECO", "ECO", "ECO", "ECO", "ECO", "ECO", 
"CUL", "CUL", "CUL", "CUL", "CUL", "CUL", "CUL", "CUL", "CUL", 
"CUL", "CUL", "CUL", "CUL", "CUL", "CUL", "CUL", "CUL", "CUL", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "CUL", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"ECO", "ECO", "ECO", "ECO", "ECO", "ECO", "ECO", "SPE", "SPE", 
"ECO", "ECO", "ECO", "ECO", "ECO", "ECO", "ECO", "ECO", "ECO", 
"ECO", "ECO", "ECO", "ECO", "ECO", "ECO", "ECO", "ECO", "ECO", 
"CUL", "CUL", "CUL", "CUL", "CUL", "CUL", "CUL", "ECO", "ECO", 
"CUL", "CUL", "CUL", "CUL", "CUL", "CUL", "CUL", "CUL", "CUL", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "CUL", "CUL", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
181L, 187L, 193L, 199L), TYPE = c("SPE", "SPE", "SPE", "SPE", 
115L, 121L, 127L, 133L, 139L, 145L, 151L, 157L, 163L, 169L, 175L, 
43L, 49L, 55L, 61L, 67L, 73L, 79L, 85L, 91L, 97L, 103L, 109L, 
115L, 121L, 127L, 133L, 139L, 145L, 151L, 1L, 8L, 25L, 31L, 37L, 
43L, 49L, 55L, 61L, 67L, 73L, 79L, 85L, 91L, 97L, 103L, 109L, 
15L, 27L, 15L, 45L, 27L, 51L, 37L, 31L, 1L, 8L, 25L, 30L, 37L, 
352L, 366L, 381L, 396L, 411L, 426L, 441L, 456L, 471L, 51L, 21L, 
190L, 205L, 220L, 235L, 250L, 265L, 280L, 295L, 306L, 322L, 337L, 
26L, 41L, 56L, 71L, 86L, 101L, 116L, 131L, 146L, 161L, 175L, 
197L, 212L, 227L, 242L, 257L, 272L, 287L, 302L, 317L, 1L, 8L, 
33L, 48L, 63L, 78L, 93L, 108L, 123L, 138L, 152L, 167L, 182L, 
22L, 22L, 22L, 22L, 22L, 22L, 22L, 22L, 22L, 22L, 1L, 8L, 27L, 
22L, 22L, 22L, 22L, 22L, 22L, 23L, 22L, 22L, 23L, 23L, 23L, 22L, 
55L, 61L, 67L, 73L, 79L, 85L, 91L, 97L, 103L, 109L, 115L, 121L, 
121L, 127L, 133L, 139L, 1L, 8L, 26L, 29L, 31L, 37L, 43L, 49L, 
49L, 55L, 61L, 67L, 73L, 79L, 85L, 91L, 97L, 103L, 109L, 115L, 
1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 7L, 1L, 8L, 25L, 31L, 37L, 43L, 
7L, 13L, 19L, 25L, 1L, 7L, 13L, 19L, 25L, 1L, 7L, 13L, 19L, 25L, 
1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 7L, 13L, 19L, 25L, 31L, 1L, 
7L, 13L, 19L, 1L, 7L, 13L, 19L, 1L, 7L, 13L, 1L, 25L, 1L, 25L, 
19L, 25L, 31L, 37L, 1L, 7L, 13L, 19L, 25L, 31L, 37L, 43L, 1L, 
1L, 7L, 13L, 19L, 25L, 1L, 7L, 1L, 25L, 1L, 25L, 1L, 7L, 13L, 
1L, 1L, 7L, 13L, 19L, 25L, 1L, 7L, 13L, 19L, 1L, 1L, 1L, 1L, 
25L, 1L, 25L, 1L, 25L, 1L, 1L, 1L, 1L, 7L, 13L, 19L, 25L, 31L, 
7L, 13L, 19L, 1L, 7L, 13L, 19L, 25L, 31L, 1L, 25L, 1L, 25L, 1L, 
25L, 31L, 1L, 7L, 13L, 19L, 1L, 7L, 13L, 19L, 25L, 31L, 1L, 1L, 
19L, 25L, 31L, 1L, 7L, 13L, 19L, 25L, 31L, 1L, 7L, 13L, 19L, 
1L, 7L, 13L, 19L, 25L, 31L, 1L, 7L, 13L, 19L, 25L, 1L, 7L, 13L, 
31L, 1L, 7L, 13L, 19L, 25L, 31L, 1L, 7L, 13L, 19L, 25L, 31L, 
13L, 19L, 1L, 7L, 13L, 19L, 25L, 31L, 1L, 7L, 13L, 19L, 25L, 
7L, 13L, 19L, 1L, 7L, 13L, 19L, 25L, 1L, 1L, 1L, 13L, 1L, 7L, 
7L, 13L, 1L, 7L, 13L, 1L, 25L, 1L, 25L, 1L, 1L, 1L, 25L, 1L, 
109L, 115L, 121L, 127L, 133L, 139L, 145L, 151L, 157L, 163L, 1L, 
37L, 43L, 49L, 55L, 61L, 67L, 73L, 79L, 85L, 91L, 97L, 103L, 
79L, 85L, 91L, 97L, 103L, 109L, 115L, 1L, 7L, 25L, 28L, 31L, 
19L, 25L, 1L, 8L, 25L, 31L, 37L, 43L, 49L, 55L, 61L, 67L, 73L, 
13L, 19L, 25L, 1L, 7L, 13L, 19L, 25L, 1L, 7L, 13L, 1L, 7L, 13L, 
1L, 1L, 1L, 1L, 7L, 1L, 7L, 1L, 1L, 7L, 13L, 19L, 25L, 1L, 7L, 
1L, 7L, 13L, 19L, 25L, 1L, 7L, 13L, 19L, 25L, 1L, 1L, 1L, 1L, 
1L, 1L, 1L, 1L, 7L, 13L, 19L, 25L, 31L, 1L, 7L, 13L, 19L, 25L, 
13L, 19L, 1L, 7L, 13L, 1L, 25L, 1L, 25L, 1L, 1L, 1L, 1L, 1L, 
1L, 7L, 13L, 19L, 25L, 31L, 37L, 43L, 1L, 7L, 13L, 19L, 1L, 7L, 
1L, 7L, 13L, 1L, 25L, 1L, 25L, 1L, 7L, 13L, 19L, 25L, 31L, 37L, 
25L, 1L, 7L, 13L, 19L, 1L, 1L, 1L, 1L, 1L, 7L, 13L, 19L, 25L, 
1L, 7L, 1L, 7L, 13L, 19L, 25L, 1L, 1L, 1L, 1L, 1L, 7L, 13L, 19L, 
25L, 1L, 25L, 1L, 1L, 1L, 1L, 7L, 13L, 19L, 25L, 31L, 1L, 1L, 
19L, 1L, 7L, 13L, 19L, 25L, 31L, 1L, 25L, 1L, 25L, 1L, 25L, 1L, 
25L, 1L, 25L, 1L, 25L, 1L, 25L, 1L, 7L, 13L, 19L, 1L, 7L, 13L, 
1L, 7L, 13L, 19L, 25L, 1L, 7L, 13L, 19L, 25L, 31L, 1L, 25L, 1L, 
7L, 13L, 19L, 1L, 7L, 13L, 19L, 1L, 7L, 13L, 19L, 25L, 31L, 1L, 
1L, 7L, 13L, 1L, 7L, 13L, 19L, 25L, 1L, 7L, 13L, 19L, 25L, 1L, 
31L, 1L, 7L, 13L, 19L, 25L, 31L, 1L, 7L, 13L, 19L, 25L, 31L, 
19L, 25L, 31L, 1L, 7L, 13L, 19L, 25L, 1L, 7L, 13L, 19L, 25L, 
13L, 19L, 25L, 31L, 1L, 7L, 13L, 19L, 25L, 31L, 1L, 7L, 13L, 
1L, 6L, 13L, 19L, 25L, 31L, 1L, 7L, 13L, 19L, 25L, 31L, 1L, 7L, 
1L, 7L, 13L, 19L, 25L, 31L, 1L, 7L, 1L, 13L, 25L, 1L, 13L, 25L, 
25L, 1L, 1L, 1L, 7L, 13L, 19L, 25L, 1L, 13L, 1L, 7L, 13L, 19L, 
1L, 25L, 1L, 1L, 1L, 25L, 1L, 7L, 13L, 19L, 1L, 7L, 13L, 19L, 
"%6.2f", "%6.1f"), LPOS1 = c(1L, 7L, 13L, 1L, 7L, 13L, 1L, 25L, 
"%6.0f", "%6.1f", "%6.1f", "%6.1f", "%6.1f", "%6.1f", "%6.3f", 
"%6.2f", "%6.1f", "%6.0f", "%6.1f", "%6.2f", "%6.0f", "%6.1f", 
"%6.1f", "%6.1f", "%6.1f", "%6.1f", "%6.1f", "%6.1f", "%6.1f", 
"%6.1f", "%6.1f", "%6.1f", "%6.1f", "%6.0f", "%6.1f", "%6.1f", 
"%6.2f", "%6.2f", "%6.0f", "%6.1f", "%6.0f", "%-6s ", "%-16s ", 
"%6.0f", "%6.0f", "%6.0f", "%6.0f", "%6.0f", "%6.0f", "%6.1f", 
"%6.0f", "%6.0f", "%6.0f", "%6.0f", "%6.1f", "%6.0f", "%6.0f", 
"%6.2f", "%6.3f", "%-6s ", "%-16s ", "%-5s", "%-7s", "%6.0f", 
"%6.3f", "%6.3f", "%6.1f", "%6.0f", "%6.0f", "%6.2f", "%6.2f", 
"%15.1f", "%15.3f", "%15.1f", "%15.1f", "%15.0f", "%15.1f", "%6.3f", 
"%11.3f", "%16.0f", "%15.0f", "%15.0f", "%14.3f", "%15.0f", "%15.2f", 
"%15.1f", "%15.0f", "%15.4f", "%15.3f", "%15.2f", "%15.3f", "%15.5f", 
"%15.0f", "%15.3f", "%15.2f", "%15.2f", "%15.0f", "%14.0f", "%15.1f", 
"%15.0f", "%-6s ", "%-18s", "%15.2f", "%15.1f", "%15.1f", "%15.0f", 
"%15.0f", "%15.0f", "%15.0f", "%15.0f", "%15.0f", "%15.0f", "%15.1f", 
"%15.0f", "%15.0f", "%14.0f", "%15.0f", "%15.0f", "%15.0f", "%15.0f", 
"%-19s", "%-6s", "%15.2f", "%15.2f", "%15.2f", "%15.2f", "%15.0f", 
"%5.2f", "%3.0f", "%3.0f", "%5.2f", "%3.0f", "%5.0f", "%-7s", 
"%7.2f", "%5.2f", "%3.0f", "%3.0f", "%5.2f", "%6.3f", "%6.3f", 
"%5.2f", "%4.1f", "%6.3f", "%7.2f", "%5.2f", "%5.2f", "%5.0f", 
"%6.1f", "%6.3f", "%6.3f", "%6.3f", "%3.0f", "%4.1f", "%5.0f", 
"%6.2f", "%6.2f", "%6.2f", "%6.2f", "%6.2f", "%6.2f", "%6.1f", 
"%-2s", "%-2s", "%6.2f", "%6.2f", "%6.2f", "%6.2f", "%6.2f", 
"%6.2f", "%6.1f", "%6.1f", "%6.3f", "%6.3f", "%-6s ", "%-17s ", 
"%6.2f", "%6.2f", "%6.0f", "%6.1f", "%6.2f", "%6.2f", "%6.1f", 
"%-6s", "%6.2f", "%6.2f", "%6.1f", "%6.1f", "%6.1f", "%6.2f", 
"%6.2f", "%6.2f", "%6.2f", "%6.1f", "%-6s ", "%-16s ", "%-5s", 
"%6.1f", "%16d", "%6.2f", "%6.3f", "%6.3f", "%6.1f", "%6.2f", 
"%6.1f", "%6.1f", "%6.1f", "%16d", "%6.1f", "%6.1f", "%6.1f", 
"%6.2f", "%6.1f", "%6.1f", "%6.1f", "%6.1f", "%16d", "%6.1f", 
"%6.2f", "%6.2f", "%-6s", "%6.1f", " %5.4f", " %5.3f", "%6.2f", 
"%-6s", "%6.2f", "%6.2f", "%6.2f", "%6.2f", "%6.2f", "%6.2f", 
"%6.0f", "%6.1f", "%6.2f", "%6.2f", "%6.1f", "%-6s", "%6.1f", 
"%6.3f", "%6.3f", "%6.2f", "%6.2f", "%6.2f", "%6.2f", "%6.2f", 
"%6.2f", "%6.1f", "%6.2f", "%6.1f", "%6.2f", "%6.1f", "%6.2f", 
"%6.0f", "%6.3f", "%6.1f", "%6.2f", "%6.2f", "%6.2f", "%6.1f", 
"%6.2f", "%6.1f", "%6.1f", "%6.1f", "%6.2f", "%6.1f", "%6.1f", 
"%6.1f", "%6.2f", "%6.2f", "%6.2f", "%6.2f", "%6.1f", "%6.1f", 
"%6.1f", "%6.0f", "%6.0f", "%6.3f", "%6.2f", "%6.1f", "%6.1f", 
"%6.2f", "%6.2f", "%6.2f", "%6.0f", "%6.0f", "%6.1f", "%6.1f", 
"%6.1f", "%6.2f", "%6.2f", "%6.2f", "%6.2f", "%6.2f", "%6.2f", 
"%6.2f", "%-6s", "%6.2f", "%-6s", "%6.2f", "%-6s", "%6.2f", "%-6s", 
"%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.2f", "%-6s", 
"%6.3f", "%-6s", "%6.2f", "%6.2f", "%6.2f", "%6.2f", "%6.3f", 
"%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.2f", 
"%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.2f", 
"%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", 
"%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", 
"%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", 
"%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", 
"%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", 
"%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", 
"%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.2f", "%6.2f", 
"%6.3f", "%6.3f", "%12.1e", " %5.4f", "%6.3f", "%6.3f", "%6.3f", 
"%6.2f", "%6.1f", " %5.4f", " %5.4f", " %5.4f", "%6.2f", "%6.3f", 
"%-6s", "%6.1f", "%6.1f", "%6.1f", "%-6s", " %5.4f", "%6.2f", 
"%6.2f", "%6.1f", "%6.2f", " %5.4f", "%6.2f", "%-6s", "%6.1f", 
"%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.1f", "%6.1f", 
"%6.1f", "%6.1f", "%6.1f", "%6.3f", "%6.3f", "%6.0f", "%6.3f", 
"%6.0f", "%6.1f", "%6.2f", "%6.1f", "%6.0f", "%6.0f", "%6.2f", 
"%-17s", "%-3s", "%-3s", "%6.2f", "%6.1f", "%6.1f", "%6.1f", 
"%6.1f", "%6.2f", "%6.2f", "%6.1f", "%6.2f", "%6.1f", "%-6s", 
"%6.1f", "%6.1f", "%6.1f", "%6.2f", "%6.2f", "%6.2f", "%6.0f", 
"%6.1f", "%6.2f", "%-6s", "%-6s", "%-17s", "%-7s", "%6.2f", "%6.3f", 
"%6.3f", "%-6s", "%6.1f", "%6.1f", "%6.3f", "%6.1f", "%6.1f", 
"%6.1f", "%6.2f", "%6.3f", "%-6s", "%6.2f", "%6.2f", "%6.2f", 
"%6.3f", "%6.1f", "%6.1f", "%6.1f", "%6.3f", "%-6s", "%6.1f", 
"%6.2f", "%6.2f", "%6.2f", "%6.2f", "%6.1f", "%6.3f", "%6.3f", 
"%6.1f", "%6.1f", "%16d", "%6.2f", "%6.3f", "%6.3f", "%6.1f", 
"%6.1f", "%6.1f", "%6.1f", "%6.1f", "%16d", "%6.1f", "%6.1f", 
"%6.2f", "%6.2f", "%6.1f", "%6.1f", "%6.1f", "%6.1f", "%16d", 
"%6.2f", "%6.2f", "%6.2f", "%-6s", "%6.1f", " %5.4f", " %5.3f", 
"%6.1f", "%-6s", "%6.2f", "%6.2f", "%6.2f", "%6.2f", "%6.2f", 
"%6.2f", "%6.0f", "%6.1f", "%6.2f", "%6.2f", "%6.1f", "%-6s", 
"%6.2f", "%6.3f", "%6.3f", "%6.2f", "%6.2f", "%6.2f", "%6.2f", 
"%6.1f", "%6.2f", "%6.1f", "%6.2f", "%6.1f", "%6.2f", "%6.1f", 
"%6.1f", "%6.0f", "%6.3f", "%6.1f", "%6.2f", "%6.2f", "%6.2f", 
"%6.2f", "%6.1f", "%6.2f", "%6.1f", "%6.1f", "%6.2f", "%6.1f", 
"%6.1f", "%6.2f", "%6.2f", "%6.2f", "%6.2f", "%6.1f", "%6.1f", 
"%6.1f", "%6.0f", "%6.0f", "%6.3f", "%6.2f", "%6.1f", "%6.1f", 
"%6.2f", "%6.2f", "%6.2f", "%6.0f", "%6.0f", "%6.1f", "%6.1f", 
"%6.2f", "%6.2f", "%6.2f", "%6.2f", "%6.2f", "%6.2f", "%6.1f", 
"%6.2f", "%6.2f", "%6.2f", "%6.2f", "%6.2f", "%6.2f", "%6.2f", 
"%6.2f", "%-6s", "%6.1f", "%6.2f", "%6.2f", "%6.2f", "%6.2f", 
"%6.2f", "%-6s", "%6.2f", "%-6s", "%6.2f", "%-6s", "%6.2f", "%-6s", 
"%6.2f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", 
"%6.2f", "%6.2f", "%6.2f", "%6.2f", "%6.2f", "%6.2f", "%6.2f", 
"%6.2f", "%-6s", "%6.2f", "%-6s", "%6.2f", "%-6s", "%6.2f", "%-6s", 
"%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.2f", "%-6s", 
"%-6s", "%6.2f", "%6.2f", "%6.2f", "%6.2f", "%6.2f", "%6.3f", 
"%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.2f", "%6.3f", 
"%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.2f", "%6.3f", 
"%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", 
"%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", 
"%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", 
"%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", 
"%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", 
"%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", 
"%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", 
"%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", 
"%6.1f", "%-6s", "%6.1f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", 
"%12.1e", "%12.1e", "%12.1e", "%12.1e", "%5.4f", " %6.4f", " %5.4f", 
"%6.3f", "%6.2f", "%6.2f", "%-6s", "%-6s", "%12.1e", "%12.1e", 
"%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", 
"%6.1f", "%6.1f", "%6.3f", "%6.3f", "%-4s", "%12.1e", " %5.4f", 
" %5.4f", " %5.4f", " %5.4f", "%6.2f", "%6.2f", "%6.3f", "%6.3f", 
"%6.1f", "%6.1f", "%-6s", " %5.4f", "%6.2f", "%6.2f", "%6.1f", 
"%6.2f", " %5.4f", "%6.2f", "%-6s", "%6.1f", "%-6s", "%6.1f", 
"NUPNF", "NUPWF"), FORMAT = c("%6.1f", "%6.1f", "%6.2f", "%6.1f", 
"GN%S", "TKFH", "SSPHS", "SSPHE", "SHWTA", "GWWF", "G#SF", "RTNUP", 
"TDPHE", "TDFAC", "TDSF", "RDGS", "HTSTD", "AWNS", "KCAN", "RS%A", 
"LSENI", "LSPHS", "LSPHE", "TIL#S", "TIPHE", "TIFAC", "TDPHS", 
"ECO#", "ECONAME", "PARUE", "PARU2", "PHL2", "PHF3", "SLAS", 
"P5", "P6", "P7", "LA1S", "LAFV", "LAFR", "VBASE", "VEFF", "PPS2", 
"P8", "G#WTS", "GWTS", "SHWTS", "PHINT", "P1", "P2", "P3", "P4", 
"SLPF", "SRGF", "VAR#", "VAR-NAME", "EXP#", "ECO#", "VREQ", "PPS1", 
"SASC", "SANI", "SAOC", "FAMN", "ICRT", "ICRES", "ICRN", "ICREN", 
"LMAX_CF(1)", "LMAX_CF(2)", "LMAX_CF(3)", "MAXLFLENGTH", "MAXLFWIDTH", 
"TTBASEPOP", "TBASEPER", "LG_AMRANGE", "LG_GP_REDUC", "LDG_FI_REDUC", 
"POPCF(1)", "POPCF(2)", "POPDECAY", "TTBASEEM", "TTBASELFEX", 
"AREAMX_CF(3)", "WIDCOR", "WMAX_CF(1)", "WMAX_CF(2)", "WMAX_CF(3)", 
"dPERdT", "EXTCFN", "EXTCFST", "LFNMXEXT", "AREAMX_CF(1)", "AREAMX_CF(2)", 
"DELTTMAX", "SWDF2AMP", "CS_CNREDUC", "CS_CNPERIOD", "Tthalfa", 
"TT_POPGROWTH", "MAX_POP", "POPTT16", "LG_AMBASE", "ECO#", "ECO-NAME", 
"PI1", "PI2", "PSWITCH", "TTPLNTEM", "TTRATNEM", "CHUPIBASE", 
"SUCA", "TBFT", "Tthalfo", "TBase", "LFMAX", "MXLFAREA", "MXLFARNO", 
"VAR#", "VAR-NAME", "ECO#", "MaxPARCE", "APFMX", "STKPFMAX", 
"EORATIO", "RWUEP1", "RWUEP2", "RWUMX", "LG_RATING", "LG_CRIT_WIND", 
"RLVMIN", "SENESF", "RESET", "PERCoeff", "CHTCoeff", "HillPar1", 
"RESPGCF", "MAX_ROOTPF", "FTCON", "SURCON", "RTCMPG", "WRK", 
"SLOBI", "TBasePhotos", "CRITSW", "HuRecover", "RespQ10", "PCB", 
"R7-R8", "FL-VS", "TRIFL", "RWDTH", "RHGHT", "R1PPO", "OPTBI", 
"PL-EM", "EM-V1", "V1-JU", "JU-R0", "PM06", "PM09", "LNGSH", 
"THRSH", "SDPRO", "SDLIP", "ECO#", "ECONAME", "MG", "TM", "THVAR", 
"SLAVR", "SIZLF", "XFRT", "WTPSD", "SFDUR", "SDPDV", "PODUR", 
"PPSEN", "EM-FL", "FL-SH", "FL-SD", "SD-PM", "FL-LF", "LFMAX", 
"KEP", "EORATIO", "VAR#", "VRNAME", "EXPNO", "ECO#", "CSDL", 
"XVSHT", "YVSHT", "YVSWH", "XHWTEM", "YHWTEM", "XHWPAR", "YHWPAR", 
"LATE(TB)", "LATE(TO1)", "LATE(TO2)", "LATE(TM)", "LATE(I)", 
"EARLY(TB)", "EARLY(TO1)", "EARLY(TO2)", "EARLY(TM)", "EARLY(I)", 
"VEGET(TB)", "VEGET(TO1)", "VEGET(T02)", "VEGET(TM)", "VEGET(I)", 
"YTRFAC", "DETACH", "DWC", "PR1DET", "PR2DET", "XP1DET", "XP2DET", 
"XXFTEM", "YXFTEM", "XSWFAC", "YSWFAC", "XSWBAR", "YSWBAR", "XTRFAC", 
"DSWBAR", "XFRMAX", "SHLAG", "FNPDT", "TYPPDT", "FNSDT", "TYPSDT", 
"RTNH4", "PORMIN", "RTEXF", "SETMAX", "SRMAX", "RFLWAB", "XMPAGE", 
"XRTFAC(3)", "YRTFAC(3)", "XRTFAC(4)", "YRTFAC(4)", "RTNO3", 
"RWUEP1", "RWUMX", "XRTFAC(1)", "YRTFAC(1)", "XRTFAC(2)", "YRTFAC(2)", 
"SENPOR", "SENMAX", "RTDEPI", "RFAC1", "RTSEN", "RLDSM", "RTSDF", 
"SENDAY", "FREEZ1", "FREEZ2", "ICMP", "TCMP", "XSTAGE", "XSENMX", 
"TURSLA", "XVGROW", "YVREF", "XSLATM", "YSLATM", "SENRTE", "SENRT2", 
"SLAREF", "SIZREF", "VSSINK", "EVMODC", "SLAMAX", "SLAMIN", "SLAPAR", 
"PORPT", "FRSTMF", "FRLFF", "ATOP", "FRCNOD", "FRLFMX", "FINREF", 
"TYPFXW", "FNFXA", "TYPFXA", "XLEAF", "YLEAF", "YSTEM", "WTFSD", 
"FNNGT", "TYPNGT", "FNFXT", "TYPFXT", "FNFXD", "TYPFXD", "FNFXW", 
"ALPHSH", "SNACTM", "NODRGM", "DWNODI", "TTFIX", "NDTHMX", "CNODCR", 
"NMOBMX", "NVSMOB", "NRCVR", "XPODF", "ALPHL", "ALPHS", "ALPHR", 
"LIPOPT", "SLOSUM*100", "CARMIN", "CMOBMX", "CADSTF", "CADPR1", 
"PMINLF", "PMINST", "PMINRT", "PMINSH", "PMINSD", "PMINNO", "LIPTB", 
"PLIGNO", "POALF", "POAST", "POART", "POASH", "POASD", "POANO", 
"PLIPSH", "PLIPNO", "PLIGLF", "PLIGST", "PLIGRT", "PLIGSH", "PLIGSD", 
"PCARRT", "PCARSH", "PCARSD", "PCARNO", "PLIPLF", "PLIPST", "PLIPRT", 
"SDPROG", "PRONOD", "PROMIN", "PROMAX", "THETA", "PCARLF", "PCARST", 
"PRORTI", "PRORTG", "PRORTF", "PROSHI", "PROSHG", "PROSHF", "SDPROS", 
"PCH2O", "PROLFI", "PROLFG", "PROLFF", "PROSTI", "PROSTG", "PROSTF", 
"RNH4C", "RPRO", "RFIXN", "RCH20", "RLIP", "RLIG", "ROA", "RMIN", 
"LNREF", "PGREF", "XPGSLW", "YPGSLW", "RES30C", "R30C2", "RNO3C", 
"PGEFF", "SCV", "KDIF", "LFANGB", "SLWREF", "SLWSLO", "NSLOPE", 
"TYPPGN", "FNPGT", "TYPPGT", "XLMAXT", "YLMAXT", "FNPGL", "TYPPGL", 
"PARMAX", "PHTMAX", "KCAN", "CCMP", "CCMAX", "CCEFF", "FNPGN", 
"R1PPO", "OPTBI", "SLOBI", "RDRMT", "RDRMG", "RDRMM", "RCHDP", 
"FL-VS", "TRIFL", "RWDTH", "RHGHT", "THRSH", "SDPRO", "SDLIP", 
"EM-V1", "V1-JU", "JU-R0", "PM06", "PM09", "LNGSH", "R7-R8", 
"SDPDV", "PODUR", "ECO#", "ECONAME", "MG", "TM", "THVAR", "PL-EM", 
"FL-LF", "LFMAX", "SLAVR", "SIZLF", "XFRT", "WTPSD", "SFDUR", 
"ECO#", "CSDL", "PPSEN", "EM-FL", "FL-SH", "FL-SD", "SD-PM", 
"FRZHRD(2)", "FRZHRD(3)", "FRZHRD(4)", "TYPHRD", "VAR#", "VRNAME", 
"FMPGD(4)", "TYPPGD", "HARD1", "HARD2", "FRZDC", "FRZHRD(1)", 
"FNPMD(3)", "FNPMD(4)", "TYPPMD", "FMPGD(1)", "FMPGD(2)", "FMPGD(3)", 
"FNPTD(2)", "FNPTD(3)", "FNPTD(4)", "TYPPTD", "FNPMD(1)", "FNPMD(2)", 
"KEP", "EORATIO", "STRSRFL", "STRLYR1", "SENSR", "FNPTD(1)", 
"XVSHT", "YVSHT", "YVSWH", "XHWTEM", "YHWTEM", "XHWPAR", "YHWPAR", 
"LATE(TB)", "LATE(TO1)", "LATE(TO2)", "LATE(TM)", "LATE(I)", 
"EARLY(TB)", "EARLY(TO1)", "EARLY(TO2)", "EARLY(TM)", "EARLY(I)", 
"VEGET(TB)", "VEGET(TO1)", "VEGET(T02)", "VEGET(TM)", "VEGET(I)", 
"YTRFAC", "DETACH", "DWC", "PR1DET", "PR2DET", "XP1DET", "XP2DET", 
"XXFTEM", "YXFTEM", "XSWFAC", "YSWFAC", "XSWBAR", "YSWBAR", "XTRFAC", 
"DSWBAR", "XFRMAX", "SHLAG", "FNPDT", "TYPPDT", "FNSDT", "TYPSDT", 
"RTNH4", "PORMIN", "RTEXF", "SETMAX", "SRMAX", "RFLWAB", "XMPAGE", 
"XRTFAC(3)", "YRTFAC(3)", "XRTFAC(4)", "YRTFAC(4)", "RTNO3", 
"RWUMX", "XRTFAC(1)", "YRTFAC(1)", "XRTFAC(2)", "YRTFAC(2)", 
"SENMAX", "RTDEPI", "RFAC1", "RTSEN", "RLDSM", "RTSDF", "RWUEP1", 
"FREEZ2", "ICMP", "TCMP", "LFSEN", "XSTAGE", "XSENMX", "SENPOR", 
"YVREF", "XSLATM", "YSLATM", "SENRTE", "SENRT2", "SENDAY", "FREEZ1", 
"VSSINK", "EVMODC", "SLAMAX", "SLAMIN", "SLAPAR", "TURSLA", "XVGROW", 
"XLFEST", "YLFEST", "YSTEST", "YSREST", "FINREF", "SLAREF", "SIZREF", 
"FRSTRF", "FRSTRMX", "PWLF", "PWST", "PWRT", "PWSR", "MXWST", 
"PORPT", "FRSTMF", "FRLFF", "ATOP", "FRCNOD", "FRLFMX", "YSTOR", 
"TYPFXW", "FNFXA", "TYPFXA", "XLEAF", "YLEAF", "YSTEM", "WTFSD", 
"FNNGT", "TYPNGT", "FNFXT", "TYPFXT", "FNFXD", "TYPFXD", "FNFXW", 
"SENCSRV", "SNACTM", "NODRGM", "DWNODI", "TTFIX", "NDTHMX", "CNODCR", 
"SENCLV", "SENNSV", "SENCSV", "SENNRV", "SENCRV", "SENNSRV", 
"TYPCREF", "LRREF", "TYPLREF", "PRREF", "TYPPREF", "SENNLV", 
"NMOBSRX", "CADPV", "LRMOB", "TYPLMOB", "NRMOB", "TYPNMOB", "CRREF", 
"ALPHSH", "ALPHSR", "CMOBSRN", "CMOBSRX", "CADSRF", "NMOBSRN", 
"NMOBMX", "NVSMOB", "NRCVR", "XPODF", "ALPHL", "ALPHS", "ALPHR", 
"LIPOPT", "SLOSUM*100", "CARMIN", "CMOBMX", "CADSTF", "CADPR1", 
"PROSRR", "PCHOLFF", "PCHOSTF", "PCHORTF", "PCHOSRF", "LIPTB", 
"PLIGSR", "POASR", "PMINSR", "KCOLD", "PROLFR", "PROSTR", "PRORTR", 
"PMINSD", "PMINNO", "PROSRI", "PROSRG", "PROSRF", "PCARSR", "PLIPSR", 
"POASH", "POASD", "POANO", "PMINLF", "PMINST", "PMINRT", "PMINSH", 
"PLIGRT", "PLIGSH", "PLIGSD", "PLIGNO", "POALF", "POAST", "POART", 
"PLIPLF", "PLIPST", "PLIPRT", "PLIPSH", "PLIPNO", "PLIGLF", "PLIGST", 
"THETA", "PCARLF", "PCARST", "PCARRT", "PCARSH", "PCARSD", "PCARNO", 
"PROSHG", "PROSHF", "SDPROS", "SDPROG", "PRONOD", "PROMIN", "PROMAX", 
"PROSTI", "PROSTG", "PROSTF", "PRORTI", "PRORTG", "PRORTF", "PROSHI", 
"TRSFN(4)", "TRSTYP", "TEMPEFFECT", "PROLFI", "PROLFG", "PROLFF", 
"STRMRC", "SHELMRC", "SDMMRC", "TRSFN(1)", "TRSFN(2)", "TRSFN(3)", 
"PCH2O", "MRSWITCH", "TRSWITCH", "LFMRC", "STMMRC", "RTMRC", 
"RNH4C", "RPRO", "RFIXN", "RCH20", "RLIP", "RLIG", "ROA", "RMIN", 
"CCNEFF", "CMXSF", "CQESF", "PGPATH", "RES30C", "R30C2", "RNO3C", 
"SLWSLO", "NSLOPE", "LNREF", "PGREF", "XPGSLW", "YPGSLW", "CICA", 
"FNPGL", "TYPPGL", "PGEFF", "SCV", "KDIF", "LFANGB", "SLWREF", 
"CCEFF", "FNPGN", "TYPPGN", "FNPGT", "TYPPGT", "XLMAXT", "YLMAXT", 
"CROPSIM"), PNAME = c("PARMAX", "PHTMAX", "KCAN", "CCMP", "CCMAX", 
"CROPSIM", "CROPSIM", "CROPSIM", "CROPSIM", "CROPSIM", "CROPSIM", 
"CROPSIM", "CROPSIM", "CROPSIM", "CROPSIM", "CROPSIM", "CROPSIM", 
"CROPSIM", "CROPSIM", "CROPSIM", "CROPSIM", "CROPSIM", "CROPSIM", 
"CROPSIM", "CROPSIM", "CROPSIM", "CROPSIM", "CROPSIM", "CROPSIM", 
"CROPSIM", "CROPSIM", "CROPSIM", "CROPSIM", "CROPSIM", "CROPSIM", 
"CROPSIM", "CROPSIM", "CROPSIM", "CROPSIM", "CROPSIM", "CROPSIM", 
"CROPSIM", "CROPSIM", "CROPSIM", "CROPSIM", "CROPSIM", "CROPSIM", 
"CROPSIM", "CROPSIM", "CROPSIM", "CROPSIM", "CROPSIM", "CROPSIM", 
"CROPSIM", "CROPSIM", "CROPSIM", "CROPSIM", "CROPSIM", "CROPSIM", 
"FILEX", "FILEX", "FILEX", "FILEX", "SOIL", "SOIL", "CROPSIM", 
"CANEGRO", "CANEGRO", "CANEGRO", "FILEX", "FILEX", "FILEX", "FILEX", 
"CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", 
"CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", 
"CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", 
"CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", 
"CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", 
"CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", 
"CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", 
"CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", 
"CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", 
"CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", 
"CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", 
"CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", 
"CROPGRO", "CROPGRO", "CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"FORAGE", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
fmt = structure(list(MODEL = c("FORAGE", "FORAGE", "FORAGE", "FORAGE", 
    model=toupper(model)
    type=toupper(type)
    prm.name=toupper(prm.name)
get.fmt <- function(prm.name,type,model){

}
    return(spr.fmt)
    spr.fmt[character]=gsub('%','%-',spr.fmt[character])
    spr.fmt=gsub('%1X,',' %',gsub('%2X,','  %',spr.fmt))
    spr.fmt=gsub(',1Xs','s ',gsub(',2Xs','s  ',spr.fmt))
    spr.fmt=gsub(',1Xf','f ',gsub(',2Xf','f  ',spr.fmt))
    spr.fmt=gsub(',1Xe','e ',gsub(',2Xe','e  ',spr.fmt))
    spr.fmt=gsub(',1Xd','d ',gsub(',2Xd','d  ',spr.fmt))
    spr.fmt[character]=paste('%',forfmt[character],'s',sep='')
    spr.fmt[scientific]=paste('%',forfmt[scientific],'e',sep='')
    spr.fmt[integer]=paste('%',forfmt[integer],'d',sep='')
    spr.fmt[real]=paste('%',forfmt[real],'f',sep='')
    forfmt=gsub('A','',forfmt)
    forfmt=gsub('ES','',forfmt)
    forfmt=gsub('I','',forfmt)
    forfmt=gsub('F','',forfmt)
    forfmt=gsub('\\)','',forfmt)
    forfmt=gsub('\\(','',forfmt)
    character=grepl('A',forfmt)
    scientific=grepl('ES',forfmt)
    integer=grepl('I',forfmt)
    real=grepl('F',forfmt)
    spr.fmt=vector(length=length(forfmt),mode='character')
fortran2sprintf<-function(forfmt){

}
    return(invisible(flx.lines))
    }
        write(flx.lines,file=filex.name)
    if(!is.null(filex.name)){
    }
        flx.lines[linenum] = ''
        linenum = linenum + 1
        }
            }
                flx.lines[linenum] = ''
                linenum = linenum + 1
                }
                    linenum = linenum + length(sub.lines)
                                (linenum+length(sub.lines))] = sub.lines
                    flx.lines[(linenum+1):
                    sub.lines = write.subsection(filex[[i]][[j]][[k]])
                for(k in 1:length(filex[[i]][[j]])){
            }else{
                linenum = linenum + length(sub.lines)
                                (linenum+length(sub.lines))] = sub.lines
                flx.lines[(linenum+1):
                sub.lines = write.subsection(filex[[i]][[j]])
            if(!grepl('SIMULATION CONTROLS',names(filex)[i])){
        for(j in 1:length(filex[[i]])){
    for (i in 2:length(filex)){
    linenum = 2
    flx.lines[2] = ''
    flx.lines[1] = filex[[1]]
    flx.lines = vector(length=nlines,mode='character')
    nlines = nlines.filex(filex)
write.filex <- function(filex,filex.name=NULL){

}
    return(filex)
#    lapply(filex,function(x) gsub('^\\*','',gsub('      *.*','',x[[1]])))
                            c('EXP.DETAILS',sectnames)))
                        gsub('      *.*','',
    names(filex) = gsub('^\\*','',
    }
        }
            filex[[i+1]]=get.sim.controls(filex=flx[2:length(flx)])
        }else{
            filex[[i+1]]=get.section(sectnames[i],file=flx[2:length(flx)])
        if(!sectnames[i]=='SIMULATION CONTROLS'){
    for (i in 1:length(sectnames)){
    sectnames = gsub('TILLAGE .*','TILLAGE',sectnames)    
    sectnames = gsub('RESIDUES .*','RESIDUES',sectnames)
    sectnames = gsub('FERTILIZERS .*','FERTILIZERS',sectnames)
    sectnames = gsub('\\)','\\\\)',sectnames)
    sectnames = gsub('\\(','\\\\(',sectnames)
    sectnames = sectnames[2:length(sectnames)]
    filex[[1]] = paste('*',sectnames[1],sep='')
    filex = vector('list',length=length(sectnames))
    sectnames = c(sectnames,'SIMULATION CONTROLS')
    sectnames = sectnames[!grepl('SIMULATION CONTROLS',sectnames)]
    sectnames = gsub('^\\*','',grep('^\\*',flx,value=T))
    flx = gsub('  *$','',flx)
    flx = readLines(filex.name)
read.filex <- function(filex.name){

}
)
"SIMULATION CONTROLS"))
"PLANTING DETAILS", "IRRIGATION AND WATER MANAGEMENT", "FERTILIZERS", 
"GENERAL", "TREATMENTS", "CULTIVARS", "FIELDS", "INITIAL CONDITIONS", 
        -1L), class = "data.frame")))), .Names = c("EXP.DETAILS", 
        "HARVEST", "HFRST", "HLAST", "HPCNP", "HPCNR"), row.names = c(NA, 
            HLAST = 82289L, HPCNP = 100, HPCNR = 0), .Names = c("N", 
        structure(list(N = 1, HARVEST = " HA          ", HFRST = 0L, 
        "RTIME", "RIDEP"), row.names = c(NA, -1L), class = "data.frame"), 
            RIDEP = 20), .Names = c("N", "RESIDUES", "RIPCN", 
            N = 1, RESIDUES = " RE          ", RIPCN = 100, RTIME = 1, 
        ), row.names = c(NA, -1L), class = "data.frame"), structure(list(
        "NITROGEN", "NMDEP", "NMTHR", "NAMNT", "NCODE", "NAOFF"
            NMTHR = 50, NAMNT = 25, NCODE = "FE001 ", NAOFF = "GS000"), .Names = c("N", 
        structure(list(N = 1, NITROGEN = " NI          ", NMDEP = 30, 
        "IREFF"), row.names = c(NA, -1L), class = "data.frame"), 
        "IMDEP", "ITHRL", "ITHRU", "IROFF", "IMETH", "IRAMT", 
            IRAMT = 10, IREFF = 1), .Names = c("N", "IRRIGATION", 
            ITHRL = 50, ITHRU = 100, IROFF = "GS000 ", IMETH = "IR001 ", 
        structure(list(N = 1, IRRIGATION = " IR          ", IMDEP = 30, 
        "PSTMX", "PSTMN"), row.names = c(NA, -1L), class = "data.frame"), 
        "PLANTING", "PFRST", "PLAST", "PH2OL", "PH2OU", "PH2OD", 
            PH2OU = 100, PH2OD = 30, PSTMX = 40, PSTMN = 10), .Names = c("N", 
            PFRST = NA_integer_, PLAST = NA_integer_, PH2OL = 40, 
        "@  AUTOMATIC MANAGEMENT", structure(list(N = 1, PLANTING = " PL          ", 
        "CHOUT", "OPOUT"), row.names = c(NA, -1L), class = "data.frame"), 
        "CAOUT", "WAOUT", "NIOUT", "MIOUT", "DIOUT", "LONG", 
        "OUTPUTS", "FNAME", "OVVEW", "SUMRY", "FROPT", "GROUT", 
            LONG = "    Y ", CHOUT = "    N ", OPOUT = "    N"), .Names = c("N", 
            NIOUT = "    Y ", MIOUT = "    N ", DIOUT = "    N ", 
            FROPT = 1, GROUT = "    Y ", CAOUT = "    N ", WAOUT = "    Y ", 
            FNAME = "    N ", OVVEW = "    Y ", SUMRY = "    Y ", 
        -1L), class = "data.frame"), structure(list(N = 1, OUTPUTS = " OU          ", 
        "IRRIG", "FERTI", "RESID", "HARVS"), row.names = c(NA, 
            HARVS = "    M"), .Names = c("N", "MANAGEMENT", "PLANT", 
            IRRIG = "    R ", FERTI = "    R ", RESID = "    N ", 
        structure(list(N = 1, MANAGEMENT = " MA          ", PLANT = "    R ", 
        "HYDRO", "NSWIT", "MESOM"), row.names = c(NA, -1L), class = "data.frame"), 
        "WTHER", "INCON", "LIGHT", "EVAPO", "INFIL", "PHOTO", 
            NSWIT = 1, MESOM = "    G"), .Names = c("N", "METHODS", 
            INFIL = "    S ", PHOTO = "    R ", HYDRO = "    R ", 
            INCON = "    M ", LIGHT = "    E ", EVAPO = "    R ", 
        structure(list(N = 1, METHODS = " ME          ", WTHER = "    M ", 
        "DISES", "CHEM", "TILL"), row.names = c(NA, -1L), class = "data.frame"), 
        "OPTIONS", "WATER", "NITRO", "SYMBI", "PHOSP", "POTAS", 
            CHEM = "    N ", TILL = "    N"), .Names = c("N", 
            PHOSP = "    N ", POTAS = "    N ", DISES = "    N ", 
            WATER = "    N ", NITRO = "    N ", SYMBI = "    N ", 
        -1L), class = "data.frame"), structure(list(N = 1, OPTIONS = " OP          ", 
        "NREPS", "START", "SDATE", "RSEED", "SNAME"), row.names = c(NA, 
            SNAME = NA_real_), .Names = c("N", "GENERAL", "NYERS", 
            NREPS = 1, START = "     S", SDATE = NA, RSEED = 2150, 
        structure(list(N = 1, GENERAL = " GE         ", NYERS = 1, 
    `SIMULATION CONTROLS` = list(list("*SIMULATION CONTROLS", 
    "FAMC", "FAMO", "FOCD", "FERNAME"), row.names = 1L, class = "data.frame")), 
    "FDATE", "FMCD", "FACD", "FDEP", "FAMN", "FAMP", "FAMK", 
        FAMC = 0, FAMO = 0, FOCD = NA_real_, FERNAME = NA_real_), .Names = c("F", 
        FDEP = NA_real_, FAMN = NA_real_, FAMP = 0, FAMK = 0, 
        F = 1, FDATE = NA_integer_, FMCD = " FE004", FACD = " AP002", 
    FERTILIZERS = list("*FERTILIZERS (INORGANIC)", structure(list(
        "IDATE", "IROP", "IRVAL"), row.names = c(NA, -6L), class = "data.frame")), 
        " IR001"), IRVAL = c(NA, 120, 120, 120, 120, 120)), .Names = c("I", 
        12093), IROP = c(NA, " IR001", " IR001", " IR001", " IR001", 
        1, 1, 1, 1, 1), IDATE = c(NA, 12011, 12032, 12056, 12074, 
        -1L), class = "data.frame"), structure(list(I = c(1, 
        "ITHR", "IEPT", "IOFF", "IAME", "IAMT", "IRNAME"), row.names = c(NA, 
            IRNAME = NA_real_), .Names = c("I", "EFIR", "IDEP", 
            IOFF = NA_real_, IAME = NA_real_, IAMT = NA_real_, 
            EFIR = 1, IDEP = NA_real_, ITHR = NA_real_, IEPT = NA_real_, 
        "*IRRIGATION AND WATER MANAGEMENT", structure(list(I = 1, 
    ), row.names = c(NA, -1L), class = "data.frame")), `IRRIGATION AND WATER MANAGEMENT` = list(
    "PLRD", "PLDP", "PLWT", "PAGE", "PENV", "PLPH", "SPRL", "PLNAME"
    "PDATE", "EDATE", "PPOP", "PPOE", "PLME", "PLDS", "PLRS", 
        PLPH = NA_real_, SPRL = 0, PLNAME = NA_real_), .Names = c("P", 
        PLDP = 5.5, PLWT = NA_real_, PAGE = NA_real_, PENV = NA_real_, 
        PLME = "     S", PLDS = "     R", PLRS = 40, PLRD = 0, 
        P = 1, PDATE = NA, EDATE = NA, PPOP = NA, PPOE = NA, 
    `PLANTING DETAILS` = list("*PLANTING DETAILS", structure(list(
        "SNO3"), row.names = c(NA, -7L), class = "data.frame")), 
            NA_real_)), .Names = c("C", "ICBL", "SH2O", "SNH4", 
            NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, 
            NA_real_, NA_real_, NA_real_), SNO3 = c(NA_real_, 
            SNH4 = c(NA_real_, NA_real_, NA_real_, NA_real_, 
            NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), 
            90, 120, 150, 180), SH2O = c(NA_real_, NA_real_, 
            C = c(1, 1, 1, 1, 1, 1, 1), ICBL = c(15, 30, 60, 
        ), row.names = c(NA, -1L), class = "data.frame"), structure(list(
        "ICRES", "ICREN", "ICREP", "ICRIP", "ICRID", "ICNAME"
        "PCR", "ICDAT", "ICRT", "ICND", "ICRN", "ICRE", "ICWD", 
            ICRIP = NA_real_, ICRID = NA_real_, ICNAME = NA_real_), .Names = c("C", 
            ICRES = NA_real_, ICREN = NA_real_, ICREP = NA_real_, 
            ICRN = NA_real_, ICRE = NA_real_, ICWD = NA_real_, 
            ICDAT = NA_integer_, ICRT = NA_real_, ICND = NA_real_, 
        "*INITIAL CONDITIONS", structure(list(C = 1, PCR = "    WH", 
        ), row.names = c(NA, -1L), class = "data.frame")), `INITIAL CONDITIONS` = list(
        "XCRD", "YCRD", "ELEV", "AREA", "SLEN", "FLWR", "SLAS"
            AREA = 0, SLEN = 0, FLWR = 0, SLAS = 0), .Names = c("L", 
        structure(list(L = 1, XCRD = 0, YCRD = 0, ELEV = 38, 
    "SLDP", "ID_SOIL", "FLNAME"), row.names = c(NA, -1L), class = "data.frame"), 
    "WSTA", "FLSA", "FLOB", "FLDT", "FLDD", "FLDS", "FLST", "SLTX", 
        ID_SOIL = NA, FLNAME = NA_real_), .Names = c("L", "ID_FIELD", 
        FLDD = 0, FLDS = 0, FLST = 0, SLTX = NA_real_, SLDP = NA, 
        WSTA = NA, FLSA = NA_real_, FLOB = 0, FLDT = " DR000", 
    FIELDS = list("*FIELDS", structure(list(L = 1, ID_FIELD = NA, 
    "CR", "INGENO", "CNAME"), row.names = 1L, class = "data.frame")), 
        INGENO = NA_character_, CNAME = NA_character_), .Names = c("C", 
    CULTIVARS = list("*CULTIVARS", structure(list(C = 1, CR = NA_character_, 
    "MF", "MR", "MC", "MT", "ME", "MH", "SM"), row.names = 1L, class = "data.frame")), 
    "R", "O", "C", "TNAME", "CU", "FL", "SA", "IC", "MP", "MI", 
        MC = 0, MT = 0, ME = 0, MH = 0, SM = 1), .Names = c("N", 
        FL = 1, SA = 0, IC = 0, MP = 1, MI = 0, MF = 0, MR = 0, 
    structure(list(N = 1, R = 1, O = 0, C = 0, TNAME = NA, CU = 1, 
    -1L), class = "data.frame")), TREATMENTS = list("*TREATMENTS                        -------------FACTOR LEVELS------------", 
    -1L), class = "data.frame"), structure(list(NOTES = NA_real_), .Names = "NOTES", row.names = c(NA, 
    -1L), class = "data.frame"), structure(list(SITE = NA), .Names = "SITE", row.names = c(NA, 
    "*GENERAL", structure(list(ADDRESS = NA), .Names = "ADDRESS", row.names = c(NA, 
    structure(list(EXP.DETAILS = "*EXP.DETAILS:", GENERAL = list(
return(
new.filex <- function(){

}
    return(ljust)
    ljust = grep('FNAME',ljust,value=T,invert=T)
    ljust = c(ljust,grep('NAME',x,value=T))
            'SMODEL','PEOPLE','ADDRESS','SITE')
            'IRRIGATION','NITROGEN','RESIDUES','HARVEST','ID_FIELD',
            'METHODS','MANAGEMENT','OUTPUTS','PLANTING',
    ljust = c('HARM','ID_SOIL','WSTA','SLTX','GENERAL','OPTIONS',
list.ljust <- function(x){

}
    return(list(pos1,pos2,svnames))
    svnames = gsub('\\.','',svnames)
    }
        pos2 = pos1 + widths - 1
        pos1[1] = 1
        pos1[2:length(widths)] = cumsum(widths[1:(length(widths)-1)])+1
        pos1 = widths
    }else{
        pos2 = pos1 + widths - 1
        }
            pos1[j,1] = 1
            }
                pos1[j,2:ncol(pos1)] = cumsum(widths[j,1:(ncol(widths)-1)])+1
            if(ncol(pos1)>1){
            }
                }
                    }
                        break
                        widths[j,k] = widths[j,k] - diffs[j]
                    }else{
                        widths[j,k] = 0
                        diffs[j] = diffs[j] - widths[j,k]
                    if(diffs[j]>=widths[j,k]){
                for(k in seq(ncol(widths),1)){
            }else{
                widths[j,ncol(widths)] = widths[j,ncol(widths)] + diffs[j]
            }else if(diffs[j]<widths[j,ncol(widths)]){
                widths[j,ncol(widths)] = widths[j,ncol(widths)] + diffs[j]
            if(diffs[j]<0){
        for (j in 1:nlines){
        pos1 = widths
        widths = matrix(nrow=nlines,rep(widths,nlines),byrow=T)
        diffs = nchar(filex[(start+1):end])-sum(widths)
        nlines = end-start
    if(any(sum(widths)!=nchar(filex[(start+1):end]))){
                        'HARVEST','ID_SOIL','SANAME')]+1
                        'PLANTING','IRRIGATION','NITROGEN','RESIDUES',
            widths[svnames%in%c('OPTIONS','METHODS','MANAGEMENT','OUTPUTS',
                        'HARVEST','ID_SOIL','SANAME')] = 
                        'PLANTING','IRRIGATION','NITROGEN','RESIDUES',
    widths[svnames%in%c('OPTIONS','METHODS','MANAGEMENT','OUTPUTS',
    widths[nchar(svnames)>1] = widths[nchar(svnames)>1] + 1
    }
        widths[svnames=='TNAME'] = 25
    if('TNAME'%in%svnames&'CU'%in%svnames){
    widths = get.widths(svnames)
    svnames = svnames[!nchar(svnames)==0]
    svnames = gsub('\\.','',unlist(strsplit(gsub('@',' ',filex[start]),'  *')))
get.pos.names <- function(filex,start,end){

}
    return(is.num)
    is.num = !any(is.na(as.numeric(vect[!vect%in%c('')])))
    vect = gsub('^ *','',gsub(' *$','',vect))
    on.exit(options(old))
    old = options(warn = -1)
# code based on all.is.numeric() from package Hmisc version 3.9-0
numeric.all <- function(vect){

}
    return(section)
    }
        }
            section[[i]]=file[begin]
        }else{
            }
                section[[i]][section[[i]]==''] = NA
                section[[i]][section[[i]]=='-99'] = NA
                section[[i]][section[[i]]==-99] = NA
                }
                    section[[i]][,j] = as.integer(section[[i]][,j])
                        'SDATE','PFRST','PLAST','HFRST','HLAST')){
                    c('FDATE','SADAT','ICDAT','PDATE','EDATE','HDATE',
                if(colnames(section[[i]])[j]%in%
                }
                    section[[i]][,j] = as.numeric(section[[i]][,j])
                if(numeric.all(section[[i]][,j])){
            for(j in 1:ncol(section[[i]])){
            }
                colnames(section[[i]])=pos.names[[3]]
                section[[i]] = as.data.frame(section[[i]],stringsAsFactors=F)
                }
                    section[[i]][,j] = substring(file[(begin+1):end],pos.names[[1]][j],pos.names[[2]][j])
                for(j in 1:length(pos.names[[1]])){
                section[[i]] = matrix(nrow=(end-begin),ncol=length(pos.names[[1]]))
            }else{
                colnames(section[[i]])=pos.names[[3]]
                section[[i]] = as.data.frame(section[[i]],stringsAsFactors=F)
                }
                    section[[i]][,j] = substring(file[(begin+1):end],pos.names[[1]][,j],pos.names[[2]][,j])
                for(j in 1:ncol(pos.names[[1]])){
                section[[i]] = matrix(nrow=(end-begin),ncol=ncol(pos.names[[1]]))
            if(!is.vector(pos.names[[1]])){
            pos.names = get.pos.names(file,begin,end)
        if(begin<end){
        }
            end = length(file)
            begin = headers[i-1]
        }else{
            end = (headers[i]-1)
            begin = headers[i-1]
        if(i<length(section)){
    for(i in 2:length(section)){
    section[[1]] = file[1]
    section = vector('list',length(headers)+1)
    headers = grep('@',file)
    file = file[!file==''&!grepl('^[[:space:]]$',file)]
read.section <- function(file){

}
    return(section)
    }
        section[[i]] = read.section(filex[sect.heading[i]:next.heading])
        }
            next.heading = length(filex)
        }else{
            next.heading = sect.heading[i] + next.heading[1]
        if(length(next.heading)>0){
        next.heading = grep('^\\*',filex[(sect.heading[i]+1):length(filex)])-1
    for(i in 1:length(sect.heading)){
    section = vector('list',length=length(sect.heading))
    sect.heading = grep(paste('\\*',sect.name,sep=''),filex)
    filex = filex[!substr(filex,1,1)=='!']
    if(is.null(filex)) filex = readLines(filex.name)
    sect.name = 'SIMULATION CONTROLS'
                                stop('No file name or filex supplied')}
    if(is.null(filex.name)&is.null(filex)){
get.sim.controls <- function(filex.name=NULL,level=NULL,filex=NULL){

}
    return(section)
    section = read.section(file)
    file = file[sect.heading:next.heading[1]]
    }
        next.heading = length(file)
    }else{
        next.heading = sect.heading + next.heading[1]
    if(length(next.heading)>0){
    }
        
    }else{
                grep('^\\*',file[(sect.heading[1]+1):length(file)])-1
        next.heading = 
        sect.heading = grep(paste('\\*',sect.name,sep=''),file)
    if(file.type=='filex'){
    file = file[!substr(file,1,1)=='!']
    if(is.null(file)) file = readLines(file.name)
                )
                    sect.name
                    sa='SOIL ANALYSIS',
                    ic='INITIAL CONDITIONS',
    sect.name = switch(sect.name,
    if(is.null(file.name)&is.null(file)) stop('No file name or file supplied')
get.section <- function(sect.name,file.type='filex',file.name=NULL,level=NULL,file=NULL){

}
    return(string)
    string = gsub('(^ +)|( +$)','',string)
trim <- function(string){

}
        return(list(x=lgdx,y=lgdy,xjust=lgdxjust,yjust=lgdyjust))
        )
                bottomright={lgdx=xlim[2];lgdy=ylim[1];lgdxjust=1;lgdyjust=0},
                        lgdy=ylim[1];lgdxjust=0.5;lgdyjust=0},
                bottom={lgdx=(xlim[2]-xlim[1])*0.5+xlim[1]
                bottomleft={lgdx=xlim[1];lgdy=ylim[1];lgdxjust=0;lgdyjust=0},
                            lgdxjust=1;lgdyjust=0.5},
                            lgdy=(ylim[2]-ylim[1])*.5+ylim[1]
                middleright={lgdx=xlim[2]
                            lgdxjust=0;lgdyjust=0.5},
                            lgdy=(ylim[2]-ylim[1])*.5+ylim[1]
                middleleft={lgdx=xlim[1]
                topright={lgdx=xlim[2];lgdy=ylim[2];lgdxjust=1;lgdyjust=1},
                        lgdy=ylim[2];lgdxjust=0.5;lgdyjust=1},
                top={lgdx=(xlim[2]-xlim[1])*0.5+xlim[1]
                topleft={lgdx=xlim[1];lgdy=ylim[2];lgdxjust=0;lgdyjust=1},
        switch(lgdpos,
convert.pos <- function(lgdpos,xlim,ylim){

}
    if(!is.null(file)) dev.off()
    }
            cex=1.5*dev.scale)
            dstat=dstat,ceff=ceff,rmse=rmse,rrmse=rrmse,
        add.stats(statpos,data,
        if(is.null(statpos$y)) statpos$y = ylim[1]
        if(is.null(statpos$x)) statpos$x = xlim[2]
        statpos = convert.pos(statpos,xlim,ylim)
        }
            data = get.obs.pred(variable=variable)
        }else{
            }
                data = rbind(data,sbdata)
                sbdata = get.obs.pred(variable=variable,trno=trno[i],add=list(pttyp=pttyp[i],ptbg=ptbg[i],ptcol=ptcol[i]))
            for (i in 1:length(trno)){
            data = NULL
            ptcol = get.col(length(trno),bw=T)
            ptbg = get.ptbg(length(trno))
            pttyp = get.pch(length(trno))
            }
                trno = get.trno(run)
            if(is.null(trno)){
        if(!is.null(run)|!is.null(trno)){
    if(any(c(dstat,ceff,rmse,rrmse))){
    }
        }
            mtext(ylab[2],2,2*dev.scale,cex=1.5*dev.scale)
            mtext(ylab[1],2,4*dev.scale,cex=1.5*dev.scale)
        }else{
            mtext(ylab,2,2*dev.scale,cex=1.5*dev.scale)
        if(variable!='PHAN'){
        if(any(!grepl('%',ylab))) ylab=parse(text=ylab)
    if(nchar(ylab)>0){
    }
        mtext(xlab,1,2*dev.scale,cex=1.5*dev.scale)
        if(!grepl('%',xlab)) xlab=parse(text=xlab)
    if(nchar(xlab)>0){
    }
#        xincrement = (xlim[2]-xlim[1])/(length(xaxis.lab)-1)
        axis.POSIXct(1,x=xax.lim,pos=ylim[1],cex.axis=1.5*dev.scale,las=las)
#        axis(1,at=seq(xlim[1],xlim[2],by=ifelse(xincrement<300,'month','year')),pos=ylim[1],cex.axis=1.5*dev.scale)
    }else{
                pos=ylim[1],cex.axis=1.5*dev.scale,las=las)
                labels=seq(daplim[1],daplim[2],length.out=dap.len),
        axis.POSIXct(1,at=seq(xax.lim[1],xax.lim[2],length.out=dap.len),
        dap.len = (daplim[2]-daplim[1])/xincrement + 1
        if(is.null(xincrement)) xincrement = get.increment(daplim)
        daplim=as.integer(difftime(xax.lim,pdate,units='days'))
    if(use.dap){
    }
        axis(2,at=seq(ylim[1],ylim[2],yincrement),labels=yaxis.lab,pos=xlim[1],cex.axis=1.5*dev.scale,las=las)
    }else{
        axis(2,at=seq(ylim[1],ylim[2],yincrement),pos=xlim[1],cex.axis=1.5*dev.scale,las=las)
    if(is.null(yaxis.lab)) {
    if(is.null(yincrement)) yincrement = get.increment(ylim)
    if(!is.null(subplot)) mtext(subplot,side=3,at=xlim[2],adj=1,padj=0,line=-.5,cex=3*dev.scale)
    }
                        lwd=2*dev.scale)
                        legend = leglab,cex=1.3*dev.scale,
                        pch=ptype,lty=ltype,pt.bg=pbg,col=plcol,
                        xjust=lgdpos$xjust,yjust=lgdpos$yjust,
        legend(x=lgdpos$x,y=lgdpos$y,
        if(is.null(lgdpos$y)) lgdpos$y = ylim[2]
        if(is.null(lgdpos$x)) lgdpos$x = xlim[1]
        lgdpos = convert.pos(lgdpos,xlim,ylim)
    if(include.legend){
    if(!is.null(trt.lab))    leglab = trt.lab
    }
        }    
            colnum = colnum + 1
            }
                }
                    leglab=paste(variable[j],'- ',TRSQ,as.character(trsq[i]))
                }else{
                    leglab=c(leglab,paste(variable[j],'- ',TRSQ,as.character(trsq[i])))
                if(exists('leglab')){
            if(is.null(trt.lab)){
            lines(pdatasub[,'DATE'],pdatasub[,variable[j]],lty=ltype[colnum],col=plcol[colnum],lwd=2*dev.scale)
            }
                points(odatasub[,'DATE'],odatasub[,variable[j]],col=plcol[colnum],pch=ptype[colnum],bg=pbg[colnum],cex=1.5*dev.scale)
                                                            colnames(odata)%in%c('DATE',variable[j])])
                odatasub = na.omit(odata[odata[,TRSQ]==trsq[i]&odata$DATE>=min(xlim)&odata$DATE<=max(xlim),
            if(plotfilet[i,j]){
                                                            colnames(pdata)%in%c('DATE',variable[j])])
            pdatasub = na.omit(pdata[pdata[,TRSQ]==trsq[i]&pdata$DATE>=min(xlim)&pdata$DATE<=max(xlim),
        for (j in 1:length(variable)){
        }
            colnum = 1
            plot(1,1,xlim=xlim,ylim=ylim,xlab='',ylab='',type='n',axes=F)
            }
                pbg = plcol
                ltype = rep(1,length(variable)*length(trsq))
            }else{
                pbg = get.ptbg(length(variable)*length(trsq))
                ltype = get.ltype(length(variable)*length(trsq))
            if(bw){
            plcol = get.col(length(variable)*length(trsq),bw=bw)
            }
                ptype = NULL
            }else{
                ptype = get.pch(length(variable)*length(trsq))
            if(!is.null(filet)){
            }
                ylab = gsub(' ','~',ylab)
            }else if(!grepl('%',ylab)){
                ylab = ''
            if(is.null(ylab)){
            }
                xlab = gsub(' ','~',xlab)
            }else if(!grepl('%',xlab)){
                }
                    xlab = 'Date'
                }else{
                    xlab = 'DAP'
                if(use.dap){
            if(is.null(xlab)){
            if(variable=='PHAN') par(mar=par('mar')*c(1,2,1,1))
            if(cimmyt) par(mar=c(4,5,2,1))
            par(mar=c(2,2,2,.5))
            par(mgp=c(3,1,0)*dev.scale)
            dev.scale = dev.size()[2]/6.99311
            }
                if (grepl('.eps',file)) postscript(filename=file)
                if (grepl('.tif',file)) tiff(filename=file)
                if (grepl('.jpg',file)) jpeg(filename=file)
            }else{
                if (new|!any(charmatch(c('x11','X11'),names(dev.cur()),nomatch=-1))) X11()
            if (is.null(file)){
            }
                xax.lim = c(xlim[1],xax.lim)
                }
                    xax.lim = as.POSIXct(xax.lim,format='%Y%j')
                if(is.character(xax.lim)){
            }else if(length(xax.lim)<2){
                xax.lim = xlim
            if(is.null(xax.lim)){
            }
                }
                    xlim = c(min(unlist(pdata[,'DATE']),na.rm=T),max(unlist(pdata[,'DATE']),na.rm=T))
                if(is.null(xlim)){
                ylim = set.xylim(ylim,na.omit(unlist(pdata[,colnames(pdata)%in%variable])))
            }else{
                }
                    }
                        xlim[1] = pdate
                    if(use.dap){
                    xlim = c(min(c(unlist(pdata[,'DATE']),unlist(odata[,'DATE'])),na.rm=T),max(c(unlist(pdata[,'DATE']),unlist(odata[,'DATE'])),na.rm=T))
                if(is.null(xlim)){
                                                unlist(odata[,colnames(odata)%in%variable]))))
                ylim = set.xylim(ylim,na.omit(c(unlist(pdata[,colnames(pdata)%in%variable]),
                odata = filet[filet[,TRSQ]%in%trsq,colnames(filet)%in%c(TRSQ,'DATE',variable)]
            if(any(plotfilet)){
            pdata = output[output[,TRSQ]%in%trsq,colnames(output)%in%c(TRSQ,'DATE',variable)]
            }
                pdate = getpltdate(filexname,sqno=trsq[1])
            }else if(use.dap){
                pdate = getpltdate(filexname,trno=trsq[1])
            if(use.dap&&TRSQ=='TRNO'){

        if(i==1){
    for(i in 1:length(trsq)){
    }
        plotfilet = trsq%in%filet[,TRSQ]%*%t(variable%in%colnames(filet))==1
    if(!exists('plotfilet')){
    if(!is.null(xlim)&is.character(xlim)) xlim = as.POSIXct(xlim,format='%Y%j')
    if(is.null(trsq)) trsq = as.integer(as.character(levels(as.factor(output[,TRSQ]))))
    }
        trsq = trno
        TRSQ = 'TRNO'
    }else{
        trsq = sqno
        TRSQ = 'SQNO'
    if ('SQNO'%in%colnames(output)){
    }
        warning('File T has not been loaded.')
        plotfilet=rep(0,length(trno))%*%t(rep(0,length(variable)))
    }else{
        filet=get('filet')
    if (exists('filet')){
    }
        if (variable=='alldm') variable = c('CWAD','LWAD','SWAD','PWAD','GWAD')
    if(length(variable)==1){
    output=get('output')
                    subplot=NULL,cimmyt=F){
                    lgdpos='topleft',statpos='bottomright',dstat=FALSE,ceff=FALSE,rmse=FALSE,rrmse=FALSE,
                    xlab=NULL,ylab=NULL,trt.lab=NULL,include.legend=T,las=0,
                    xaxis.lab=NULL,yaxis.lab=NULL,use.dap=F,filexname=NULL,
                    xax.lim=NULL,yax.lim=NULL,
                    xlim=NULL,ylim=NULL,xincrement=NULL,yincrement=NULL,
dssat.plot <- function(variable,trno=NULL,sqno=NULL,run=NULL,new=FALSE,file=NULL,bw=FALSE,

}
    return(ylab)
    )
            LnumSD={ylab = 'Main Stem Node Number (nodes plant^-1)'}
            LAID={ylab = 'Leaf Area Index'},
            SLAD={ylab = 'Specific Leaf Area (cm^2 g^-1)'},
            HIAD={ylab = 'Grain Harvest Index'},
            HIPD={ylab = 'Pod Harvest Index'},
            SHpctD={ylab = 'Shelling Percentage (%)'},
            PHAN={ylab = c('Canopy Photosynthesis','(mg CO[2] m^-2 s^-1)')},
            QCpctD={ylab = 'Rhizome TNC (%)'},
            QNpctD={ylab = 'Rhizome N (%)'},
            GNpctD={ylab = 'Grain N Concentration (%)'},
            SHND={ylab = 'Shell N Concentration (%)'},
            SNpctD={ylab = 'Stem N Concentration (%)'},
            LNpctD={ylab = 'Leaf N Concentration (%)'},
            QWAD={ylab = 'Rhizome Mass (kg ha^-1)'},
            GWAD={ylab = 'Grain Mass (kg ha^-1)'},
            PWAD={ylab = 'Pod Mass (kg ha^-1)'},
            RWAD={ylab = 'Root Mass (kg ha^-1)'},
            CWAD={ylab = 'Shoot Mass (kg ha^-1)'},
            SWAD={ylab = 'Stem Mass (kg ha^-1)'},
            LWAD={ylab = 'Leaf Mass (kg ha^-1)'},
    switch(sub('#','num',sub('%','pct',variable)),
get.ylabel <- function(variable){

}
    return(units)
    )
            LnumSD={units = 'nodes plant^-1'}
            LAID={units = ''},#'m^2 m^-2'},
            SLAD={units = 'cm^2 g^-1'},
            HIAD={units = ''},
            HIPD={units = ''},
            SHpctD={units = '%'},
            GNpctD={units = '%'},
            SHND={units = '%'},
            SNpctD={units = '%'},
            LNpctD={units = '%'},
            GWAD={units = 'kg ha^-1'},
            PWAD={units = 'kg ha^-1'},
            RWAD={units = 'kg ha^-1'},
            CWAD={units = 'kg ha^-1'},
            SWAD={units = 'kg ha^-1'},
            LWAD={units = 'kg ha^-1'},
    switch(sub('#','num',sub('%','pct',variable)),
get.units <- function(variable){

}
            legend=lgd,cex=cex,bty='n')
    legend(x=xypos$x,y=xypos$y,xjust=xypos$xjust,yjust=xypos$yjust,
    }
        }
            lgd = paste('RRMSE: ',signif(rrmse(data),digits=2),sep='')
        }else{
            lgd = c(lgd,paste('RRMSE: ',signif(rrmse(data),digits=2),sep=''))
        if(exists('lgd')){
    if(rrmse){
    }
        }
            lgd = paste('RMSE: ',signif(rmse(data),digits=2),sep='')
        }else{
            lgd = c(lgd,paste('RMSE: ',signif(rmse(data),digits=2),sep=''))
        if(exists('lgd')){
    if(rmse){
    }
        }
            lgd = parse(text=paste('C[eff]: ',ceff(data),sep=''))
        }else{
            lgd = c(lgd,parse(text=paste('C[eff]: ',ceff(data),sep='')))
        if(exists('lgd')){
    if(ceff){
    }
        }
            lgd = paste('D: ',dstat(data),sep='')
        }else{
            lgd = c(lgd,paste('D: ',dstat(data),sep=''))
        if(exists('lgd')){
    if(dstat){
add.stats <- function(xypos,data,dstat=T,ceff=F,rmse=T,rrmse=F,cex=1){

}
    }
        add.stats(statpos,data,dstat=dstat,ceff=ceff,rmse=rmse,rrmse=rrmse,cex=1.5*dev.scale)
        statpos = convert.pos('bottomright',xylim,xylim)
    if(any(c(dstat,ceff,rmse,rrmse))) {
    if(!is.null(subplot)) mtext(subplot,side=3,at=xylim[2]*.95,line=-.5,cex=3*dev.scale)
#    text(x=rep(xylim[2]*.7,length(lgd))*dev.scale,y=seq(1,length(lgd))*xylim[2]/15/dev.scale,label=lgd,pos=4,cex=1.5*dev.scale)
    lines(c(xylim[1],signif(xylim[2],digits=2)),c(xylim[1],signif(xylim[2],digits=2)),lwd=dev.scale)
    mtext(ylabel,2,2*dev.scale,cex=1.5*dev.scale)
    mtext(xlabel,1,2*dev.scale,cex=1.5*dev.scale)
#    axis(4,at=seq(0,xylim[2],xylim[2]/4),label=rep('',5),pos=c(0,xylim[2]),lwd.ticks=0)
#    axis(3,at=seq(0,xylim[2],xylim[2]/4),label=rep('',5),pos=c(0,xylim[2]),lwd.ticks=0)
    axis(2,at=seq(xylim[1],xylim[2],increment),pos=xylim[1],cex.axis=1.5*dev.scale)
    axis(1,at=seq(xylim[1],xylim[2],increment),pos=xylim[1],cex.axis=1.5*dev.scale)
    if(is.null(increment)) increment = get.increment(xylim)
    mtext(main,3,2*dev.scale,cex=1.5*dev.scale)
                                    pt.bg=get.ptbg(length(trt.labels)))
                                    col=get.col(length(trt.labels),T),
                                    pch=get.pch(length(trt.labels)),
                                    cex=1.5*dev.scale,
                                    legend=trt.labels,
    if(!is.null(trt.labels)) legend(xylim[1],xylim[2],
    plot(obs,pred,typ='p',pch=pch,col=pcol,bg=ptbg,xlim=xylim,ylim=xylim,xlab='',ylab='',cex=1.5*dev.scale,axes=F)
    if('ptbg'%in%colnames(data)) ptbg = data$ptbg else ptbg = 'black'
    if('ptcol'%in%colnames(data)) pcol = data$ptcol else pcol = 'black'
    if('pttyp'%in%colnames(data)) pch = data$pttyp else pch = 19
    if(cimmyt) par(mar=c(4,5,2,1))
    par(mar=c(2,2,2,.5))
    par(mgp=c(3,1,0)*dev.scale)
    dev.scale = dev.size()[2]/6.99311
    if (new|!any(charmatch(c('x11','X11'),names(dev.cur()),nomatch=-1))) X11()
    }
        ylabel = paste('Predicted ',axis.label,sep='')
    }else{
        ylabel = parse(text=paste('Predicted',axis.label,sep=''))        
    if(!grepl('%',axis.label)){
    }
        xlabel = paste('Observed ',axis.label,sep='')
    }else{
            xlabel = parse(text=paste('Observed',axis.label,sep=''))
    if(!grepl('%',axis.label)){
    }
        axis.label=paste('~',gsub(' ','~',axis.label))
    }else if(axis.label!=''&!grepl('%',axis.label)){
        axis.label=paste('~',gsub(' ','~',axis.label),units,sep='')
        units = paste('~(',gsub(' ','~',units),')',collapse='')
    if (units!=''){
    xylim = set.xylim(xylim,unlist(data[,1:2]))
    pred = data[,2]
    obs = data[,1]
                    trt.labels=NULL,bw=F,subplot=NULL,cimmyt=F){
                    increment=NULL,dstat=T,rmse=T,rrmse=F,ceff=F,new=F,
one21 <- function(data,units='',main='',axis.label='',xylim=NULL,

}
    return(xylim)
    }
        xylim[1]= min(data)%/%ord.mag*ord.mag
        ord.mag = 10^floor(log10(xylim[2]))
    if(is.na(xylim[1])) {
    }
        if(xylim[2]<max(data)) xylim[2] = xylim[2] + 10^(log10(signif(max(data),digits=1))-1)
        if(xylim[2]<max(data)) xylim[2]=signif(max(data),digits=2)
        xylim[2]=signif(max(data),digits=1)
    if(is.na(xylim[2])){
    }
        xylim = c(NA,xylim)
    }else if(length(xylim)<2){
        xylim = c(NA,NA)
    if(is.null(xylim)){
set.xylim <- function(xylim,data){

}
    return(increment)
    if(is.null(increment)) increment = (xylim[2]-xylim[1])/4
    }
        }
            break
            increment = (xylim[2]-xylim[1])/i
        if((xylim[2]-xylim[1])%/%i==(xylim[2]-xylim[1])/i){
    for(i in 4:2){
    increment = NULL
get.increment <- function(xylim){

}
            new=new,trt.labels=trt.labels,bw=bw,subplot=subplot,cimmyt=cimmyt)
            dstat=dstat,rmse=rmse,rrmse=rrmse,ceff=ceff,
            xylim=xylim,increment=increment,
    one21(data=data,units=units,main=main,axis.label=axis.label,
    }
        data = get.obs.pred(variable=variable,sqno=sqno)
    }else{
        }
            data = rbind(data,sbdata)
            sbdata = get.obs.pred(variable=variable,trno=trno[i],add=list(pttyp=pttyp[i],ptbg=ptbg[i],ptcol=ptcol[i]))
        for (i in 1:length(trno)){
        data = NULL
        ptcol = get.col(length(trno),bw=T)
        ptbg = get.ptbg(length(trno))
        pttyp = get.pch(length(trno))
        }
            trno = get.trno(run)
        if(is.null(trno)){
    if(!is.null(run)|!is.null(trno)){
    }
        main = variable
    if(is.null(main)){
                        dstat=T,rmse=T,rrmse=F,ceff=F,trt.labels=NULL,bw=F,subplot=NULL,cimmyt=F){
                        main=NULL,xylim=NULL,increment=NULL,axis.label='',
one21.dssat <- function(variable,trno=NULL,sqno=NULL,run=NULL,dap=NULL,new=FALSE,units='',

}
    return(trno)
    trno = as.integer(as.character(levels(as.factor(output[output$RUN%in%run,]$TRNO))))
    }
        stop('Model outputs have not been loaded.')
    }else{
        output=get('output')
    if (exists('output')){
get.trno <- function(run){

}
    return(ltype)
    ltype = ltype[1:nlevel]
    ltype = c(1:5,1:5)#c(rep(1,2),rep(2,2),rep(3,2),rep(4,2),rep(5,2))
get.ltype <- function(nlevel){

}
    return(plcol)
    plcol = plcol[1:nlevel]
    }
        plcol = 1:nlevel
    }else{
        plcol = rep('black',nlevel)#rep('black',ceiling(nlevel/4)),rep('gray60',ceiling(nlevel/4))
    if(bw){
get.col <- function(nlevel,bw){

}
    return(pbg)
    pbg = pbg[1:nlevel]
    pbg = rep(c('black','white'),12)#c(rep('black',3),rep('white',3),rep('grey',3),rep('white',3))
get.ptbg <- function(nlevel){

}
    return(pch)
    pch = pch[1:nlevel]
    pch = c(21,21,22,22,23,23)#rep(c(21,22,24),2)
get.pch <- function(nlevel){

}
    return(rrmse(data))
    data = get.obs.pred(variable=variable,trno=trno,sqno=sqno,run=run)
rrmse.out <- function(variable,trno=NULL,sqno=NULL,run=NULL){

}
    return(rmse(data))
    data = get.obs.pred(variable=variable,trno=trno,sqno=sqno,run=run)
rmse.out <- function(variable,trno=NULL,sqno=NULL,run=NULL){

}
    return(ceff(data))
    data = get.obs.pred(variable=variable,trno=trno,sqno=sqno,run=run)
ceff.out <- function(variable,trno=NULL,sqno=NULL,run=NULL){

}
    return(dstat(data))
    data = get.obs.pred(variable=variable,trno=trno,sqno=sqno,run=run)
dstat.out <- function(variable,trno=NULL,sqno=NULL,run=NULL){

}
    return(opdata)
    opdata = opdata[,!colnames(opdata)%in%c('SQNO','TRNO','DATE')]
      }
        opdata = do.call('data.frame',lapply(c('opdata',names(add)),as.name))
    if(length(add)>0){
    opdata=cbind(obs,pred)
#    opdata = merge(obs,pred,all=T)
    colnames(pred)[ncol(pred)] = 'pred'
    colnames(obs)[ncol(obs)] = 'obs'
    }
        assign(i,rep(add[[i]],nrow(obs)))
    for (i in names(add)){
#    pred = pred[,3]
#    obs = obs[,3]
    }
        }
            }
                          c('SQNO',variable)])
                pred[i,] = na.omit(output[output$SQNO%in%pred$SQNO[i],
            }else{
                          c('SQNO','DATE',variable)])
                          output$SQNO%in%pred$SQNO[i],
                pred[i,] = na.omit(output[round(output$DATE,'days')%in%round(pred$DATE[i],'days')&
            if('DATE'%in%colnames(output)){
        }else{
            }
                          c('TRNO',variable)])
                pred[i,] = na.omit(output[output$TRNO%in%pred$TRNO[i],
            }else{
                          c('TRNO','DATE',variable)])
                          output$TRNO%in%pred$TRNO[i],
                pred[i,] = na.omit(output[round(output$DATE,'days')%in%round(pred$DATE[i],'days')&
            if('DATE'%in%colnames(output)){
        if(is.null(sqno)){
    for (i in 1:nrow(pred)){
    pred[,3] = 0
    pred = obs
    }
        }
              c('SQNO',variable)]
            obs = fileat[!is.na(fileat[,variable])&fileat$SQNO%in%sqno,
        }else{
              c('SQNO','DATE',variable)]
            obs = fileat[!is.na(fileat[,variable])&fileat$SQNO%in%sqno,
        if('DATE'%in%colnames(fileat)){
    }else{
        }
                c('TRNO',variable)]
            obs = fileat[!is.na(fileat[,variable])&fileat$TRNO%in%trno,
        }else{
                c('TRNO','DATE',variable)]
            obs = fileat[!is.na(fileat[,variable])&fileat$TRNO%in%trno,
        if('DATE'%in%colnames(fileat)){
        }
            }
                trno = as.integer(as.character(levels(as.factor(output[output$RUN%in%run,]$TRNO))))
            if(!is.null(run)){
        }else{
            trno = as.integer(as.character(levels(as.factor(output$TRNO))))
        if (is.null(trno)&is.null(run)){
    if (is.null(sqno)){
    }
        stop(paste(variable,'not found in File A or File T.'))
    if(!variable%in%colnames(fileat)){
    }
        }
            if(variable%in%colnames(fileat)) break
            fileat=get(file)
        if (exists(file)){
    for( file in c('filet','filea')){
    }
        stop('Neither File T nor File A have been loaded.')
    if(!exists('filet')&!exists('filea')){
    }
        stop(paste(variable,'not found in any loaded model output file.'))
    if(!variable%in%colnames(output)){
    }
        }
            }
                break
                  variable,colnames(output))
                colnames(output)=gsub(paste(variable,'S',sep=''),
              paste(variable,'S',sep='')%in%colnames(output)){
            }else if(file=='Evaluate'&
                break
            if(variable%in%colnames(output)){
            output=get(file)
        if (exists(file)){
    for(file in c('output','PlantGro','Evaluate','Summary')){
    }
        stop('Model outputs have not been loaded.')
       !exists('Evaluate')&!exists('Summary')){
    if(!exists('output')&!exists('PlantGro')&
get.obs.pred <- function(variable,sqno=NULL,trno=NULL,run=NULL,add=NULL){ 

}
    return(rrmse)
    rrmse=round(rrmse,digits=2)
    rrmse=sqrt(mean((data$obs-data$pred)^2))/mean(data$obs)
rrmse <- function(data){

}
    return(rmse)
    rmse=signif(rmse,4)
    rmse=sqrt(mean((data$obs-data$pred)^2))
rmse <- function(data){

}
    return(ceff)
    ceff=round(ceff,2)
    ceff=1-sum((data$obs-data$pred)^2)/sum((data$obs-obsmean)^2)
    obsmean=mean(data$obs)
ceff <- function(data){

}
    return(willmott)
    willmott = round(willmott,2)
    willmott = 1-sum((data$obs-data$pred)^2)/sum((abs(data$pred-mean(data$obs))+abs(data$obs-mean(data$obs)))^2)
dstat <- function(data){

}
    assign('output',output,envir=globalenv())
    }
        output=output[order(output$DATE),]
    }else{
        output=output[order(output$TRNO,output$DATE),]
    if ('TRNO'%in%colnames(output)){
    }
        }
            }
                output = merge(output,temp,by='DATE',sort=T,all=T)
                    colnames(temp)%in%'DATE']
                temp = temp[,!colnames(temp)%in%colnames(output)|
            }else{
                output = merge(output,temp,by=c('TRNO','DATE'),sort=T,all=T)
                    colnames(temp)%in%c('TRNO','DATE')]
                temp = temp[,!colnames(temp)%in%colnames(output)|
            if(is.null(sqno)){
        }else if(!is.null(temp)){
            assign('output',temp)
        if(!is.null(temp)&&!exists('output')){
        }
            if(!is.null(sqno)) colnames(temp)[1] = 'SQNO'
#            read.dssat.binary(files[i])
        }else if(type=='R'&&any(grepl(tolower(files[i]),dirfiles))){
            temp = eval(as.name(files[i]))
                       model,sqno=sqno)
            read.dssat(fileout=paste(files[i],'.OUT',sep=''),
        if (type=='OUT'&&any(dirfiles==paste(files[i],'.',type,sep=''))) {
        temp = NULL
    for (i in 1:length(files)) {
    if(exists('output')) rm('output',inherits=T)
    dirfiles = list.files()
    files = c('PlantGro','PlantN','PlantC','PlantP','ETPhot','Weather','SoilWat','SoilNi','SoilPi','Dormancy','SOMLITC','SOMLITN')
load.dssat = function(model='cropgro',sqno=NULL,type='OUT'){

}
    assign(substr(fileout,start=1,stop=nchar(fileout)-4),pdata,envir=parent.frame())
    }
        }
            pdata=temp
        }else{
            pdata=merge(pdata,temp,all=T)
        if(exists('pdata')){
        temp = read.table(textConnection(filelines[headers[i]:end]),header=T,comment.char="!",na.strings='-99')
        }
            end = length(filelines)
        }else{
            end = headers[i+1]-1
        if(i<length(headers)){
    for (i in 1:length(headers)){
    headers = grep('@RUN',filelines)
    filelines = readLines(fileout)
read.evaluate <- function(fileout,model='cropgro'){

}
    assign(substr(fileout,start=1,stop=nchar(fileout)-4),pdata,envir=parent.frame())
    colnames(pdata) = cnames
    cnames = cnames[cnames!='']
    cnames = unlist(strsplit(gsub('@','',filelines[header]),split='  *'))
    pdata = read.table(fileout,header=F,skip=header,comment.char="!",na.strings='-99')
    header = (1:length(filelines))[substr(filelines,1,1)=='@']
    filelines = readLines(fileout)
read.summary <- function(fileout,model='cropgro'){

}
    return(names)
            }))
                    x)
                    SL.20D='SL%20D',
                    IR.C='IR#C',
                    N.HN='N%HN',
                    N.LN='N%LN',
                    LI.N='LI%N',
                    LI.D='LI%D',
                    GC.D='GC%D',
                    GL.D='GL%D',
                    CS.D='CS%D',
                    CL.D='CL%D',
                    SH.D='SH%D',
                    G.AD='G#AD',
                    P.AD='P#AD',
                    L.SD='L#SD',
                    RN.D='RN%D',
                    GN.D='GN%D',
                    VN.D='VN%D',
                    SN.D='SN%D',
                    LN.D='LN%D',
            switch(x,
    names=unlist(lapply(names,FUN=function(x){
fix.names <- function(names){

}
           pdata,envir=parent.frame())
    assign(substr(fileout,start=1,stop=nchar(fileout)-4),
    }
      }
        }
            pdata = pdata[!(1:nrow(pdata))%in%trash,]
            trash = trash[c(trash[1:(length(trash)-1)]==temp-1,F)]
            temp = trash[2:length(trash)]
            trash = (1:nrow(pdata))[paste(pdata[,1],pdata$DATE)%in%dates]
            dates=names(freq[freq>1])
        if(any(freq>1)){
        freq=table(paste(pdata[,1],pdata$DATE))
      if('DATE'%in%colnames(pdata)){
           colnames(pdata)=fix.names(colnames(pdata))
           colnames(pdata)[grepl('TRNO',colnames(pdata))]='SQNO'
    if(!is.null(sqno)&'TRNO'%in%colnames(pdata)){
    }
        }
            pdata=temp
        }else{
            pdata=merge(pdata,temp,all=T)
        if(exists('pdata')){
        }
            }
              colnames(temp) = c('SQNO','RUN','DATE',cnames)
              temp = cbind(SQNO,RUN,DATE,temp)
              SQNO = rep(sqno[i],nrow(temp))
            }else{
              colnames(temp) = c('TRNO','RUN','DATE',cnames)
              temp = cbind(TRNO,RUN,DATE,temp)
            if(is.null(sqno)){
            cnames=colnames(temp)
            temp = temp[,!colnames(temp)%in%c('DAP','DAS','YEAR','DOY')]
            DATE = as.POSIXct(paste(temp$YEAR,temp$DOY,sep=''),format='%Y%j')
            if(!any(grepl('RUN',colnames(temp)))) RUN = rep(runs[i],nrow(temp))
               !'TR'%in%colnames(temp)) TRNO = rep(trts[i],nrow(temp))
            if(!any(grepl('TRNO',colnames(temp)))&
            colnames(temp)[grepl('YEAR',colnames(temp))]='YEAR'
            colnames(temp) = parse.header(filelines[headers[i]])
                            widths=widths)
                            blank.lines.skip=T,comment.char="!",
                            colClasses='numeric',header=F,
                            skip=headers[i],nrows=nrows[i],
            temp = read.fwf(file=fileout,
        }else{
            colnames(temp) = parse.header(filelines[headers[i]])
                            widths=widths)
                            header=F,blank.lines.skip=T,comment.char="!",
                            skip=headers[i],nrows=nrows[i],
            temp = read.fwf(file=fileout,
           tolower(fileout)=='evaluate.out'){
        if(tolower(fileout)=='summary.out'|
        widths = get.name.widths(filelines[headers[i]])
    for (i in 1:length(headers)){
    filelines = gsub('@',' ',filelines)
                           sqno[length(headers)%%length(sqno)])
    if(!is.null(sqno)) sqno = c(rep(sqno,length(headers)%/%length(sqno)),
    }
        nrows = ends - headers
        }
            ends = length(filelines)
        }else{
            ends = c(headers[2:length(headers)]-1,length(filelines))
        if(length(headers)>1){
    }else{
        }
          nrows = length(filelines)-headers
        }else{
          nrows = c(nrows,length(filelines) - headers[length(headers)])
          nrows = ends[2:length(ends)] - headers[1:(length(headers)-1)]-1
        if(length(headers)>1) {
        if(!is.null(sqno)) sqno = sqno[sqno%in%trts]
        runs = as.integer(substring(runs,5,8))
        runs = filelines[substring(filelines,1,4)=='*RUN']
        trts = as.integer(substring(trts,11,13))
        trts = filelines[substring(filelines,1,10)==' TREATMENT']
        cnames[3] = 'YEAR'
           unlist(strsplit(filelines[headers[1]],split='  *')))
        cnames = c('TRNO','RUN',
        ends = (1:length(filelines))[substring(filelines,1,6)=='*DSSAT']-1
       tolower(fileout)!='evaluate.out'){
    if(tolower(fileout)!='summary.out'&
    headers = (1:length(filelines))[substring(filelines,1,1)=='@']
    filelines = readLines(fileout)
    options(stringsAsFactors=F)
#    on.exit(options(stringsAsFactors=prev.saf))
#    prev.saf = options('stringsAsFactors')
read.dssat <- function(fileout,model='cropgro',sqno=NULL){

}
    return(widths)
    }
        widths[i] = attr(x,'match.length')
        x = regexpr(paste('  *',names[i],sep=''),header)
        names[i] = gsub('\\#','\\\\#',gsub('\\+','\\\\+',names[i]))
    for(i in 1:length(names)){
    widths = vector('integer',length=length(names))
    names = parse.header(header)
get.name.widths <- function(header){

}
    return(names)
    }
        names=gsub('  *','',names)
        }
            names[i]=substr(header,((i-1)*6+1),i*6)
        for(i in 1:length(names)){
        names=vector(length=ceiling(nchar(header)/6),mode='character')
    if(any(nchar(names)>6)){
    names = names[names!='']
    names = unlist(strsplit(header,'  *'))
parse.header <- function(header){

}
    return(pdate)
    pdate=as.POSIXct(as.character(pdate),format='%Y%j')
    }
        pdate=pdate+1900000
    }else{
        pdate=pdate+2000000
    if(pdate<20000){
    pdate = filex[[sec]][[subsec]]$PDATE[filex[[sec]][[subsec]]$P%in%xmthplt]
    subsec = grep('PDATE',filex[[sec]])
    sec = grep('PDATE',filex)
    }
        xmthplt = filex[[sec]][[subsec]]$MP[xsq%in%sqno]
        xsq = filex[[sec]][[subsec]]$R
    }else{
        xmthplt = filex[[sec]][[subsec]]$MP[xtrno%in%trno]
        xtrno = filex[[sec]][[subsec]]$N
    if(is.null(sqno)){
    subsec = grep('TNAME',filex[[sec]])
    sec = grep('TNAME',filex)
    fname = filexname
    filex = read.filex(filexname)
    fname = ''
getpltdate <- function(filexname,trno=NULL,sqno=NULL){

}
    write(fileat.lines,file=filename)
    fileat.lines = gsub('    NA','      ',fileat.lines)
    
    }
        fileat.lines[l1:l2] = paste(fileat.lines[l1:l2],sprintf(formatting[i],fileat[,i]),sep='')
    for (i in 1:ncol(fileat)){
    l2=length(fileat.lines)
    l1=length(info)+4

#    formatting = formatting[3:length(formatting)]
    formatting[fclass=='character'] = '%6s'
    fclass = unlist(lapply(fileat,class))
    formatting[cnames%in%c('L#SD','GWGD')] = '%6.1f'
#    formatting[cnames%in%c('SHND','LAID','G#PD')] = '%6.2f'
#    formatting[cnames%in%c('LFFD','STFD','RSRD')] = '%6.2f'
    formatting[cnames%in%c('SL%20D','HWUM')] = '%6.3f'
    formatting[grepl('HI',cnames)] = '%6.3f'
#    formatting[grepl('%',cnames)] = '%6.2f'
    formatting[grepl('DAP',cnames)] = '%6.0f'
    formatting[grepl('TRNO',cnames)] = '%6.0f'
    formatting[grepl('AD',cnames)] = '%6.0f'
    formatting[grepl('AM',cnames)] = '%6.0f'
    formatting[] = '%6.2f'
    formatting = vector(length=ncol(fileat),mode='character')

    fileat.lines[length(info)+3] = paste(header,collapse='')
    fileat.lines[3:(length(info)+2)] = paste('!',info)
    fileat.lines[1] = paste('*EXP. DATA (',type,'):',title)
    fileat.lines[] = ''
    fileat.lines = vector(length = nrow(fileat)+3+length(info),mode='character')
    
#    colnames(fileat) = header
#    fileat[,2] = as.character(fileat[,2])
#                            FUN=function(x) as.numeric(as.character(x))))
#    fileat[3:ncol(fileat)] = as.data.frame(lapply(fileat[3:ncol(fileat)],
#    }
#        fileat = as.data.frame(cbind(fileat$TRNO,DATE,fileat[,!colnames(fileat)%in%c('TRNO','DATE','DAP')]))
#    }else{
#        fileat = as.data.frame(cbind(fileat$SQNO,DATE,fileat[,!colnames(fileat)%in%c('SQNO','DATE','DAP')]))
#    if('SQNO'%in%colnames(fileat)){
     colnames(fileat) = cnames
        return(x)}))
        }
                sep='')
                        as.POSIXlt(x)$yday+1))),
                    sprintf('%3.3i',as.integer(as.character(
            x = paste(substr(as.character(as.POSIXlt(x)$year+1900),3,4),
        if(any(grepl('POSIXt',class(x)))){
    fileat=data.frame(lapply(fileat,function(x){
    cnames = colnames(fileat)
    header = gsub('  *TRNO','@TRNO ',header)
    header = sprintf('%6s',colnames(fileat))
    if(is.na(type)) type=substr(filename,nchar(filename),nchar(filename))
#    header = header[!header%in%c('TRNO','DATE','DAP')]
#    header = colnames(filet)
write.fileat <- function(fileat,filename,type=NA,title='',info=''){

}
    return(invisible(fileat))
    }
        colnames(fileat)[1]='SQNO'
    if(!is.null(sqno)){
    colnames(fileat)=gsub('@','',colnames(fileat))
#    assign(tolower(substr(filename,1,8)),fileat,envir=globalenv())
    }
        }
            fileat=temp
        }else{
            fileat=merge(fileat,temp,by=c('@TRNO','DATE'))
        if(i>1){
        colnames(temp)=cnames
        }
            }
                temp[,j]=as.numeric(temp[,j])
            }else{
                }
                    temp[,j]=as.POSIXct(temp[,j],format='%Y%j')
                    temp[,j]=paste(19,temp[,j],sep='')
                }else{
                    temp[,j]=as.POSIXct(temp[,j],format='%Y%j')
                    temp[,j]=paste(20,temp[,j],sep='')
                if(as.numeric(substr(temp[1,j],1,2))<20){
                temp[,j]=gsub(' ','',temp[,j])
            if(grepl('DAT',cnames[j])){
        for(j in 1:length(cnames)){
        temp=as.data.frame(temp)
        }
            temp[,j]=substr(filelines[(headlines[i]+1):(headlines[i]+nrows)],((j-1)*6+1),j*6)
        for(j in 1:length(cnames)){
        temp=matrix(nrow=nrows,ncol=length(cnames))
        }
            nrows = nlines - headlines[i]
        }else{
            nrows = headlines[i+1] - headlines[i] - 1
        if (i < length(headlines)){
        cnames=parse.header(filelines[headlines[i]])
    for (i in 1:length(headlines)){
    headlines=grep('@TRNO',filelines)
    nlines=length(filelines)
    filelines=readLines(filename)
    classes='numeric'
read.fileat <- function(filename,sqno=NULL){

}
    return(output)
    colnames(output)=c('@  SLB','  SLMH','  SLLL','  SDUL','  SSAT','  SRGF','  SSKS','  SBDM','  SLOC','  SLCL','  SLSI','  SLCF','  SLNI','  SLHW','  SLHB','  SCEC','  SADC')
    output=cbind(depth,SLMH,SLLL,SDUL,SSAT,SRGF,SSKS,SBDM,SLOC,SLCL,SLSI,SLCF,SLNI,SLHW,SLHB,SCEC,SADC)
    SADC = rep(0,nrow(soil))
    SCEC = unknown
    SLHB = unknown
    SLHW = unknown
    SLNI = unknown
    SLCF = unknown
    SLMH = unknown
    unknown=as.numeric(rep(-99,nrow(soil)))
    }
        if(layercenter>20) SRGF[i] = exp(-0.02*layercenter) else SRGF[i] = 1
        SDUL[i]=exp(log(33/A)/B)
        SLLL[i]=exp(log(1500/A)/B)
        B = -3.140-0.00222*clay[i]^2-(3.484e-5)*sand[i]^2*clay[i]
        A = exp(-4.396-0.0715*clay[i]-(4.880e-4)*sand[i]^2-(4.285e-5)*sand[i]*clay[i])*100
        SKSS[i] = get.ksat(sand[i],silt[i],clay[i])
#        SSKS[i] = 2.778e-6*exp((12.012-0.0755*sand[i]+(-3.8950+0.03671*sand[i]-0.1103*clay[i]+8.7546e-4*clay[i]^2))/SSAT[i])
#        SSAT[i] = 0.332-7.251e-4*(sand[i])+0.1276*log10(clay[i])
        SSAT[i] = get.ssat(som[i],SBDM[I],minBD)
        SBDM[i] = get.sbdm(som[i],minBD)
        minBD=minBDtab[ceiling(clay[i]/5),ceiling(sand[i]/5)]
        if(i>1) layercenter = depth[i-1]+(depth[i]-depth[i-1])/2 else layercenter = depth[i]/2
    for(i in 1:nrow(soil)){
    SRGF = vector(length=nrow(soil),mode='numeric')
    SDUL = vector(length=nrow(soil),mode='numeric')
    SLLL = vector(length=nrow(soil),mode='numeric')
    SSKS = vector(length=nrow(soil),mode='numeric')
    SSAT = vector(length=nrow(soil),mode='numeric')
    SBDM = vector(length=nrow(soil),mode='numeric')
    SLSI = soil$silt
    SLCL = soil$clay
    SLOC = soil$soc
    depth = soil$depth
    som = soil$soc*1.72
    clay = soil$clay
    sand = 100-soil$clay-soil$silt
    minBDtab = dget('~/R/source/rawlsbrakensiek1985fig1.r')
SOILest <- function(soil){

}
    
    new.bd = new.bd/(1-coarse/100)
    new.bd = get.sbdm(som,minBD)
    minBD = get.minBD(sand,clay)
    coarse = soil.profile$layer$SLCF
    som = soil.profile$layer$SLOC*1.72
    sand = 100 - silt - clay
    clay = soil.profile$layer$SLCL
    silt = soil.profile$layer$SLSI
cf.bd.adjust <- function(soil.profile){

}
    soil.profile$layer$SLOC[i]=sloc
    sloc = sloc*(1-coarse/100)
    sloc = soil.profile$layer$SLCF[i]
    coarse = soil.profile$layer$SLCF[i]
    i=!is.na(soil.profile$layer$SLCF)
cf.sloc.adjust <- function(soil.profile){

}
    return(soil.profile$SDUL)
    soil.profile$layer$SDUL[i] = sdul
    sdul = (sdul-slll)*(1-coarse/100)+slll
    sdul = soil.profile$layer$SDUL[i]
    slll = soil.profile$layer$SLLL[i]
    coarse = soil.profile$layer$SLCF[i]
    i=!is.na(soil.profile$layer$SLCF)
cf.slll.adjust <- function(soil.profile){

}
    return(soil.profile$SDUL)
    soil.profile$layer$SDUL[i] = sdul
    sdul = (sdul-slll)*(1-coarse/100)+slll
    sdul = soil.profile$layer$SDUL[i]
    slll = soil.profile$layer$SLLL[i]
    coarse = soil.profile$layer$SLCF[i]
    i=!is.na(soil.profile$layer$SLCF)
cf.sdul.adjust <- function(soil.profile){

}
    return(soil.profile)
#    }
#        }
#            soil.profile$layer[,i] = -99.0
#        if(any(is.na(soil.profile$layer[,i]))){
#    for(i in 1:ncol(soil.profile$layer)){
#    }
#        soil.profile$layer$SADC[na] = 0.0
#    if(any(na)){
#    na = is.na(soil.profile$layer$SADC)
#    }
#        soil.profile$layer$SLMH[na] = ''
#    if(any(na)){
#    na = is.na(soil.profile$layer$SLMH)
    }
        soil.profile$layer$SRGF[na] = get.srgf(soil.profile$layer$SLB)[na]
    if(any(na)){
    na = is.na(soil.profile$layer$SRGF)
    }
            get.slll.saxton(sand,clay,coarse)[na]
        soil.profile$layer$SLLL[na] = 
    if(any(na)){
    na = is.na(soil.profile$layer$SLLL)
    }
            get.sdul.saxton(sand,clay,coarse)[na]
        soil.profile$layer$SDUL[na] = 
    if(any(na)){
    na = is.na(soil.profile$layer$SDUL)
    }
        soil.profile$layer$SSKS[na] = get.ksat(sand,silt,clay)[na]
    if(any(na)){
    na = is.na(soil.profile$layer$SSKS)
    }
            get.ssat(som,soil.profile$layer$SBDM,minBD,coarse)[na]
        soil.profile$layer$SSAT[na] = 
    if(any(na)){
    na = is.na(soil.profile$layer$SSAT)
    }
        soil.profile$layer$SBDM[na] = get.sbdm(som,minBD)[na]
    if(any(na)){
    na = is.na(soil.profile$layer$SBDM)
    minBD = get.minBD(sand,clay)
    som = soil.profile$layer$SLOC*1.72
    sand = 100 - silt - clay
    coarse = soil.profile$layer$SLCF
    clay = soil.profile$layer$SLCL
    silt = soil.profile$layer$SLSI
fill.in.profile <- function(soil.profile){

}
    return(slll)
#    }
#        slll[!na] = slll[!na]*(1-coarse[!na]/100)
#    if(any(!na)){
#    na=is.na(coarse)
    slll=exp(log(1500/A)/B)
    B = -3.140-0.00222*clay^2-(3.484e-5)*sand^2*clay
    A = exp(-4.396-0.0715*clay-(4.880e-4)*sand^2-(4.285e-5)*sand*clay)*100
get.slll.saxton <- function(sand,clay,coarse){

}
    return(sdul)
#    }
#        sdul[!na] = sdul[!na]*(1-coarse[!na]/100)
#    if(any(!na)){
#    na=is.na(coarse)
    sdul=exp(log(33/A)/B)
    B = -3.140-0.00222*clay^2-(3.484e-5)*sand^2*clay
    A = exp(-4.396-0.0715*clay-(4.880e-4)*sand^2-(4.285e-5)*sand*clay)*100
get.sdul.saxton <- function(sand,clay,coarse){

}
    return(srgf)
    srgf[lyr.center > 20] = exp(-0.02*lyr.center[lyr.center > 20])
    lyr.center = (slb - c(0,slb[1:(length(slb)-1)]))/2
    srgf[] = 1
    srgf = vector(length=length(slb),mode='numeric')
get.srgf <- function(slb){

}
    return(sbdm)
    sbdm = 100/(som/0.224+(100-som)/minBD)
get.sbdm <- function(som,minBD){

}
    return(minBD)
    }
        minBD[i]=minBDtab[ceiling(clay[i]/5),ceiling(sand[i]/5)]
    for (i in 1:length(minBD)){
    minBD = vector(length=length(clay),mode='numeric')
-21L))
"V19", "V20", "V21", "V22", "V23"), class = "data.frame", row.names = c(NA, 
"V10", "V11", "V12", "V13", "V14", "V15", "V16", "V17", "V18", 
    )), .Names = c("V3", "V4", "V5", "V6", "V7", "V8", "V9", 
    -99, -99, -99, -99, -99, -99, -99, -99, -99, -99, -99, -99
    ), V23 = c(1.53, -99, -99, -99, -99, -99, -99, -99, -99, 
    -99, -99, -99, -99, -99, -99, -99, -99, -99, -99, -99, -99
    ), V22 = c(1.52, 1.57, -99, -99, -99, -99, -99, -99, -99, 
    -99, -99, -99, -99, -99, -99, -99, -99, -99, -99, -99, -99
    ), V21 = c(1.52, 1.56, 1.64, -99, -99, -99, -99, -99, -99, 
    -99, -99, -99, -99, -99, -99, -99, -99, -99, -99, -99, -99
    -99), V20 = c(1.5, 1.6, 1.64, 1.7, -99, -99, -99, -99, -99, 
    -99, -99, -99, -99, -99, -99, -99, -99, -99, -99, -99, -99, 
    -99), V19 = c(1.4, 1.55, 1.63, 1.7, 1.72, -99, -99, -99, 
    -99, -99, -99, -99, -99, -99, -99, -99, -99, -99, -99, -99, 
    -99), V18 = c(1.44, 1.51, 1.6, 1.65, 1.68, 1.69, -99, -99, 
    -99, -99, -99, -99, -99, -99, -99, -99, -99, -99, -99, -99, 
    -99), V17 = c(1.4, 1.47, 1.57, 1.62, 1.64, 1.66, 1.67, -99, 
    -99, -99, -99, -99, -99, -99, -99, -99, -99, -99, -99, -99, 
    -99), V16 = c(1.35, 1.43, 1.53, 1.6, 1.62, 1.63, 1.65, 1.66, 
    1.63, -99, -99, -99, -99, -99, -99, -99, -99, -99, -99, -99, 
    ), V15 = c(1.3, 1.38, 1.51, 1.55, 1.59, 1.61, 1.62, 1.63, 
    1.62, -99, -99, -99, -99, -99, -99, -99, -99, -99, -99, -99
    V14 = c(1.27, 1.33, 1.46, 1.52, 1.56, 1.58, 1.61, 1.62, 1.62, 
1.61, 1.61, -99, -99, -99, -99, -99, -99, -99, -99, -99, -99), 
-99), V13 = c(1.15, 1.29, 1.4, 1.5, 1.53, 1.55, 1.58, 1.59, 1.61, 
1.57, 1.58, 1.58, 1.56, -99, -99, -99, -99, -99, -99, -99, -99, 
-99, -99), V12 = c(1.18, 1.25, 1.35, 1.44, 1.5, 1.52, 1.54, 1.56, 
1.51, 1.54, 1.54, 1.53, 1.52, 1.5, -99, -99, -99, -99, -99, -99, 
-99, -99, -99), V11 = c(1.14, 1.23, 1.31, 1.4, 1.45, 1.48, 1.5, 
1.47, 1.48, 1.48, 1.48, 1.47, 1.45, 1.44, -99, -99, -99, -99, 
-99, -99, -99), V10 = c(1.12, 1.21, 1.29, 1.35, 1.42, 1.45, 1.46, 
1.43, 1.44, 1.44, 1.44, 1.43, 1.42, 1.41, 1.35, -99, -99, -99, 
-99, -99, -99), V9 = c(1.08, 1.17, 1.25, 1.3, 1.35, 1.39, 1.42, 
1.37, 1.39, 1.4, 1.41, 1.41, 1.4, 1.36, 1.3, 1.25, -99, -99, 
-99, -99, -99), V8 = c(0.99, 1.12, 1.2, 1.23, 1.27, 1.3, 1.35, 
1.32, 1.35, 1.37, 1.37, 1.37, 1.37, 1.35, 1.31, 1.26, 1.2, -99, 
-99, -99, -99), V7 = c(1, 1.1, 1.18, 1.22, 1.25, 1.27, 1.29, 
1.34, 1.35, 1.37, 1.37, 1.37, 1.37, 1.37, 1.33, 1.32, 1.25, 1.17, 
1.18, -99, -99), V6 = c(1.12, 1.2, 1.26, 1.3, 1.3, 1.3, 1.31, 
1.4, 1.39, 1.39, 1.38, 1.38, 1.38, 1.38, 1.37, 1.34, 1.32, 1.24, 
1.2, 1.17, -99), V5 = c(1.25, 1.35, 1.4, 1.4, 1.4, 1.4, 1.4, 
1.46, 1.45, 1.45, 1.44, 1.43, 1.42, 1.41, 1.4, 1.37, 1.33, 1.28, 
1.23, 1.2, 1.16), V4 = c(1.4, 1.45, 1.46, 1.46, 1.47, 1.47, 1.47, 
1.52, 1.5, 1.48, 1.47, 1.47, 1.45, 1.43, 1.42, 1.4, 1.33, 1.28, 
structure(list(V3 = c(1.49, 1.52, 1.52, 1.52, 1.52, 1.52, 1.52, 
    minBDtab = 
# The following table was taken from figure 1 of Rawls & Brakensiek (1985)
#    minBDtab = dget('~/R/source/rawlsbrakensiek1985fig1.r')
get.minBD <- function(sand,clay){

}
    return(ssat)
#    }
#        ssat[!na] = ssat[!na]*(1-coarse[!na]/100)
#    if(any(!na)){
#    na=is.na(coarse)
    ssat = 0.95*(1/sbdm-1/(som/0.224+(100-som)/minBD))
get.ssat <- function(som,sbdm,minBD,coarse){

}
    return(ksat)
    }
        }
            ksat[i] = 0.06
        }else if(clay[i] >= 40 & sand[i] < 45 & silt[i] < 40){
            ksat[i] = 0.09
        }else if(clay[i] >= 40 & silt[i] >= 40){
            ksat[i] = 0.12
        }else if(clay[i] >= 35 & sand[i] >= 45){
            ksat[i] = 0.15
        }else if(clay[i] >= 27 & clay[i] < 40 & sand[i] >= 0 & sand[i] < 20){
            ksat[i] = 0.23
        }else if(clay[i] >= 27 & clay[i] < 40 & sand[i] >= 20 & sand[i] < 45){
            ksat[i] = 0.43
        }else if(clay[i] >= 20 & clay[i] < 35 & silt[i] >= 0 & silt[i] < 28 & sand[i] >= 45){
            ksat[i] = 0.68
                (silt[i] >= 50 & clay[i] >= 0 & clay[i] < 12)){
        }else if((silt[i] >= 50 & silt[i] <= 88 & clay[i] >= 12 & clay[i] <= 27) |
            ksat[i] = 1.32
                sand[i] >= 23 & sand[i] < 52){
        }else if(clay[i] >= 7 & clay[i] <= 27 & silt[i] >= 28 & silt[i] < 50 &
            ksat[i] = 2.59
                (clay[i] < 7 & silt[i] < 50 & sand[i] > 43 & sand[i] < 52)){
        }else if((clay[i] <= 20 & sand[i] >= 52 & silt[i]+2*clay[i] > 30) |
            ksat[i] = 6.11
                (sand[i] >= 70 & sand[i] < 85 & silt[i]+2*clay[i] <=30)){
