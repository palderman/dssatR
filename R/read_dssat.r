read.dssat <- function(fileout,model='cropgro',sqno=NULL){
#    prev.saf = options('stringsAsFactors')
#    on.exit(options(stringsAsFactors=prev.saf))
    options(stringsAsFactors=F)
    filelines = readLines(fileout)
    headers = (1:length(filelines))[substring(filelines,1,1)=='@']
    if(tolower(fileout)!='summary.out'&
       tolower(fileout)!='evaluate.out'){
        ends = (1:length(filelines))[substring(filelines,1,6)=='*DSSAT']-1
        cnames = c('TRNO','RUN',
           unlist(strsplit(filelines[headers[1]],split='  *')))
        cnames[3] = 'YEAR'
        trts = filelines[substring(filelines,1,10)==' TREATMENT']
        trts = as.integer(substring(trts,11,13))
        runs = filelines[substring(filelines,1,4)=='*RUN']
        runs = as.integer(substring(runs,5,8))
        if(!is.null(sqno)) sqno = sqno[sqno%in%trts]
        if(length(headers)>1) {
          nrows = ends[2:length(ends)] - headers[1:(length(headers)-1)]-1
          nrows = c(nrows,length(filelines) - headers[length(headers)])
        }else{
          nrows = length(filelines)-headers
        }
    }else{
        if(length(headers)>1){
            ends = c(headers[2:length(headers)]-1,length(filelines))
        }else{
            ends = length(filelines)
        }
        nrows = ends - headers
    }
    if(!is.null(sqno)) sqno = c(rep(sqno,length(headers)%/%length(sqno)),
                           sqno[length(headers)%%length(sqno)])
    filelines = gsub('@',' ',filelines)
    for (i in 1:length(headers)){
        widths = get.name.widths(filelines[headers[i]])
        if(tolower(fileout)=='summary.out'|
           tolower(fileout)=='evaluate.out'){
            temp = read.fwf(file=fileout,
                            skip=headers[i],nrows=nrows[i],
                            header=F,blank.lines.skip=T,comment.char="!",
                            widths=widths)
            colnames(temp) = parse.header(filelines[headers[i]])
        }else{
            temp = read.fwf(file=fileout,
                            skip=headers[i],nrows=nrows[i],
                            colClasses='numeric',header=F,
                            blank.lines.skip=T,comment.char="!",
                            widths=widths)
            colnames(temp) = parse.header(filelines[headers[i]])
            colnames(temp)[grepl('YEAR',colnames(temp))]='YEAR'
            if(!any(grepl('TRNO',colnames(temp)))&
               !'TR'%in%colnames(temp)) TRNO = rep(trts[i],nrow(temp))
            if(!any(grepl('RUN',colnames(temp)))) RUN = rep(runs[i],nrow(temp))
            DATE = as.POSIXct(paste(temp$YEAR,temp$DOY,sep=''),format='%Y%j')
            temp = temp[,!colnames(temp)%in%c('DAP','DAS','YEAR','DOY')]
            cnames=colnames(temp)
            if(is.null(sqno)){
              temp = cbind(TRNO,RUN,DATE,temp)
              colnames(temp) = c('TRNO','RUN','DATE',cnames)
            }else{
              SQNO = rep(sqno[i],nrow(temp))
              temp = cbind(SQNO,RUN,DATE,temp)
              colnames(temp) = c('SQNO','RUN','DATE',cnames)
            }
        }
        if(exists('pdata')){
            pdata=merge(pdata,temp,all=T)
        }else{
            pdata=temp
        }
    }
    if(!is.null(sqno)&'TRNO'%in%colnames(pdata)){
           colnames(pdata)[grepl('TRNO',colnames(pdata))]='SQNO'
           colnames(pdata)=fix.names(colnames(pdata))
      if('DATE'%in%colnames(pdata)){
        freq=table(paste(pdata[,1],pdata$DATE))
        if(any(freq>1)){
            dates=names(freq[freq>1])
            trash = (1:nrow(pdata))[paste(pdata[,1],pdata$DATE)%in%dates]
            temp = trash[2:length(trash)]
            trash = trash[c(trash[1:(length(trash)-1)]==temp-1,F)]
            pdata = pdata[!(1:nrow(pdata))%in%trash,]
        }
      }
    }
    assign(substr(fileout,start=1,stop=nchar(fileout)-4),
           pdata,envir=parent.frame())
}

