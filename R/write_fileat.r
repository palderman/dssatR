write.fileat <- function(fileat,filename,type=NA,title='',info=''){
    if(is.na(type)) type=substr(filename,nchar(filename),nchar(filename))
    
    cnames = colnames(fileat)
    header = sprintf('%6s',cnames)
    width = nchar(gsub(' *','',cnames))+1
    width[width<6] = 6
    width[grepl('DAT$',cnames)] = 6
    hd.fmt = paste('%',width,'s',sep='')
    header = gsub('  *TRNO','@TRNO ',header)
    fileat=data.frame(lapply(fileat,function(x){
        if(any(grepl('POSIXt',class(x)))){
            yr = as.POSIXlt(x)$year
            yr[!is.na(yr)&&yr>100] = yr[!is.na(yr)&&yr>100] - 100
            yrdoy = yr*1000 +
                as.integer(as.character(as.POSIXlt(x)$yday+1))
            x = yrdoy
        }
        return(x)}))
     colnames(fileat) = cnames
#    if('SQNO'%in%colnames(fileat)){
#        fileat = as.data.frame(cbind(fileat$SQNO,DATE,fileat[,!colnames(fileat)%in%c('SQNO','DATE','DAP')]))
#    }else{
#        fileat = as.data.frame(cbind(fileat$TRNO,DATE,fileat[,!colnames(fileat)%in%c('TRNO','DATE','DAP')]))
#    }
#    fileat[3:ncol(fileat)] = as.data.frame(lapply(fileat[3:ncol(fileat)],
#                            FUN=function(x) as.numeric(as.character(x))))
#    fileat[,2] = as.character(fileat[,2])
#    colnames(fileat) = header
    
    fileat.lines = vector(length = nrow(fileat)+3+length(info),mode='character')
    fileat.lines[] = ''
    fileat.lines[1] = paste('*EXP. DATA (',type,'):',title)
    fileat.lines[3:(length(info)+2)] = paste('!',info)
    fileat.lines[length(info)+3] = paste(header,collapse='')

    fmt.list = fmt.default()
    fmt = vector(length=ncol(fileat),mode='character')
    for(i in 1:length(fmt.list)){
        fmt[cnames%in%fmt.list[[i]]] = names(fmt.list)[i]
    }
    
    l1=length(info)+4
    l2=length(fileat.lines)
    for (i in 1:ncol(fileat)){
        fileat.lines[l1:l2] = paste(fileat.lines[l1:l2],sprintf(fmt[i],fileat[,i]),sep='')
    }
    
    fileat.lines = gsub('    NA','      ',fileat.lines)
    write(fileat.lines,file=filename)
    return(invisible(fileat.lines))
}

