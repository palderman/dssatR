write.fileat <- function(fileat,filename,type=NA,title='',info=''){
#    header = colnames(filet)
#    header = header[!header%in%c('TRNO','DATE','DAP')]
    if(is.na(type)) type=substr(filename,nchar(filename),nchar(filename))
    header = sprintf('%6s',colnames(fileat))
    header = gsub('  *TRNO','@TRNO ',header)
    cnames = colnames(fileat)
    fileat=data.frame(lapply(fileat,function(x){
        if(any(grepl('POSIXt',class(x)))){
            x = paste(substr(as.character(as.POSIXlt(x)$year+1900),3,4),
                    sprintf('%3.3i',as.integer(as.character(
                        as.POSIXlt(x)$yday+1))),
                sep='')
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

    formatting = vector(length=ncol(fileat),mode='character')
    formatting[] = '%6.2f'
    formatting[grepl('AM',cnames)] = '%6.0f'
    formatting[grepl('AD',cnames)] = '%6.0f'
    formatting[grepl('TRNO',cnames)] = '%6.0f'
    formatting[grepl('DAP',cnames)] = '%6.0f'
#    formatting[grepl('%',cnames)] = '%6.2f'
    formatting[grepl('HI',cnames)] = '%6.3f'
    formatting[cnames%in%c('SL%20D','HWUM')] = '%6.3f'
#    formatting[cnames%in%c('LFFD','STFD','RSRD')] = '%6.2f'
#    formatting[cnames%in%c('SHND','LAID','G#PD')] = '%6.2f'
    formatting[cnames%in%c('L#SD','GWGD')] = '%6.1f'
    fclass = unlist(lapply(fileat,class))
    formatting[fclass=='character'] = '%6s'
#    formatting = formatting[3:length(formatting)]

    l1=length(info)+4
    l2=length(fileat.lines)
    for (i in 1:ncol(fileat)){
        fileat.lines[l1:l2] = paste(fileat.lines[l1:l2],sprintf(formatting[i],fileat[,i]),sep='')
    }
    
    fileat.lines = gsub('    NA','      ',fileat.lines)
    write(fileat.lines,file=filename)
}

