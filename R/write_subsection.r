write.subsection <- function(subsection){
    if(is.vector(subsection)){
        sub.lines = vector(length=1,mode='character')
        sub.lines = as.character(subsection)
    }else{
        sub.lines = vector(length=nrow(subsection)+1,mode='character')
        cnames = colnames(subsection)
        ljust = list.ljust(cnames)
        is.ljust = cnames %in% ljust
        cnames.nchar = nchar(cnames)
        widths = get.widths(cnames)
        if('TNAME'%in%cnames&'CU'%in%cnames){
            widths[cnames=='TNAME'] = 25
        }
        cnames = add.periods(cnames)
        for (k in 1:length(cnames)){
            if(cnames.nchar[k]>1){
                cnames[k] = format(cnames[k],
                    width=ifelse(widths[k]<40,widths[k],nchar(cnames[k])),
                    justify=ifelse(cnames[k]%in%ljust,'left','right'))
            }
        }
        cnames = paste(' ',cnames,sep='')
        cnames[grepl('PLNAME',cnames)] = paste(paste(rep(' ',24),collapse=''),'PLNAME',sep='')
        cnames[grepl('ID_SOIL',cnames)] = '  ID_SOIL   '
        cnames[grepl('SANAME',cnames)] = '  SANAME'
        sub.lines[1] = paste(cnames,collapse='')
        substr(sub.lines[1],1,1) = '@'
        for(k in 1:ncol(subsection)){
            if(class(subsection[,k])=='integer'){
                #fmt = paste('%',widths[k],'.',widths[k],'i',sep='')
                #subsection[,k] = sprintf(fmt,subsection[,k])
                subsection[,k] = format(subsection[,k])
            }else if(class(subsection[,k])=='numeric'){
                subsection[,k] = signif(subsection[,k],digits=widths[k])
                if(any(nchar(as.character(subsection[,k]))>widths[k])){
                    for (l in 1:length(subsection[,k])){
                        nwwd = check.width(subsection[l,k],widths[k])
                        subsection[l,k] = signif(subsection[l,k],digits=nwwd)
                    }
                }
                subsection[,k] = format(subsection[,k])
            }else{
                subsection[,k] = format(subsection[,k])
            }
        }
        subsection = matrix(unlist(lapply(subsection,function(x){
                                        x = sub('^ *','',sub(' *$','',x))
                                        x = gsub('^NA','-99',x)
                                        return(x)})),ncol=length(cnames))
        for (k in 1:ncol(subsection)){
            if(is.ljust[k]){
                just = 'left'
            }else{
                just = 'right'
            }
            if(grepl('PLNAME',cnames[k])){
                subsection[,k] = paste(paste(rep(' ',23),collapse=''),subsection[,k],sep='')
            }else if(grepl('ID_SOIL',cnames[k])){
                subsection[,k] = paste(' ',subsection[,k],sep='')
            }else if(grepl('SANAME',cnames[k])){
                subsection[,k] = paste(' ',subsection[,k],sep='')
            }
            if(widths[k]<40){
                subsection[,k] = format(subsection[,k],justify=just,width=widths[k])
            }
            if((cnames.nchar[k]>1&k>1)|grepl('PAREA',cnames[k])){
                subsection[,k] = paste(' ',subsection[,k],sep='')
            }
        }
        for(k in 1:nrow(subsection)){
            sub.lines[k+1] = paste(subsection[k,],collapse='')
        }
    }
    return(sub.lines)
}

