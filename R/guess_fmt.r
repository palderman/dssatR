guess.fmt <- function(lines){
    cnames = strsplit(gsub('@','',lines[1]),split='  *')[[1]]
    cnames = cnames[cnames!='']
    pos1=integer(length=length(cnames))
    pos2=integer(length=length(cnames))
    fmt=character(length=length(cnames))
    pos1[1] = 1
    tmp = regexpr(cnames[1],lines[1],fixed=T)
    pos2[1] = tmp[[1]]+attr(tmp,'match.length') - 1
    for(i in 2:(length(cnames)-1)){
        tmp = regexpr(sprintf(' %s ',cnames[i]),lines[1],fixed=T)
        pos2[i] = tmp[[1]]+attr(tmp,'match.length')-1
        pos1[i]=pos2[i-1]+1
    }
    pos1[length(cnames)] = pos2[length(cnames)-1]+1
    tmp = regexpr(cnames[length(cnames)],lines[1],fixed=T)
    pos2[length(cnames)] = tmp[[1]]+attr(tmp,'match.length') - 1
    widths = pos2-pos1+1
    tmp = read.fwf(textConnection(lines[2:length(lines)]),widths=widths,
        colClasses='character',comment.char='!',blank.lines.skip=TRUE,
        header=FALSE)
    for(i in 1:length(cnames)){
        if(cnames[i]%in%c('YEAR','DOY','DAS','DAP')){
            fmt[i] = paste('%',widths[i],'i',sep='')
        }else if(any(regexpr('[A-z]',tmp[,i])>0)){
            fmt[i] = paste('%',widths[i],'s',sep='')
        }else{
            if(any(grepl('\\.',tmp[,i]))){
                dec = round(mean(nchar(gsub('  *.*\\.','',tmp[,i]))),0)
            }else{
                dec = 0
            }
            fmt[i] = paste('%',widths[i],'.',dec,'f',sep='')
        }
    }
    fmt.list = vector(length=length(levels(as.factor(fmt))),mode='list')
    names(fmt.list) = levels(as.factor(fmt))
    for(i in 1:length(fmt.list)){
        fmt.list[[i]] = cnames[fmt==names(fmt.list)[i]]
    }
    return(fmt.list)
}
