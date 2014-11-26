write.fileat <- function(fileat,filename,type=NA,title='',info='',fmt.list=NULL){
    if(is.na(type)) type=substr(filename,nchar(filename),nchar(filename))
    
    cnames = colnames(fileat)
    header = sprintf('%6s',cnames)
    width = nchar(gsub(' *','',cnames))+1
    width[width<6] = 6
    width[grepl('DAT$',cnames)] = 6

    fileat.lines = vector(length = 2+length(info),mode='character')
    fileat.lines[] = ''
    fileat.lines[1] = paste('*EXP. DATA (',type,'):',title)
    fileat.lines[3:(length(info)+2)] = paste('!',info)

    if(is.null(fmt.list)) fmt.list = fmt.default()

    write(fileat.lines,file=filename)

    write.tier(fileat,filename,fmt.list=fmt.list)
    
    return(invisible(fileat.lines))
}

