read.tier <- function(header,l1,nrows,file.name,fmt.list=NULL){
    header = gsub('@',' ',header)
    cnames = strsplit(gsub('@','',header),split='  *')[[1]]
    cnames = cnames[cnames!='']
    pos = vector(length=length(cnames),mode='list')
    fmt = vector(length=length(cnames),mode='character')
    if(is.null(fmt.list)) fmt.list = fmt.default()
    for(i in 1:length(fmt.list)){
        fmt[cnames%in%fmt.list[[i]]]=names(fmt.list)[i]
    }
    class = fmt2class(fmt)
    if(all(cnames%in%unlist(fmt.list))){
        widths = fmt2width(fmt)
    }else{
        widths = vector(length=length(cnames),mode='numeric')
        for(i in 1:length(cnames)){
            tmp = str.index(header,cnames[i])
            widths[i]=tmp$stop-tmp$start+1
        }
    }
    vars = read.fwf(file.name,widths=widths,skip=l1,nrow=nrows,
        colClasses=class,comment.char='!',blank.lines.skip=TRUE,
        na.strings=c('-99','-99.','-99.0'))
    colnames(vars)=cnames
    return(vars)
}
