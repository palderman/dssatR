write.tier <- function(tier,file.name,fmt.list=NULL){
    cnames = colnames(tier)
    fmt = vector(length=length(cnames),mode='character')
    if(is.null(fmt.list)) fmt.list = fmt.default()
    for(i in 1:length(fmt.list)){
        fmt[cnames%in%fmt.list[[i]]]=names(fmt.list)[i]
    }
    class = fmt2class(fmt)
    for(c in 1:ncol(tier)){
        tier[,c] = sprintf(fmt[c],tier[,c])
    }
    tier = do.call(paste,c(tier,sep=''))
    write(tier,file=file.name,append=TRUE)
    return(invisible(tier))
}
