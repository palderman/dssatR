read.filet <- function(filename){
    fmt.list = fmt.default()
    out = read.dssat(filename,fmt.list=fmt.list)
    out = out[,!colnames(out)%in%c('RUN')]
    return(out)
}
