fmt.filex.env <- function(){
    fmt = list(
        `%2i`=c('E'),
        `%7yrdoy`=c('ODATE'),
        ` %-6s`=c('ECO2','EDAY','EDEW','EMAX','EMIN','ERAD','ERAIN','EWIND'),
        ` %-25s`=c('ENVNAME'))
    return(fmt)
}
