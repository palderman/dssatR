fmt.filex.irr <- function(){
    fmt = list(
        `%2i` = c('I'),
        `%6.5i` = c('IDATE'),
        `%6s` = c('IROP'),
        `%6.0f` = c('EFIR','IDEP','ITHR','IEPT','IOFF','IAME','IAMT','IRVAL'),
        ` %s` = c('IRNAME')
        )
    return(fmt)
}
