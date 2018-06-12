fmt.filex.irr <- function(){
    fmt = list(
        `%2i` = c('I'),
        `%6.5i` = c('IDATE'),
        `%6s` = c('IROP','IOFF'),
        `%6.0f` = c('IDEP','ITHR','IEPT','IAME','IAMT','IRVAL'),
        `%6.2f` = c('EFIR'),
        ` %s` = c('IRNAME')
        )
    return(fmt)
}
