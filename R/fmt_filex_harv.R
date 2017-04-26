fmt.filex.harv <- function(){
    fmt = list(
        `%2i` = c('H'),
        `%6.5i` = c('HDATE'),
        `%6s` = c('HSTG','HCOM','HSIZE'),
        `%6.0f` = c('HPC','HBPC'),
        ` %s` = c('HNAME')
        )
    return(fmt)
}
