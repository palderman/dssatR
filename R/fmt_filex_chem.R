fmt.filex.chem <- function(){
    fmt = list(
        `%2i` = c('C'),
        `%6.5i` = c('CDATE'),
        `%6s` = c('CHCOD','CHME'),
        `%6.0f` = c('CHDEP'),
        `%6.2f` = c('CHAMT'),
        ` %s` = c('CHTCHNAME')
        )
    return(fmt)
}
