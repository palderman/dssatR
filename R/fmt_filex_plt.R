fmt.filex.plt <- function(){
    fmt = list(
        `%2i`=c('P'),
        `%6s`=c('PLME','PLDS'),
        `%6.0f`=c('PLRS','PLRD','PAGE','SPRL','PLWT','PENV','PLPH'),
        `%6.1f`=c('PLDP','PPOP','PPOE'),
        ` %-28s`=c('PLNAME'),
        `%6yrdoy`=c('PDATE','EDATE')
        )
    return(fmt)
}
