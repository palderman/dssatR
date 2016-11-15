fmt.filex.plt <- function(){
    fmt = list(
        `%2i`=c('P'),
        `%6s`=c('PLME','PLDS'),
        `%6.0f`=c('PLRS','PLRD','PAGE','SPRL'),
        `%6.1f`=c('PPOP','PPOE','PLDP','PLWT','PENV','PLPH'),
        ` %-28s`=c('PLNAME'),
        `%6yrdoy`=c('PDATE','EDATE')
        )
    return(fmt)
}
