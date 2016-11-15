fmt.filex.fld <- function(){
    fmt = list(
        `%2i`=c('L'),
        ` %4s`=c('SLTX'),
        `%6.0f`=c('FLOB','FLSA','FLDD','FLDS','FLST','SLDP'),
        `%6s`=c('FLDT'),
        `%6.1f`=c('SLEN','FLWR','SLAS'),
        ` %-8s`=c('ID_FIELD','WSTA'),
        ` %9.0f`=c('ELEV'),
        `  %-10s`=c('ID_SOIL'),
        ` %15.5f`=c('XCRD','YCRD'),
        ` %17.0f`=c('AREA'),
        ` %-25s`=c('FLNAME'))
    return(fmt)
}
