fmt.default <- function(){
    fmt = list(
        `%6.0f`=c('HWAM','CWAM','T#AM','H#AM','CWAD','GWAD','LWAD','SWAD','GSTD'),
        `%6.1f`=c('H#UM'),
        `%6.2f`=c('HWUM'),
        `%6.3f`=c('HIAM'),
        `%6.5i`=c('ADAT','MDAT','IEDAT','DATE'),
        `%6i`=c('TRNO')
        )
    return(fmt)
}
