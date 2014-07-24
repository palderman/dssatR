fmt.default <- function(){
    fmt = list(
        `%6.0f`=c('HWAM','CWAM','T#AM','H#AM','CWAD','GWAD','LWAD','SWAD',
            'GSTD','WELEV','ELEV','FLAG','REFHT','WNDHT'),
        `%6.1f`=c('H#UM','TMAX','TMIN','RAIN','WIND','TDEW','T2M','RH2M',
            'TAV','AMP','SRAD','DEWP','VPRS','RHUM'),
        `%6.2f`=c('HWUM'),
        `%6.3f`=c('HIAM'),
        `%6.5i`=c('ADAT','MDAT','IEDAT','DATE'),
        `%6i`=c('TRNO','WEYR','WEDAY'),
        `%6s`=c('INSI'),
        `%9.3f`=c('LAT','WTHLAT','LONG','WTHLONG')
        )
    return(fmt)
}
