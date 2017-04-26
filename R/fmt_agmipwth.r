fmt.agmip.wth <- function(){
    fmt = list(
        `%7.0f`=c('WELEV','FLAG','REFHT','WNDHT',),
        `%6.1f`=c('TAV','AMP','SRAD','TMAX','TMIN','RAIN','WIND','DEWP','VPRS'),
        `%6i`=c('TRNO','WEYR','WEDAY','YYYY'),
        `%4i`=c('MM','DD'),
        `%6s`=c('INSI'),
        `%9.3f`=c('WTHLAT'),
        `%10.3f`=c('WTHLONG'),
        `%8yyyymmdd`=c('DATE')
        )
    return(fmt)
}
