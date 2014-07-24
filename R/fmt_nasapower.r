fmt.nasapower <- function(){
    fmt = list(
        `%7.0f`=c('WELEV','FLAG','REFHT','WNDHT'),
        `%7.1f`=c('TMAX','TMIN','RAIN','WIND','TDEW','T2M','RH2M'),
        `%6.1f`=c('TAV','AMP','SRAD'),
        `%6i`=c('TRNO','WEYR','WEDAY'),
        `%6s`=c('INSI'),
        `%9.3f`=c('WTHLAT')
        `%10.3f`=c('WTHLONG')
        )
    return(fmt)
}
