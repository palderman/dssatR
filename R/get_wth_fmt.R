get.wth.fmt <- function(name){
    fmt = unlist(lapply(name,function(x){
               return(switch(x,
                    INSI='%6s',
                    LAT='%9.3f',
                    LONG='%9.3f',
                    ELEV='%6.0f',
                    FLAG='%6.0f',
                    DATE='%5.5i',
                    '%6.1f'
                ))}))
    return(fmt)
}

