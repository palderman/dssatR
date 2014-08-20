fmt2class <- function(fmt){
    class = gsub('.*i.*','integer',fmt)
    class = gsub('.*s.*','character',class)
    class = gsub('.*f.*','numeric',class)
    class = gsub('.*doy.*','character',class)
    return(class)
}
