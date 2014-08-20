fmt2width <- function(fmt){
    width = gsub('%-*','',fmt)
    width = gsub('[a-z].*','',width)
    width = as.numeric(gsub('\\..*','',width))
    width = width + nchar(fmt) - nchar(gsub(' ','',fmt))
    return(width)
}
