fmt2width <- function(fmt){
    width = gsub('%-*','',fmt)
    width = gsub('[a-z].*','',width)
    width = as.numeric(gsub('\\..*','',width))
    return(width)
}
