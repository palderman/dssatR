read.filea <- function(filename){
    fmt.list = fmt.default()
#    title = readLines(filename,1)
#    title = gsub('  *$','',gsub('^  *','',gsub('^.*:','',title)))
    out = read.dssat(filename,fmt.list=fmt.list)
#    attr(out,'title') <- title
#    class(out) <- 'filea'
    return(out)
}
