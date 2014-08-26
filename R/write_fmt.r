write.fmt <- function(fmt.list,file){
    fmt.name = gsub('_','.',gsub('\\..*','',basename(file)))
    fmt.deparsed = deparse(fmt.list)
    fmt.deparsed =
        c(paste(fmt.name,'='),'function(){','return(',fmt.deparsed,')','}')
    write(fmt.deparsed,file=file)
}
