get.name.widths <- function(header){
    names = parse.header(header)
    widths = vector('integer',length=length(names))
    for(i in 1:length(names)){
        names[i] = gsub('\\#','\\\\#',gsub('\\+','\\\\+',names[i]))
        x = regexpr(paste('  *',names[i],sep=''),header)
        widths[i] = attr(x,'match.length')
    }
    return(widths)
}

