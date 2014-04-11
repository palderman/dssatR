parse.header <- function(header){
    names = unlist(strsplit(header,'  *'))
    names = names[names!='']
    if(any(nchar(names)>6)){
        names=vector(length=ceiling(nchar(header)/6),mode='character')
        for(i in 1:length(names)){
            names[i]=substr(header,((i-1)*6+1),i*6)
        }
        names=gsub('  *','',names)
    }
    return(names)
}

