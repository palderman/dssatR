str.index <- function(full,sub){
    tmp = gregexpr(pattern=sub,full)
    pos2 = tmp[[1]][1]+attr(tmp[[1]],'match.length')-1
    tmp  = gregexpr(pattern='  *',substr(full,1,pos2))
    pos1 = tmp[[1]]
    pos1 = pos1[length(pos1)]
    return(list(start=pos1,stop=pos2))
}
