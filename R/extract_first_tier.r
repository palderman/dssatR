extract.first.tier <- function(filename){
    tmp = readLines(filename)
    hlines = grep('@',tmp)
    stars = grep('^\\*',tmp)
    if(length(hlines)>1){
        end = min(c(stars[stars>hlines[1]]-1,hlines[2]-1))
    }else{
        end = length(tmp)
    }
    tmp = tmp[hlines[1]:end]
    tmp = tmp[!substr(tmp,1,1)=='!']
    tmp = tmp[nchar(gsub('^  *','',gsub('  *','',tmp)))!=0]
    return(tmp)
}
