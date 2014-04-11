read.summary <- function(fileout,model='cropgro'){
    filelines = readLines(fileout)
    header = (1:length(filelines))[substr(filelines,1,1)=='@']
    pdata = read.table(fileout,header=F,skip=header,comment.char="!",na.strings='-99')
    cnames = unlist(strsplit(gsub('@','',filelines[header]),split='  *'))
    cnames = cnames[cnames!='']
    colnames(pdata) = cnames
    assign(substr(fileout,start=1,stop=nchar(fileout)-4),pdata,envir=parent.frame())
}

