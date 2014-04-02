numeric.all <- function(vect){
# code based on all.is.numeric() from package Hmisc version 3.9-0
    old = options(warn = -1)
    on.exit(options(old))
    vect = gsub('^ *','',gsub(' *$','',vect))
    is.num = !any(is.na(as.numeric(vect[!vect%in%c('')])))
    return(is.num)
}

