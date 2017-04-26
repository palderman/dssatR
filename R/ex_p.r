ex.p <- function(gen,p,vrec=NULL){
    out=NULL
    for(i in 1:length(gen$parameters)){
        if(p%in%colnames(gen$parameters[[i]])){
            if(!is.null(vrec)){
                out=gen$parameters[[i]][gen$parameters[[i]][,1]==vrec,p]
            }else{
                out=gen$parameters[[i]][,p]
            }
        }
    }
    return(out)
}
