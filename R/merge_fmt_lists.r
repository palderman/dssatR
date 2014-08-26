merge.fmt.lists <- function(...){
    fmt.list = list(...)
    while(length(fmt.list)>1){
        if(any(unlist(fmt.list[[2]])%in%unlist(fmt.list[[1]]))){
            for(i in 1:length(fmt.list[[2]])){
                fmt.list[[2]][[i]] =
                    fmt.list[[2]][[i]][
                                !fmt.list[[2]][[i]]%in%unlist(fmt.list[[1]])]
            }
            fmt.list[[2]] = fmt.list[[2]][unlist(lapply(fmt.list[[2]],length))>0]
        }
        if(length(fmt.list[[2]])>0){
            new.names = levels(as.factor(
                c(names(fmt.list[[1]]),names(fmt.list[[2]]))))
            fmt.list.new = vector(length=length(new.names),mode='list')
            names(fmt.list.new) = new.names
            for(i in 1:length(fmt.list.new)){
                fmt.list.new[[i]] =
                    c(fmt.list[[1]][[new.names[i]]],
                      fmt.list[[2]][[new.names[i]]])
            }
            fmt.list[[1]]=fmt.list.new
        }
        fmt.list = fmt.list[-2]
    }
    return(fmt.list[[1]])
}
