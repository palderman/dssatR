extract.gen <- function(gen,number){
    gen$parameters=lapply(gen$parameters,
            function(x,number){
                if('ECO#'%in%colnames(x)){
                    return(x[x$`ECO#`==number,])
                }else if('VAR#'%in%colnames(x)){
                    return(x[x$`VAR#`==number,])
                }
            },
        number=number)
    return(gen)
}
