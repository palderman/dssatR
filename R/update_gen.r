update.gen <- function(gen,new.vals,genno){
    for(i in 1:length(gen$parameters)){
        genno.col = grep('ECO#',colnames(gen$parameters[[i]]))
        if(length(genno.col)<1){
            genno.col = grep('VAR#',colnames(gen$parameters[[i]]))
        }
        ind = gen$parameters[[i]][genno.col,]==genno
        if(is.data.frame(new.vals)){
            sub = colnames(new.vals)[
                colnames(new.vals)%in%colnames(gen$parameters[[i]])]
            gen$parameters[[i]][ind,sub]=new.vals[,sub]
        }else{
            sub = names(new.vals)[
                names(new.vals)%in%colnames(gen$parameters[[i]])]
            gen$parameters[[i]][ind,sub]=new.vals[sub]
        }
    }
    return(gen)
}
