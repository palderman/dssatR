update.gen <- function(gen,new.vals){
    if(is.data.frame(new.vals)){
        for(i in 1:length(gen$parameters)){
            sub = colnames(new.vals)[
                colnames(new.vals)%in%colnames(gen$parameters[[i]])]
            gen$parameters[[i]][,sub]=new.vals[,sub]
        }
    }else{
        for(i in 1:length(gen$parameters)){
            sub = names(new.vals)[
                names(new.vals)%in%colnames(gen$parameters[[i]])]
            gen$parameters[[i]][,sub]=new.vals[sub]
        }
    }
    return(gen)
}
