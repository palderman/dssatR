fill.missing.vals <- function(data,fill.from){
    ind = Reduce('|',lapply(data,is.na))
    missing = data[ind,]
    if(nrow(missing)>0){
        for(i in 1:nrow(missing)){
            for(j in 1:ncol(missing)){
                if(is.na(missing[i,j])){
                    missing[i,j]=
                        fill.from[fill.from$DATE==missing[i,'DATE'],
                                  colnames(missing)[j]]
                }
            }
        }
    }
    data[ind,] = missing
    return(data)
}
