fill.missing.vals <- function(data,fill.from){
    missing = data[!complete.cases(data),]
    if(nrow(missing)>1){
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
    data[!complete.cases(data),] = missing
    return(data)
}
