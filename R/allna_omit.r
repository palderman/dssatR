allna.omit <- function(data){
data = data[rowSums(is.na(data))!=ncol(data),]
data=data[,unlist(lapply(data,function(x){all(is.na(x))}))]
return(data)
}
