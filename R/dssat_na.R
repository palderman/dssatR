dssat.na.strings <- function(){
na.strings=c('-99','-99.','-99.0','-99.00','-99.000',substring('********',1,1:8))
return(na.strings)
}
