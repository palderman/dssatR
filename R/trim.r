trim <- function(string){
    string = gsub('(^ +)|( +$)','',string)
    return(string)
}

