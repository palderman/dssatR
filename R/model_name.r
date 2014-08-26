model.name <- function(string){
    if(grepl('GRO',string)){
        model='CROPGRO'
    }else if(grepl('CRP',string)){
        model = 'CROPSIM'
        fmt.list = cropsim.fmt()
    }else if(grepl('CER',string)){
        model = 'CERES'
        fmt.list = ceres.fmt()
    }else if(grepl('APS',string)){
        model = 'NWHEAT'
        fmt.list = nwheat.fmt()
    }else{
        stop('Please provide model name.')
    }
    return(tolower(model))
}
