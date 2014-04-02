get.trno <- function(run){
    if (exists('output')){
        output=get('output')
    }else{
        stop('Model outputs have not been loaded.')
    }
    trno = as.integer(as.character(levels(as.factor(output[output$RUN%in%run,]$TRNO))))
    return(trno)
}

