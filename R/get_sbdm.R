get.sbdm <- function(som,minBD){
    sbdm = 100/(som/0.224+(100-som)/minBD)
    return(sbdm)
}

