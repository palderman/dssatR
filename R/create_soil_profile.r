create.soil.profile <- function(empty=F){
    if (exists('soil.profile.backup')){
        soil.profile = get('soil.profile.backup')
    }else{
        soil.profile = list(ID_SOIL = NULL,
                        SOURCE = NULL,
                        TEXTURE_CLASS = NULL,
                        DEPTH = NULL,
                        SOIL_NAME = NULL,
                        SITE = NULL,
                        COUNTRY = NULL,
                        LAT = NULL,
                        LONG = NULL,
                        SCS_FAMILY = NULL,
                        SCOM = NULL,
                        SALB = NULL,
                        SLU1 = NULL,
                        SLDR = NULL,
                        SLRO = NULL,
                        SLNF = NULL,
                        SLPF = NULL,
                        SMHB = NULL,
                        SMPX = NULL,
                        SMKE = NULL,
                        layer = cbind(
                                    SLB = rep(NA,10), SLMH = rep(NA,10), 
                                    SLLL = rep(NA,10), SDUL = rep(NA,10),
                                    SSAT = rep(NA,10), SRGF = rep(NA,10), 
                                    SSKS = rep(NA,10), SBDM = rep(NA,10), 
                                    SLOC = rep(NA,10), SLCL = rep(NA,10), 
                                    SLSI = rep(NA,10), SLCF = rep(NA,10), 
                                    SLNI = rep(NA,10), SLHW = rep(NA,10), 
                                    SLHB = rep(NA,10), SCEC = rep(NA,10), 
                                    SADC = rep(NA,10))
                        )
    }
    if(!empty){
        variable.names = names(soil.profile)
        i = 1; entry = ''
        while(i < length(variable.names)){
            if(is.null(soil.profile[[i]])|tolower(entry)=='b'){
                cat(paste('Enter ',variable.names[i],' below:',sep=''),'\n')
                entry = readline('(Press [b] to go back)\n')
                if(tolower(entry) == 'b'){
                    i = i -1
                }else{
                    soil.profile[[i]] = entry
                }
                assign('soil.profile.backup',soil.profile,envir=globalenv())
            }
            if (tolower(entry) != 'b') i = i + 1
        }
        nlayer = eval(parse(text=readline('Enter number of soil layers:\n')))
        if(nlayer <= 10){
            soil.profile$layer = soil.profile$layer[1:nlayer,]
        }else{
            soil.profile$layer = rbind(soil.profile$layer,
                                        soil.profile$layer[1:(nlayer-10),])
        }
        i = 1; j = 1; entry = ''
        while(i <= nlayer){
            while(j <= ncol(soil.profile$layer)){
                if(is.na(soil.profile$layer[i,j])|tolower(entry)=='b'){
                    cat(paste('Enter ',colnames(soil.profile$layer),' for layer',i,' below:',sep=''),'\n')
                    entry = readline('(Press [b] to go back)\n')
                    if ( tolower(entry) != 'b'){
                        if(grepl('^[[:alpha:]]$',entry)){
                            soil.profile$layer[i,j] = entry
                        }else{
                            soil.profile$layer[i,j] = eval(parse(text=entry))
                        }
                    }else{
                        if(j > 1){
                            j = j - 1
                        }else{
                            i = i - 1
                            j = ncol(soil.profile$layer)
                        }
                    }
                }
                if(tolower(entry) != 'b') j = j + 1
            }
            if(tolower(entry) != 'b') i = i + 1
        }
    }
    if(exists('soil.profile.backup')) rm('soil.profile.backup',envir=globalenv())
    return(soil.profile)
}

