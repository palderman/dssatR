df2nc <- function(cul,eco,spe,file.name){
    require(ncdf4)

    cul$`VAR-NAME` <- gsub(' ','',as.character(cul$`VAR-NAME`))
    cul$`VAR#` <- gsub(' ','',as.character(cul$`VAR#`))
    eco$`ECO#` <- gsub(' ','',as.character(eco$`ECO#`))
    
    culdim <- ncdim_def('cul','count',1:nrow(cul))
    varnamedim <- ncdim_def('varnamelen',
                            '',
                            1:max(nchar(cul$`VAR-NAME`)))
    econumdim <- ncdim_def('econumlen',
                        '',
                        1:6)
    ecodim <- ncdim_def('eco','count',1:nrow(eco))

    l3 <- ncdim_def('l3','count',1:3)
    l4 <- ncdim_def('l4','count',1:4)
    l6 <- ncdim_def('l6','count',1:6)
    l8 <- ncdim_def('l8','count',1:8)
    l10 <- ncdim_def('l10','count',1:10)

    v.list <- list()

    for(v in factor(cul$`VAR#`,levels=cul$`VAR#`)){
        v.list[[paste0('CUL',v)]] <- ncvar_def(paste0('CUL',v),
                                 '',
                                 list(),
                                 missval=-99,
                                 prec='integer')
    }

    for (p in names(cul)){
        if(!p%in%c('VAR#','EXP#')){
            if(p=='VAR-NAME'){
                v.list[[p]] <- ncvar_def(p,
                                         '',
                                         list(varnamedim,culdim),
                                         missval=' ',
                                         prec='char')
            }else if(p=='ECO#'){
                v.list[[p]] <- ncvar_def(p,
                                         '',
                                         list(econumdim,culdim),
                                         missval=' ',
                                         prec='char')
            }else{
                v.list[[p]] <- ncvar_def(p,
                                         '',
                                         culdim,
                                         missval=-99,
                                         prec='float')
            }
        }
    }
    
    for(v in factor(eco$`ECO#`,levels=eco$`ECO#`)){
        v.list[[paste0('ECO',v)]] <- ncvar_def(paste0('ECO',v),
                                 '',
                                 list(),
                                 missval=-99,
                                 prec='integer')
    }

    for (p in names(eco)){
        if(!p%in%c('ECO#')){
                v.list[[p]] <- ncvar_def(p,
                                         '',
                                         ecodim,
                                         missval=-99,
                                         prec='float')
        }
    }

    for(i in 1:length(spe)){
        for(v in colnames(spe[[i]])){
            if(nrow(spe[[i]])==1) the.dim = list()
            if(nrow(spe[[i]])==3) the.dim = l3
            if(nrow(spe[[i]])==4) the.dim = l4
            if(nrow(spe[[i]])==6) the.dim = l6
            if(nrow(spe[[i]])==8) the.dim = l8
            if(nrow(spe[[i]])==10) the.dim = l10
            v.list[[v]] <- ncvar_def(v,
                                     '',
                                     the.dim,
                                     missval=-99,
                                     prec='float')
        }
    }


    ncid <- nc_create(file.name,v.list)

    for (v in 1:nrow(cul)){
        ncvar_put(ncid,paste0('CUL',cul$`VAR#`[v]),v)
    }
    for (p in names(cul)){
        if(!p%in%c('VAR#','EXP#')){
            ncvar_put(ncid,p,cul[[p]])
        }
    }
    for (v in 1:nrow(eco)){
        ncvar_put(ncid,paste0('ECO',eco$`ECO#`[v]),v)
    }
    for (p in names(eco)){
        if(!p%in%c('ECO#')){
            ncvar_put(ncid,p,eco[[p]])
        }
    }

    for(i in 1:length(spe)){
        for(v in colnames(spe[[i]])){
            ncvar_put(ncid,v,spe[[i]][[v]])
        }
    }

    nc_close(ncid)
}
