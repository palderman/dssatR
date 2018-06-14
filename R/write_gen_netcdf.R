write.gen.netcdf <- function(cul,eco,spe,file.name){

    require(ncdf4)
    require(dplyr)

    # Subselect only parameter values from cultivar object
    if('parameters'%in%names(cul))
        cul <- cul$parameters[[1]]

    # Subselect only parameter values from ecotype object
    if('parameters'%in%names(eco))
        eco <- eco$parameters[[1]]

    # Subselect only parameter values from species object
    if('parameters'%in%names(spe))
        spe <- lapply(spe$parameters,as.list) %>%
            do.call(c,.)

    cul.names <- names(cul)
    eco.names <- names(eco)
    spe.names <- names(spe)

    if(any(cul.names%in%spe.names)){
        for(n in cul.names[cul.names%in%spe.names]){
            cul.names <- gsub(n,paste0(n,'_CUL'),cul.names)
        }
    }
    if(any(cul.names%in%eco.names)){
        for(n in cul.names[cul.names%in%eco.names]){
            cul.names <- gsub(n,paste0(n,'_CUL'),cul.names)
        }
    }
    if(any(eco.names%in%spe.names)){
        for(n in eco.names[eco.names%in%spe.names]){
            eco.names <- gsub(n,paste0(n,'_ECO'),eco.names)
        }
    }
    names(cul) <- cul.names
    names(eco) <- eco.names

    culdim <- ncdim_def('cul','count',1:nrow(cul))
    ecodim <- ncdim_def('eco','count',1:nrow(eco))

    gen <- c(spe,
             as.list(eco[,names(eco)!='ECO#']),
             as.list(cul[,names(cul)!='VAR#']))
    names(gen) <- gsub('(ECO#)|(ECO#_CUL)','ECONO',names(gen))

    # Determine lengths for all character parameters
    gen.clen <- sapply(gen,function(x){
                           if(is.character(x)|is.factor(x)){
                               clen <- as.character(x) %>%
                                   gsub('(^ *)|( *$)','',.) %>%
                                   nchar(.) %>%
                                   max(.,na.rm=TRUE)
                               return(clen)
                           }else{
                               return(NA)
                           }
    }) %>%
        unique(.) %>%
        na.omit(.)

    # Determine lengths of all parameter arrays
    gen.len <- sapply(gen,length) %>%
        unique(.) %>%
        na.omit(.)

    all.len <- unique(c(gen.clen,gen.len))

    # Create NetCDF dimension variables for each length determined above
    len.dims <- list()
    for(l in all.len){
        l.name <- paste0('l',l)
        len.dims[[l.name]] <- ncdim_def(l.name,'count',1:l)
    }

    v.list <- list()

    # Create variables for storing ecotype numbers
    for(v in gsub('(^ *)|( *$)','',eco$`ECO#`)){
        v.list[[paste0('ECO',v)]] <- ncvar_def(paste0('ECO',v),
                                 '',
                                 list(),
                                 missval=-99,
                                 prec='integer')
    }

    # Create variables for storing cultivar numbers
    for(v in gsub('(^ *)|( *$)','',cul$`VAR#`)){
        v.list[[paste0('CUL',v)]] <- ncvar_def(paste0('CUL',v),
                                 '',
                                 list(),
                                 missval=-99,
                                 prec='integer')
    }

    # Create variables for all other parameters
    for(p in names(gen)){
        the.dim <- len.dims[[which(all.len==length(gen[[p]]))]]
        if(is.character(gen[[p]])|is.factor(gen[[p]])){
            gen[[p]] <- as.character(gen[[p]]) %>%
                gsub('(^ *)|( *$)','',.)
            clen <- gen[[p]] %>%
                nchar(.) %>%
                max(.,na.rm=TRUE)
            v.list[[p]] <- ncvar_def(p,
                                     '',
                                     list(len.dims[[which(all.len==clen)]],the.dim),
                                     missval=' ',
                                     prec='char')
        }else{
            v.list[[p]] <- ncvar_def(p,
                                     '',
                                     the.dim,
                                     missval=-99,
                                     prec='float')
        }
    }

                                        # Create new NetCDF file
    ncid <- nc_create(file.name,v.list)


    # Add cultivar numbers to NetCDF file
    for (v in 1:nrow(cul)){
        ncvar_put(ncid,paste0('CUL',gsub('(^ *)|( *$)','',cul$`VAR#`[v])),v)
    }
    # Add ecotype numbers to NetCDF file
    for (v in 1:nrow(eco)){
        ncvar_put(ncid,paste0('ECO',gsub('(^ *)|( *$)','',eco$`ECO#`[v])),v)
    }
    # Add all other parameter values to NetCDF file
    for(p in names(gen)){
        ncvar_put(ncid,p,gen[[p]])
    }

    # Write data and close NetCDF file
    nc_close(ncid)
}
