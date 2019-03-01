write.sol.netcdf <- function(sol,file.name){

    require(ncdf4)
    require(dplyr)

    # Create NetCDF dimension variables for latitude, longitude and date
    lat.dim <- ncdim_def('latitude','degrees',sol$LAT)
    long.dim <- ncdim_def('longitude','degrees',sol$LONG)
    layer.dim <- ncdim_def('layer','count',1:nrow(sol$layer))

    layers <- sol$layer

    for(i in 1:ncol(layers)){
        if(is.character(layers[,i])|is.factor(layers[,i])){
            layers[,i] <- as.character(layers[,i]) %>%
                gsub('(^  *)|(  *$)','',.)
        }
    }

    sol <- sol[-which(names(sol)=='layer')]

    # Determine lengths for all character parameters
    sol.clen <- sapply(c(sol,as.list(layers)),function(x){
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

    sol.clen[sol.clen==Inf|sol.clen==-Inf] <- 1

    all.len <- unique(c(1,sol.clen))

    # Create NetCDF dimension variables for each length determined above
    len.dims <- list()
    for(l in all.len){
        l.name <- paste0('l',l)
        len.dims[[l.name]] <- ncdim_def(l.name,'count',1:l)
    }

#    wth <- lapply(wth[c('station.info','data')],as.list) %>%
#        do.call(c,.) %>%
#        {names(.) <- gsub('(station\\.info\\.)|(data\\.)','',names(.));.}

    v.list <- list()

    # Create variables for other station information variables
    for(p in names(sol)){
        if(!p %in% c('LAT','LONG')){
            the.dim <- len.dims[[which(all.len==length(sol[[p]]))]]
            if(is.character(sol[[p]]) |
               is.factor(sol[[p]])){
                clen <- as.character(sol[[p]]) %>%
                    gsub('(^ *)|( *$)','',.) %>%
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
    }

    # Create variables for other station information variables
    for(p in names(layers)){
        if(is.character(layers[[p]]) |
           is.factor(layers[[p]])){
            clen <- as.character(layers[[p]]) %>%
                gsub('(^ *)|( *$)','',.) %>%
                nchar(.) %>%
                max(.,na.rm=TRUE)
            v.list[[p]] <- ncvar_def(p,
                                     '',
                                     list(len.dims[[which(all.len==clen)]],layer.dim,long.dim,lat.dim),
                                     missval=' ',
                                     prec='char')
        }else{
            v.list[[p]] <- ncvar_def(p,
                                     '',
                                     list(layer.dim,long.dim,lat.dim),
                                     missval=-99,
                                     prec='float')
        }
    }

    # Create new NetCDF file
    ncid <- nc_create(file.name,v.list)

    # Add all other parameter values to NetCDF file
    for(p in names(sol)){
        if(is.factor(sol[[p]])) sol[[p]] <- as.character(sol[[p]])
        if(!p %in% c('LAT','LONG')){
            ncvar_put(ncid,p,sol[[p]])
        }
    }
    for(p in names(layers)){
        ncvar_put(ncid,p,layers[[p]])
    }

    # Write data and close NetCDF file
    nc_close(ncid)
}
