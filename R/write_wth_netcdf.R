write.wth.netcdf <- function(wth,file.name){

    require(ncdf4)
    require(dplyr)

    # Create NetCDF dimension variables for latitude, longitude and date
    lat.dim <- ncdim_def('latitude','degrees',wth$station.info$LAT)
    long.dim <- ncdim_def('longitude','degrees',wth$station.info$LONG)
    if('POSIXt'%in%class(wth$data$DATE)){
        date.dim <- ncdim_def('DATE','YEARDOY',as.integer(format(wth$data$DATE,'%Y%j')))
    }else{
        date.dim <- ncdim_def('DATE','YEARDOY',as.integer(wth$data$DATE))
    }

    # Determine lengths for all character parameters
    wth.clen <- sapply(wth$station.info,function(x){
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
    
    all.len <- unique(c(1,wth.clen))

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
    for(p in names(wth$station.info)){
        if(!p %in% c('LAT','LONG')){
            the.dim <- len.dims[[which(all.len==length(wth$station.info[[p]]))]]
            if(is.character(wth$station.info[[p]]) |
               is.factor(wth$station.info[[p]])){
                clen <- as.character(wth$station.info[[p]]) %>%
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
    for(p in names(wth$data)){
        if(p != 'DATE'){
            v.list[[p]] <- ncvar_def(p,
                                     '',
                                     list(long.dim,lat.dim,date.dim),
                                     missval=-99,
                                     prec='float')
        }
    }

    # Create new NetCDF file
    ncid <- nc_create(file.name,v.list)

    # Add all other parameter values to NetCDF file
    for(p in names(wth$station.info)){
        if(!p %in% c('LAT','LONG')){
            ncvar_put(ncid,p,wth$station.info[[p]])
        }
    }
    for(p in names(wth$data)){
        if(p != 'DATE'){
            ncvar_put(ncid,p,wth$data[[p]])
        }
    }

    # Write data and close NetCDF file
    nc_close(ncid)
}
