create_ssurgo_profile <- function(latitude,longitude,id_soil="SSURGO000001",site='',use_ptf=FALSE){
  require(dplyr)
  require(soilDB)

  ssurgo_data <- get.ssurgo.data(lat=latitude,lon=longitude,
                            variables=c('compname AS SOIL_NAME',
                                        'comppct_r',
                                        'hzdepb_r AS SLB',
                                        'albedodry_r AS SALB',
                                        'drainagecl AS drainage',
                                        'hydgrp',
                                        'slope_r',
                                        'dbovendry_r AS SBDM',
                                        '(1-dbovendry_r/2.65)*0.95 AS SSAT',
                                        'wthirdbar_r/100. AS SDUL',
                                        'ksat_r AS SSKS',
                                        'wfifteenbar_r/100. AS SLLL',
                                        'claytotal_r AS SLCL',
                                        'silttotal_r AS SLSI',
                                        'om_r/1.724 AS SLOC'
                            )) %>%
    mutate(SLB = as.numeric(as.character(SLB))) %>%
    filter(.,comppct_r==max(comppct_r))
  
  sloc <- mean(ssurgo_data$SLOC,na.rm=TRUE)
  
  soil <- init.soil(nrow(ssurgo_data))
  
  soil$ID_SOIL <- id_soil
  soil$SOURCE <- 'SSURGO      '
  soil$TEXTURE_CLASS <- as.character(ssurgo_data$Soil.Texture[1]) %>%
    abbreviate(.,3) %>%
    toupper(.)
  soil$DEPTH <- ssurgo_data$SLB[nrow(ssurgo_data)]
  soil$SOIL_NAME <- unique(ssurgo_data$SOIL_NAME)
  soil$SITE <- substr(site,1,12)
  soil$COUNTRY <- 'USA'
  soil$LAT <- latitude
  soil$LONG <- longitude
  soil$SCS_FAMILY <- NA
  soil$SCOM <- 'BR'
  soil$SALB <- ssurgo_data$SALB[1]
  soil$SLU1 <- 5
  soil$SLDR <- get.sldr(ssurgo_data$drainage[1])
  soil$SLRO <- get.slro(ssurgo_data$hydgrp[1],ssurgo_data$slope_r[1])
  soil$SLNF <- 1
  soil$SLPF <- 1
  soil$SMHB <- 'IB001'
  soil$SMPX <- 'IB001'
  soil$SMKE <- 'IB001'
  
  soil$layer$SLB <- ssurgo_data$SLB
  soil$layer$SLLL <- ssurgo_data$SLLL
  soil$layer$SDUL <- ssurgo_data$SDUL
  soil$layer$SSKS <- ssurgo_data$SSKS
  #    if(any(is.na(soil$layer$SDUL))){
  #        l <- which(is.na(soil$layer$SDUL))
  #        soil$layer$SDUL[l] <- soil$layer$SDUL[l+1]
  #    }
  soil$layer$SSAT <- ssurgo_data$SSAT
  soil$layer$SBDM <- ssurgo_data$SBDM
  soil$layer$SLCL <- ssurgo_data$SLCL
  soil$layer$SLSI <- ssurgo_data$SLSI
  soil$layer$SLOC <- ifelse(is.nan(sloc),NA,sloc)
  soil$layer$SADC <- 0
  
  soil <- fill.in.profile(soil)

  return(soil)  
}