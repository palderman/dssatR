init.soil <- function(nlayr){
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
                        layer = data.frame(
                                    SLB = rep(NA,nlayr), SLMH = rep(NA,nlayr), 
                                    SLLL = rep(NA,nlayr), SDUL = rep(NA,nlayr),
                                    SSAT = rep(NA,nlayr), SRGF = rep(NA,nlayr), 
                                    SSKS = rep(NA,nlayr), SBDM = rep(NA,nlayr), 
                                    SLOC = rep(NA,nlayr), SLCL = rep(NA,nlayr), 
                                    SLSI = rep(NA,nlayr), SLCF = rep(NA,nlayr), 
                                    SLNI = rep(NA,nlayr), SLHW = rep(NA,nlayr), 
                                    SLHB = rep(NA,nlayr), SCEC = rep(NA,nlayr), 
                                    SADC = rep(NA,nlayr))
                        )

}
