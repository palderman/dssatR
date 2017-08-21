read.filex.netcdf <- function(file.name){
  suppressMessages(require(ncdf4))
  suppressMessages(require(dplyr))
  nc <- nc_open(file.name)
  dims <- names(nc$dim) %>% {.[!grepl('len',.)]}
  var.names <- names(nc$var)
  tiers <- lapply(dims,function(dim,nc,vnames){
      var.list=sapply(nc$var,function(y,dim){any(grepl(dim,y$dim))},
                  dim=dim) %>%
                  {vnames[.]}
      tier=lapply(var.list,ncvar_get,nc=nc) %>%
        do.call(data.frame,.)
      colnames(tier) <- var.list
      return(tier)
  },nc=nc,vnames=var.names)
  names(tiers) <- dims
  return(tiers)
}