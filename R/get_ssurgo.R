get.ssurgo.data <- function(lat,lon,variables=''){
    require(soilDB)
    if(identical(variables,'')) variables <- c('compname','comppct_r')
    query<- paste("SELECT ",paste(variables,collapse=','),
                  "FROM mapunit",
                  "INNER JOIN component ON mapunit.mukey = component.mukey",
                  "INNER JOIN chorizon ON chorizon.cokey = component.cokey",
                  "WHERE mapunit.mukey IN",
                             paste("(SELECT * FROM",
                                   "SDA_Get_Mukey_from_intersection_with_WktWgs84('point(",
                                   lon,lat,")'))"),
                  "ORDER BY comppct_r DESC,compname,hzdept_r ASC"
                  )
    soil <- SDA_query(query)
    return(soil)
}
