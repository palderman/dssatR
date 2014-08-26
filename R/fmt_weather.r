fmt.weather =
function(){
return(
structure(list("%5i" = c("YEAR", "DOY"), "%6.0f" = "CO2D", "%6i" = "DAS", 
    "%7.2f" = c("PRED", "DYLD", "TWLD", "SRAD", "PARD", "TMXD", 
    "TMND", "TAVD", "TDYD", "TDWD", "TGAD", "TGRD", "WDSD", "DAYLD"
    ), "%7.3f" = "CLDD"), .Names = c("%5i", "%6.0f", "%6i", "%7.2f", 
"%7.3f"))
)
}
