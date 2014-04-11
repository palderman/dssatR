get.soil.profile <- function(soil_id,filename){
    soil = readLines(filename)
    soil = soil[grep(soil_id,soil):length(soil)]
    soil = soil[!substr(soil,1,1)=='!']
    soil = soil[!soil==''&!grepl('^[[:space:]]$',soil)]
    nextprof = grep('^\\*',soil[2:length(soil)])
    if(length(nextprof)>0) soil = soil[1:(nextprof[1]-1)]
    soil.profile = create.soil.profile(empty=T)
    soil.profile$ID_SOIL = trim(substr(soil[1],2,11))
    soil.profile$SOURCE = trim(substr(soil[1],14,25))
    soil.profile$TEXTURE_CLASS = trim(substr(soil[1],26,28))
    soil.profile$DEPTH = as.integer(trim(substr(soil[1],34,36)))
    soil.profile$SOIL_NAME = trim(substr(soil[1],38,nchar(soil[1])))
    soil.profile$SITE = trim(substr(soil[3],1,12))
    soil.profile$COUNTRY = trim(substr(soil[3],14,27))
    soil.profile$LAT = as.numeric(trim(substr(soil[3],28,34)))
    soil.profile$LONG = as.numeric(trim(substr(soil[3],36,42)))
    soil.profile$SCS_FAMILY = trim(substr(soil[3],44,nchar(soil[3])))
    soil.profile$SCOM = trim(substr(soil[5],1,6))
    soil.profile$SALB = as.numeric(substr(soil[5],7,12))
    soil.profile$SLU1 = as.numeric(substr(soil[5],13,18))
    soil.profile$SLDR = as.numeric(substr(soil[5],19,24))
    soil.profile$SLRO = as.numeric(substr(soil[5],25,30))
    soil.profile$SLNF = as.numeric(substr(soil[5],31,36))
    soil.profile$SLPF = as.numeric(substr(soil[5],37,42))
    soil.profile$SMHB = trim(substr(soil[5],43,48))
    soil.profile$SMPX = trim(substr(soil[5],49,54))
    soil.profile$SMKE = trim(substr(soil[5],55,60))
    headlines = grep('@',soil[6:length(soil)])
    for (i in 1:length(headlines)){
        line1 = 5 + headlines[i]
        if (i < length(headlines)){
            line2 = 5 + headlines[i+1]-1
        }else{
            line2 = length(soil)
        }
        temp = read.fwf(
                textConnection(soil[(line1+1):line2]),
                header=F,
                widths=rep(6,nchar(soil[line1])/6))
        temp[temp==-99] = NA
        cnames = unlist(strsplit(
                                gsub('  *','!',
                                gsub('@',' ',soil[line1])),
                                split='!'))
        colnames(temp) = cnames[cnames!='']
        if(i>1){
            soil.profile$layer=merge(soil.profile$layer,temp,by='SLB',all=T)
        }else{
            soil.profile$layer=temp
        }
    }
    soil.profile$layer$SLMH = as.character(soil.profile$layer$SLMH)
    return(soil.profile)
}

