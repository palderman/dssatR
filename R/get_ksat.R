get.ksat <- function(sand,silt,clay){
    ksat = vector(length(sand),mode='numeric')
    for(i in 1:length(ksat)){
        if(sand[i] >= 85 & silt[i]+1.5*clay[i] <= 15){
            ksat[i] = 21
        }else if((sand[i] >= 85 & sand[i] < 90 & silt[i]+1.5*clay[i] >= 15) |
                (sand[i] >= 70 & sand[i] < 85 & silt[i]+2*clay[i] <=30)){
            ksat[i] = 6.11
        }else if((clay[i] <= 20 & sand[i] >= 52 & silt[i]+2*clay[i] > 30) |
                (clay[i] < 7 & silt[i] < 50 & sand[i] > 43 & sand[i] < 52)){
            ksat[i] = 2.59
        }else if(clay[i] >= 7 & clay[i] <= 27 & silt[i] >= 28 & silt[i] < 50 &
                sand[i] >= 23 & sand[i] < 52){
            ksat[i] = 1.32
        }else if((silt[i] >= 50 & silt[i] <= 88 & clay[i] >= 12 & clay[i] <= 27) |
                (silt[i] >= 50 & clay[i] >= 0 & clay[i] < 12)){
            ksat[i] = 0.68
        }else if(clay[i] >= 20 & clay[i] < 35 & silt[i] >= 0 & silt[i] < 28 & sand[i] >= 45){
            ksat[i] = 0.43
        }else if(clay[i] >= 27 & clay[i] < 40 & sand[i] >= 20 & sand[i] < 45){
            ksat[i] = 0.23
        }else if(clay[i] >= 27 & clay[i] < 40 & sand[i] >= 0 & sand[i] < 20){
            ksat[i] = 0.15
        }else if(clay[i] >= 35 & sand[i] >= 45){
            ksat[i] = 0.12
        }else if(clay[i] >= 40 & silt[i] >= 40){
            ksat[i] = 0.09
        }else if(clay[i] >= 40 & sand[i] < 45 & silt[i] < 40){
            ksat[i] = 0.06
        }
    }
    return(ksat)
}

