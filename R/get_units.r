get.units <- function(variable){
    switch(sub('#','num',sub('%','pct',variable)),
            LWAD={units = 'kg ha^-1'},
            SWAD={units = 'kg ha^-1'},
            CWAD={units = 'kg ha^-1'},
            RWAD={units = 'kg ha^-1'},
            PWAD={units = 'kg ha^-1'},
            GWAD={units = 'kg ha^-1'},
            LNpctD={units = '%'},
            SNpctD={units = '%'},
            SHND={units = '%'},
            GNpctD={units = '%'},
            SHpctD={units = '%'},
            HIPD={units = ''},
            HIAD={units = ''},
            SLAD={units = 'cm^2 g^-1'},
            LAID={units = ''},#'m^2 m^-2'},
            LnumSD={units = 'nodes plant^-1'}
    )
    return(units)
}

