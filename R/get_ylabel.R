get.ylabel <- function(variable){
    switch(sub('#','num',sub('%','pct',variable)),
            LWAD={ylab = 'Leaf Mass (kg ha^-1)'},
            SWAD={ylab = 'Stem Mass (kg ha^-1)'},
            CWAD={ylab = 'Shoot Mass (kg ha^-1)'},
            RWAD={ylab = 'Root Mass (kg ha^-1)'},
            PWAD={ylab = 'Pod Mass (kg ha^-1)'},
            GWAD={ylab = 'Grain Mass (kg ha^-1)'},
            QWAD={ylab = 'Rhizome Mass (kg ha^-1)'},
            LNpctD={ylab = 'Leaf N Concentration (%)'},
            SNpctD={ylab = 'Stem N Concentration (%)'},
            SHND={ylab = 'Shell N Concentration (%)'},
            GNpctD={ylab = 'Grain N Concentration (%)'},
            QNpctD={ylab = 'Rhizome N (%)'},
            QCpctD={ylab = 'Rhizome TNC (%)'},
            PHAN={ylab = c('Canopy Photosynthesis','(mg CO[2] m^-2 s^-1)')},
            SHpctD={ylab = 'Shelling Percentage (%)'},
            HIPD={ylab = 'Pod Harvest Index'},
            HIAD={ylab = 'Grain Harvest Index'},
            SLAD={ylab = 'Specific Leaf Area (cm^2 g^-1)'},
            LAID={ylab = 'Leaf Area Index'},
            LnumSD={ylab = 'Main Stem Node Number (nodes plant^-1)'}
    )
    return(ylab)
}

