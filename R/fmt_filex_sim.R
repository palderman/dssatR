fmt.filex.sim = function(){
    fmt = list(
        `%2i` = c('N'),
        `%6.0f` = c('NYERS','NREPS','RSEED','PH2OL','PH2OU','PH2OD','PSTMX',
            'PSTMN','IMDEP','ITHRL','ITHRU','IRAMT','NMDEP','NMTHR',
            'NAMNT','RIPCN','RTIME','RIDEP','HPCNP','HPCNR'),
        `%6.2f` = c('IREFF'),
        `%6s` = c('START','WATER','NITRO','SYMBI','PHOSP','POTAS','DISES',
            'CHEM','TILL','CO2','USECT','WTHER','INCON','LIGHT','EVAPO',
            'INFIL','PHOTO','HYDRO','MESOM','MESEV','PLANT','IRRIG','FERTI',
            'RESID','HARVS','FNAME','OVVEW','SUMRY','FROPT','GROUT','CAOUT',
            'WAOUT','NIOUT','MIOUT','DIOUT','LONG','CHOUT','OPOUT','IROFF',
            'IMETH','NCODE','NAOFF','VBOSE'),
        `%6i` = c('NSWIT','MESOL','FROPT'),
        `%6yrdoy` = c('SDATE','PFRST','PLAST','HFRST','HLAST'),
        ` %-11s` = c('GENERAL','OPTIONS','METHODS','MANAGEMENT','OUTPUTS',
            'PLANTING','IRRIGATION','NITROGEN','RESIDUES','HARVEST'),
        ` %s` = c('SNAME','SMODEL')
        )
    return(fmt)
}
