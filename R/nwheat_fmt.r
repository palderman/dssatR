nwheat.fmt <- function(){
    return(list(
        `%6.0f` = c("RTDP2","PLGP1","RTDP1", "VREQ", "DSGFT", "CDAY"),
        `%6.1f` = c("TBASE", "TOPT", "ROPT", "TTOP", "P20", "GDDE", 
            "RUE", "TSEN","P1", "P5","GRNO", "PHINT","SLAP1", "SLAP2",
            "P3AF",  "MXGWT"),
        `%6.2f` = c("STMMX","VSEN", "PPSEN", "KCAN","TC1P1",
            "P4AF","P2AF","PLGP2","MXFIL", "TC1P2",
            "P5AF", "P6AF", "ADLAI","ADTIL", "ADPHO", "STEMN","MXNUP",
            "WFNU", "EXNO3", "MNNO3", "EXNH4", "MNNH4", "INGWT",
            "GPPSS", "GPPES", "MNRTN"),
        `%6.3f` = c(  "DTNP1", "MXNCR","PNUPR",
            "INGNC", "FREAR", "MNNCR","NOMOB"),
        `%7s` = c("ECO#", "VAR#"), 
        `%6s` = "EXPNO",
        `%-17s` = "ECONAME.........",
        `%-16s` = "VRNAME.........."))
}
