get.fmt <- function(prm.name,type,model){
    prm.name=toupper(prm.name)
    type=toupper(type)
    model=toupper(model)
fmt = structure(list(MODEL = c("FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", "FORAGE", 
"FORAGE", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", "CROPGRO", 
"CROPGRO", "CROPGRO", "CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", 
"CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", 
"CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", 
"CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", 
"CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", 
"CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", 
"CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", 
"CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", 
"CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", 
"CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", 
"CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", 
"CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", 
"CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", "CANEGRO", 
"CANEGRO", "CANEGRO", "CANEGRO", "FILEX", "FILEX", "FILEX", "FILEX", 
"FILEX", "FILEX", "FILEX", "FILEX", "SOIL", "SOIL", "CROPSIM", 
"CROPSIM", "CROPSIM", "CROPSIM", "CROPSIM", "CROPSIM", "CROPSIM", 
"CROPSIM", "CROPSIM", "CROPSIM", "CROPSIM", "CROPSIM", "CROPSIM", 
"CROPSIM", "CROPSIM", "CROPSIM", "CROPSIM", "CROPSIM", "CROPSIM", 
"CROPSIM", "CROPSIM", "CROPSIM", "CROPSIM", "CROPSIM", "CROPSIM", 
"CROPSIM", "CROPSIM", "CROPSIM", "CROPSIM", "CROPSIM", "CROPSIM", 
"CROPSIM", "CROPSIM", "CROPSIM", "CROPSIM", "CROPSIM", "CROPSIM", 
"CROPSIM", "CROPSIM", "CROPSIM", "CROPSIM", "CROPSIM", "CROPSIM", 
"CROPSIM", "CROPSIM", "CROPSIM", "CROPSIM", "CROPSIM", "CROPSIM", 
"CROPSIM", "CROPSIM", "CROPSIM", "CROPSIM", "CROPSIM", "CROPSIM", 
"CROPSIM"), PNAME = c("PARMAX", "PHTMAX", "KCAN", "CCMP", "CCMAX", 
"CCEFF", "FNPGN", "TYPPGN", "FNPGT", "TYPPGT", "XLMAXT", "YLMAXT", 
"FNPGL", "TYPPGL", "PGEFF", "SCV", "KDIF", "LFANGB", "SLWREF", 
"SLWSLO", "NSLOPE", "LNREF", "PGREF", "XPGSLW", "YPGSLW", "CICA", 
"CCNEFF", "CMXSF", "CQESF", "PGPATH", "RES30C", "R30C2", "RNO3C", 
"RNH4C", "RPRO", "RFIXN", "RCH20", "RLIP", "RLIG", "ROA", "RMIN", 
"PCH2O", "MRSWITCH", "TRSWITCH", "LFMRC", "STMMRC", "RTMRC", 
"STRMRC", "SHELMRC", "SDMMRC", "TRSFN(1)", "TRSFN(2)", "TRSFN(3)", 
"TRSFN(4)", "TRSTYP", "TEMPEFFECT", "PROLFI", "PROLFG", "PROLFF", 
"PROSTI", "PROSTG", "PROSTF", "PRORTI", "PRORTG", "PRORTF", "PROSHI", 
"PROSHG", "PROSHF", "SDPROS", "SDPROG", "PRONOD", "PROMIN", "PROMAX", 
"THETA", "PCARLF", "PCARST", "PCARRT", "PCARSH", "PCARSD", "PCARNO", 
"PLIPLF", "PLIPST", "PLIPRT", "PLIPSH", "PLIPNO", "PLIGLF", "PLIGST", 
"PLIGRT", "PLIGSH", "PLIGSD", "PLIGNO", "POALF", "POAST", "POART", 
"POASH", "POASD", "POANO", "PMINLF", "PMINST", "PMINRT", "PMINSH", 
"PMINSD", "PMINNO", "PROSRI", "PROSRG", "PROSRF", "PCARSR", "PLIPSR", 
"PLIGSR", "POASR", "PMINSR", "KCOLD", "PROLFR", "PROSTR", "PRORTR", 
"PROSRR", "PCHOLFF", "PCHOSTF", "PCHORTF", "PCHOSRF", "LIPTB", 
"LIPOPT", "SLOSUM*100", "CARMIN", "CMOBMX", "CADSTF", "CADPR1", 
"NMOBMX", "NVSMOB", "NRCVR", "XPODF", "ALPHL", "ALPHS", "ALPHR", 
"ALPHSH", "ALPHSR", "CMOBSRN", "CMOBSRX", "CADSRF", "NMOBSRN", 
"NMOBSRX", "CADPV", "LRMOB", "TYPLMOB", "NRMOB", "TYPNMOB", "CRREF", 
"TYPCREF", "LRREF", "TYPLREF", "PRREF", "TYPPREF", "SENNLV", 
"SENCLV", "SENNSV", "SENCSV", "SENNRV", "SENCRV", "SENNSRV", 
"SENCSRV", "SNACTM", "NODRGM", "DWNODI", "TTFIX", "NDTHMX", "CNODCR", 
"FNNGT", "TYPNGT", "FNFXT", "TYPFXT", "FNFXD", "TYPFXD", "FNFXW", 
"TYPFXW", "FNFXA", "TYPFXA", "XLEAF", "YLEAF", "YSTEM", "WTFSD", 
"PORPT", "FRSTMF", "FRLFF", "ATOP", "FRCNOD", "FRLFMX", "YSTOR", 
"FRSTRF", "FRSTRMX", "PWLF", "PWST", "PWRT", "PWSR", "MXWST", 
"XLFEST", "YLFEST", "YSTEST", "YSREST", "FINREF", "SLAREF", "SIZREF", 
"VSSINK", "EVMODC", "SLAMAX", "SLAMIN", "SLAPAR", "TURSLA", "XVGROW", 
"YVREF", "XSLATM", "YSLATM", "SENRTE", "SENRT2", "SENDAY", "FREEZ1", 
"FREEZ2", "ICMP", "TCMP", "LFSEN", "XSTAGE", "XSENMX", "SENPOR", 
"SENMAX", "RTDEPI", "RFAC1", "RTSEN", "RLDSM", "RTSDF", "RWUEP1", 
"RWUMX", "XRTFAC(1)", "YRTFAC(1)", "XRTFAC(2)", "YRTFAC(2)", 
"XRTFAC(3)", "YRTFAC(3)", "XRTFAC(4)", "YRTFAC(4)", "RTNO3", 
"RTNH4", "PORMIN", "RTEXF", "SETMAX", "SRMAX", "RFLWAB", "XMPAGE", 
"DSWBAR", "XFRMAX", "SHLAG", "FNPDT", "TYPPDT", "FNSDT", "TYPSDT", 
"XXFTEM", "YXFTEM", "XSWFAC", "YSWFAC", "XSWBAR", "YSWBAR", "XTRFAC", 
"YTRFAC", "DETACH", "DWC", "PR1DET", "PR2DET", "XP1DET", "XP2DET", 
"VEGET(TB)", "VEGET(TO1)", "VEGET(T02)", "VEGET(TM)", "VEGET(I)", 
"EARLY(TB)", "EARLY(TO1)", "EARLY(TO2)", "EARLY(TM)", "EARLY(I)", 
"LATE(TB)", "LATE(TO1)", "LATE(TO2)", "LATE(TM)", "LATE(I)", 
"XVSHT", "YVSHT", "YVSWH", "XHWTEM", "YHWTEM", "XHWPAR", "YHWPAR", 
"KEP", "EORATIO", "STRSRFL", "STRLYR1", "SENSR", "FNPTD(1)", 
"FNPTD(2)", "FNPTD(3)", "FNPTD(4)", "TYPPTD", "FNPMD(1)", "FNPMD(2)", 
"FNPMD(3)", "FNPMD(4)", "TYPPMD", "FMPGD(1)", "FMPGD(2)", "FMPGD(3)", 
"FMPGD(4)", "TYPPGD", "HARD1", "HARD2", "FRZDC", "FRZHRD(1)", 
"FRZHRD(2)", "FRZHRD(3)", "FRZHRD(4)", "TYPHRD", "VAR#", "VRNAME", 
"ECO#", "CSDL", "PPSEN", "EM-FL", "FL-SH", "FL-SD", "SD-PM", 
"FL-LF", "LFMAX", "SLAVR", "SIZLF", "XFRT", "WTPSD", "SFDUR", 
"SDPDV", "PODUR", "ECO#", "ECONAME", "MG", "TM", "THVAR", "PL-EM", 
"EM-V1", "V1-JU", "JU-R0", "PM06", "PM09", "LNGSH", "R7-R8", 
"FL-VS", "TRIFL", "RWDTH", "RHGHT", "THRSH", "SDPRO", "SDLIP", 
"R1PPO", "OPTBI", "SLOBI", "RDRMT", "RDRMG", "RDRMM", "RCHDP", 
"PARMAX", "PHTMAX", "KCAN", "CCMP", "CCMAX", "CCEFF", "FNPGN", 
"TYPPGN", "FNPGT", "TYPPGT", "XLMAXT", "YLMAXT", "FNPGL", "TYPPGL", 
"PGEFF", "SCV", "KDIF", "LFANGB", "SLWREF", "SLWSLO", "NSLOPE", 
"LNREF", "PGREF", "XPGSLW", "YPGSLW", "RES30C", "R30C2", "RNO3C", 
"RNH4C", "RPRO", "RFIXN", "RCH20", "RLIP", "RLIG", "ROA", "RMIN", 
"PCH2O", "PROLFI", "PROLFG", "PROLFF", "PROSTI", "PROSTG", "PROSTF", 
"PRORTI", "PRORTG", "PRORTF", "PROSHI", "PROSHG", "PROSHF", "SDPROS", 
"SDPROG", "PRONOD", "PROMIN", "PROMAX", "THETA", "PCARLF", "PCARST", 
"PCARRT", "PCARSH", "PCARSD", "PCARNO", "PLIPLF", "PLIPST", "PLIPRT", 
"PLIPSH", "PLIPNO", "PLIGLF", "PLIGST", "PLIGRT", "PLIGSH", "PLIGSD", 
"PLIGNO", "POALF", "POAST", "POART", "POASH", "POASD", "POANO", 
"PMINLF", "PMINST", "PMINRT", "PMINSH", "PMINSD", "PMINNO", "LIPTB", 
"LIPOPT", "SLOSUM*100", "CARMIN", "CMOBMX", "CADSTF", "CADPR1", 
"NMOBMX", "NVSMOB", "NRCVR", "XPODF", "ALPHL", "ALPHS", "ALPHR", 
"ALPHSH", "SNACTM", "NODRGM", "DWNODI", "TTFIX", "NDTHMX", "CNODCR", 
"FNNGT", "TYPNGT", "FNFXT", "TYPFXT", "FNFXD", "TYPFXD", "FNFXW", 
"TYPFXW", "FNFXA", "TYPFXA", "XLEAF", "YLEAF", "YSTEM", "WTFSD", 
"PORPT", "FRSTMF", "FRLFF", "ATOP", "FRCNOD", "FRLFMX", "FINREF", 
"SLAREF", "SIZREF", "VSSINK", "EVMODC", "SLAMAX", "SLAMIN", "SLAPAR", 
"TURSLA", "XVGROW", "YVREF", "XSLATM", "YSLATM", "SENRTE", "SENRT2", 
"SENDAY", "FREEZ1", "FREEZ2", "ICMP", "TCMP", "XSTAGE", "XSENMX", 
"SENPOR", "SENMAX", "RTDEPI", "RFAC1", "RTSEN", "RLDSM", "RTSDF", 
"RWUEP1", "RWUMX", "XRTFAC(1)", "YRTFAC(1)", "XRTFAC(2)", "YRTFAC(2)", 
"XRTFAC(3)", "YRTFAC(3)", "XRTFAC(4)", "YRTFAC(4)", "RTNO3", 
"RTNH4", "PORMIN", "RTEXF", "SETMAX", "SRMAX", "RFLWAB", "XMPAGE", 
"DSWBAR", "XFRMAX", "SHLAG", "FNPDT", "TYPPDT", "FNSDT", "TYPSDT", 
"XXFTEM", "YXFTEM", "XSWFAC", "YSWFAC", "XSWBAR", "YSWBAR", "XTRFAC", 
"YTRFAC", "DETACH", "DWC", "PR1DET", "PR2DET", "XP1DET", "XP2DET", 
"VEGET(TB)", "VEGET(TO1)", "VEGET(T02)", "VEGET(TM)", "VEGET(I)", 
"EARLY(TB)", "EARLY(TO1)", "EARLY(TO2)", "EARLY(TM)", "EARLY(I)", 
"LATE(TB)", "LATE(TO1)", "LATE(TO2)", "LATE(TM)", "LATE(I)", 
"XVSHT", "YVSHT", "YVSWH", "XHWTEM", "YHWTEM", "XHWPAR", "YHWPAR", 
"KEP", "EORATIO", "VAR#", "VRNAME", "EXPNO", "ECO#", "CSDL", 
"PPSEN", "EM-FL", "FL-SH", "FL-SD", "SD-PM", "FL-LF", "LFMAX", 
"SLAVR", "SIZLF", "XFRT", "WTPSD", "SFDUR", "SDPDV", "PODUR", 
"THRSH", "SDPRO", "SDLIP", "ECO#", "ECONAME", "MG", "TM", "THVAR", 
"PL-EM", "EM-V1", "V1-JU", "JU-R0", "PM06", "PM09", "LNGSH", 
"R7-R8", "FL-VS", "TRIFL", "RWDTH", "RHGHT", "R1PPO", "OPTBI", 
"SLOBI", "TBasePhotos", "CRITSW", "HuRecover", "RespQ10", "PCB", 
"RESPGCF", "MAX_ROOTPF", "FTCON", "SURCON", "RTCMPG", "WRK", 
"RLVMIN", "SENESF", "RESET", "PERCoeff", "CHTCoeff", "HillPar1", 
"EORATIO", "RWUEP1", "RWUEP2", "RWUMX", "LG_RATING", "LG_CRIT_WIND", 
"VAR#", "VAR-NAME", "ECO#", "MaxPARCE", "APFMX", "STKPFMAX", 
"SUCA", "TBFT", "Tthalfo", "TBase", "LFMAX", "MXLFAREA", "MXLFARNO", 
"PI1", "PI2", "PSWITCH", "TTPLNTEM", "TTRATNEM", "CHUPIBASE", 
"TT_POPGROWTH", "MAX_POP", "POPTT16", "LG_AMBASE", "ECO#", "ECO-NAME", 
"DELTTMAX", "SWDF2AMP", "CS_CNREDUC", "CS_CNPERIOD", "Tthalfa", 
"dPERdT", "EXTCFN", "EXTCFST", "LFNMXEXT", "AREAMX_CF(1)", "AREAMX_CF(2)", 
"AREAMX_CF(3)", "WIDCOR", "WMAX_CF(1)", "WMAX_CF(2)", "WMAX_CF(3)", 
"POPCF(1)", "POPCF(2)", "POPDECAY", "TTBASEEM", "TTBASELFEX", 
"TTBASEPOP", "TBASEPER", "LG_AMRANGE", "LG_GP_REDUC", "LDG_FI_REDUC", 
"LMAX_CF(1)", "LMAX_CF(2)", "LMAX_CF(3)", "MAXLFLENGTH", "MAXLFWIDTH", 
"SASC", "SANI", "SAOC", "FAMN", "ICRT", "ICRES", "ICRN", "ICREN", 
"SLPF", "SRGF", "VAR#", "VAR-NAME", "EXP#", "ECO#", "VREQ", "PPS1", 
"P8", "G#WTS", "GWTS", "SHWTS", "PHINT", "P1", "P2", "P3", "P4", 
"P5", "P6", "P7", "LA1S", "LAFV", "LAFR", "VBASE", "VEFF", "PPS2", 
"ECO#", "ECONAME", "PARUE", "PARU2", "PHL2", "PHF3", "SLAS", 
"LSENI", "LSPHS", "LSPHE", "TIL#S", "TIPHE", "TIFAC", "TDPHS", 
"TDPHE", "TDFAC", "TDSF", "RDGS", "HTSTD", "AWNS", "KCAN", "RS%A", 
"GN%S", "TKFH", "SSPHS", "SSPHE", "SHWTA", "GWWF", "G#SF", "RTNUP", 
"NUPNF", "NUPWF"), FORMAT = c("%6.1f", "%6.1f", "%6.2f", "%6.1f", 
"%6.2f", " %5.4f", "%6.2f", "%-6s", "%6.1f", "%-6s", "%6.1f", 
"%6.1f", "%6.1f", "%-6s", " %5.4f", "%6.2f", "%6.2f", "%6.1f", 
" %5.4f", " %5.4f", " %5.4f", "%6.2f", "%6.2f", "%6.3f", "%6.3f", 
"%6.1f", "%6.1f", "%6.3f", "%6.3f", "%-4s", "%12.1e", " %5.4f", 
"%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", 
"%6.3f", "%6.2f", "%6.2f", "%-6s", "%-6s", "%12.1e", "%12.1e", 
"%12.1e", "%12.1e", "%12.1e", "%12.1e", "%5.4f", " %6.4f", " %5.4f", 
"%6.1f", "%-6s", "%6.1f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", 
"%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", 
"%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", 
"%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", 
"%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", 
"%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", 
"%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", 
"%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", 
"%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", 
"%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.2f", "%6.3f", 
"%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.2f", "%6.3f", 
"%-6s", "%6.2f", "%6.2f", "%6.2f", "%6.2f", "%6.2f", "%6.3f", 
"%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.2f", "%-6s", 
"%6.2f", "%-6s", "%6.2f", "%-6s", "%6.2f", "%-6s", "%6.2f", "%-6s", 
"%6.2f", "%6.2f", "%6.2f", "%6.2f", "%6.2f", "%6.2f", "%6.2f", 
"%6.2f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", 
"%6.2f", "%-6s", "%6.2f", "%-6s", "%6.2f", "%-6s", "%6.2f", "%-6s", 
"%6.2f", "%-6s", "%6.1f", "%6.2f", "%6.2f", "%6.2f", "%6.2f", 
"%6.2f", "%6.2f", "%6.2f", "%6.2f", "%6.2f", "%6.2f", "%6.2f", 
"%6.2f", "%6.2f", "%6.2f", "%6.2f", "%6.2f", "%6.2f", "%6.1f", 
"%6.2f", "%6.2f", "%6.2f", "%6.0f", "%6.0f", "%6.1f", "%6.1f", 
"%6.1f", "%6.0f", "%6.0f", "%6.3f", "%6.2f", "%6.1f", "%6.1f", 
"%6.1f", "%6.2f", "%6.2f", "%6.2f", "%6.2f", "%6.1f", "%6.1f", 
"%6.2f", "%6.1f", "%6.2f", "%6.1f", "%6.1f", "%6.2f", "%6.1f", 
"%6.1f", "%6.0f", "%6.3f", "%6.1f", "%6.2f", "%6.2f", "%6.2f", 
"%6.1f", "%6.2f", "%6.1f", "%6.2f", "%6.1f", "%6.2f", "%6.1f", 
"%6.2f", "%6.3f", "%6.3f", "%6.2f", "%6.2f", "%6.2f", "%6.2f", 
"%6.2f", "%6.0f", "%6.1f", "%6.2f", "%6.2f", "%6.1f", "%-6s", 
"%6.1f", "%-6s", "%6.2f", "%6.2f", "%6.2f", "%6.2f", "%6.2f", 
"%6.2f", "%6.2f", "%6.2f", "%-6s", "%6.1f", " %5.4f", " %5.3f", 
"%6.2f", "%6.2f", "%6.1f", "%6.1f", "%6.1f", "%6.1f", "%16d", 
"%6.1f", "%6.1f", "%6.1f", "%6.1f", "%16d", "%6.1f", "%6.1f", 
"%6.1f", "%6.1f", "%16d", "%6.2f", "%6.3f", "%6.3f", "%6.1f", 
"%6.2f", "%6.2f", "%6.2f", "%6.2f", "%6.1f", "%6.3f", "%6.3f", 
"%6.3f", "%6.1f", "%6.1f", "%6.1f", "%6.3f", "%-6s", "%6.1f", 
"%6.1f", "%6.2f", "%6.3f", "%-6s", "%6.2f", "%6.2f", "%6.2f", 
"%6.3f", "%-6s", "%6.1f", "%6.1f", "%6.3f", "%6.1f", "%6.1f", 
"%6.1f", "%6.2f", "%-6s", "%-6s", "%-17s", "%-7s", "%6.2f", "%6.3f", 
"%6.1f", "%6.1f", "%6.1f", "%6.2f", "%6.2f", "%6.2f", "%6.0f", 
"%6.1f", "%6.2f", "%6.2f", "%6.1f", "%6.2f", "%6.1f", "%-6s", 
"%-17s", "%-3s", "%-3s", "%6.2f", "%6.1f", "%6.1f", "%6.1f", 
"%6.0f", "%6.1f", "%6.2f", "%6.1f", "%6.0f", "%6.0f", "%6.2f", 
"%6.1f", "%6.1f", "%6.1f", "%6.3f", "%6.3f", "%6.0f", "%6.3f", 
"%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.1f", "%6.1f", 
"%6.2f", "%6.1f", "%6.2f", " %5.4f", "%6.2f", "%-6s", "%6.1f", 
"%-6s", "%6.1f", "%6.1f", "%6.1f", "%-6s", " %5.4f", "%6.2f", 
"%6.2f", "%6.1f", " %5.4f", " %5.4f", " %5.4f", "%6.2f", "%6.3f", 
"%6.3f", "%6.3f", "%12.1e", " %5.4f", "%6.3f", "%6.3f", "%6.3f", 
"%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.2f", "%6.2f", 
"%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", 
"%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", 
"%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", 
"%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", 
"%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", 
"%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", 
"%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.2f", 
"%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.2f", 
"%6.3f", "%-6s", "%6.2f", "%6.2f", "%6.2f", "%6.2f", "%6.3f", 
"%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.3f", "%6.2f", "%-6s", 
"%6.2f", "%-6s", "%6.2f", "%-6s", "%6.2f", "%-6s", "%6.2f", "%-6s", 
"%6.1f", "%6.2f", "%6.2f", "%6.2f", "%6.2f", "%6.2f", "%6.2f", 
"%6.2f", "%6.2f", "%6.2f", "%6.0f", "%6.0f", "%6.1f", "%6.1f", 
"%6.1f", "%6.0f", "%6.0f", "%6.3f", "%6.2f", "%6.1f", "%6.1f", 
"%6.1f", "%6.2f", "%6.2f", "%6.2f", "%6.2f", "%6.1f", "%6.1f", 
"%6.2f", "%6.1f", "%6.1f", "%6.1f", "%6.2f", "%6.1f", "%6.1f", 
"%6.0f", "%6.3f", "%6.1f", "%6.2f", "%6.2f", "%6.2f", "%6.1f", 
"%6.2f", "%6.1f", "%6.2f", "%6.1f", "%6.2f", "%6.1f", "%6.2f", 
"%6.3f", "%6.3f", "%6.2f", "%6.2f", "%6.2f", "%6.2f", "%6.2f", 
"%6.0f", "%6.1f", "%6.2f", "%6.2f", "%6.1f", "%-6s", "%6.1f", 
"%-6s", "%6.2f", "%6.2f", "%6.2f", "%6.2f", "%6.2f", "%6.2f", 
"%6.2f", "%6.2f", "%-6s", "%6.1f", " %5.4f", " %5.3f", "%6.2f", 
"%6.2f", "%6.1f", "%6.1f", "%6.1f", "%6.1f", "%16d", "%6.1f", 
"%6.1f", "%6.1f", "%6.1f", "%16d", "%6.1f", "%6.1f", "%6.1f", 
"%6.1f", "%16d", "%6.2f", "%6.3f", "%6.3f", "%6.1f", "%6.2f", 
"%6.2f", "%6.2f", "%6.2f", "%6.1f", "%-6s ", "%-16s ", "%-5s", 
"%-6s", "%6.2f", "%6.2f", "%6.1f", "%6.1f", "%6.1f", "%6.2f", 
"%6.2f", "%6.2f", "%6.0f", "%6.1f", "%6.2f", "%6.2f", "%6.1f", 
"%6.2f", "%6.1f", "%6.1f", "%6.3f", "%6.3f", "%-6s ", "%-17s ", 
"%-2s", "%-2s", "%6.2f", "%6.2f", "%6.2f", "%6.2f", "%6.2f", 
"%6.2f", "%6.2f", "%6.2f", "%6.2f", "%6.2f", "%6.2f", "%6.1f", 
"%6.1f", "%6.3f", "%6.3f", "%6.3f", "%3.0f", "%4.1f", "%5.0f", 
"%5.2f", "%4.1f", "%6.3f", "%7.2f", "%5.2f", "%5.2f", "%5.0f", 
"%7.2f", "%5.2f", "%3.0f", "%3.0f", "%5.2f", "%6.3f", "%6.3f", 
"%5.2f", "%3.0f", "%3.0f", "%5.2f", "%3.0f", "%5.0f", "%-7s", 
"%-19s", "%-6s", "%15.2f", "%15.2f", "%15.2f", "%15.2f", "%15.0f", 
"%15.0f", "%15.0f", "%14.0f", "%15.0f", "%15.0f", "%15.0f", "%15.0f", 
"%15.0f", "%15.0f", "%15.0f", "%15.0f", "%15.0f", "%15.0f", "%15.1f", 
"%15.0f", "%-6s ", "%-18s", "%15.2f", "%15.1f", "%15.1f", "%15.0f", 
"%15.0f", "%15.3f", "%15.2f", "%15.2f", "%15.0f", "%14.0f", "%15.1f", 
"%15.1f", "%15.0f", "%15.4f", "%15.3f", "%15.2f", "%15.3f", "%15.5f", 
"%11.3f", "%16.0f", "%15.0f", "%15.0f", "%14.3f", "%15.0f", "%15.2f", 
"%15.1f", "%15.3f", "%15.1f", "%15.1f", "%15.0f", "%15.1f", "%6.3f", 
"%6.3f", "%6.3f", "%6.1f", "%6.0f", "%6.0f", "%6.2f", "%6.2f", 
"%6.2f", "%6.3f", "%-6s ", "%-16s ", "%-5s", "%-7s", "%6.0f", 
"%6.0f", "%6.0f", "%6.0f", "%6.0f", "%6.1f", "%6.0f", "%6.0f", 
"%6.0f", "%6.0f", "%6.0f", "%6.0f", "%6.0f", "%6.0f", "%6.1f", 
"%6.2f", "%6.2f", "%6.0f", "%6.1f", "%6.0f", "%-6s ", "%-16s ", 
"%6.1f", "%6.1f", "%6.1f", "%6.1f", "%6.0f", "%6.1f", "%6.1f", 
"%6.1f", "%6.1f", "%6.1f", "%6.1f", "%6.1f", "%6.1f", "%6.1f", 
"%6.2f", "%6.1f", "%6.0f", "%6.1f", "%6.2f", "%6.0f", "%6.1f", 
"%6.0f", "%6.1f", "%6.1f", "%6.1f", "%6.1f", "%6.1f", "%6.3f", 
"%6.2f", "%6.1f"), LPOS1 = c(1L, 7L, 13L, 1L, 7L, 13L, 1L, 25L, 
1L, 25L, 1L, 1L, 1L, 25L, 1L, 7L, 13L, 19L, 1L, 7L, 13L, 19L, 
25L, 1L, 1L, 1L, 7L, 13L, 19L, 25L, 1L, 13L, 1L, 7L, 13L, 19L, 
1L, 7L, 13L, 19L, 25L, 31L, 1L, 7L, 1L, 13L, 25L, 1L, 13L, 25L, 
1L, 6L, 13L, 19L, 25L, 31L, 1L, 7L, 13L, 19L, 25L, 31L, 1L, 7L, 
13L, 19L, 25L, 31L, 1L, 7L, 13L, 19L, 25L, 31L, 1L, 7L, 13L, 
19L, 25L, 31L, 1L, 7L, 13L, 19L, 25L, 1L, 7L, 13L, 19L, 25L, 
31L, 1L, 7L, 13L, 19L, 25L, 31L, 1L, 7L, 13L, 19L, 25L, 31L, 
1L, 7L, 13L, 1L, 7L, 13L, 19L, 25L, 1L, 7L, 13L, 19L, 25L, 1L, 
7L, 13L, 19L, 1L, 7L, 13L, 19L, 1L, 7L, 13L, 19L, 25L, 31L, 1L, 
1L, 7L, 13L, 19L, 25L, 1L, 7L, 13L, 19L, 25L, 31L, 1L, 25L, 1L, 
25L, 1L, 25L, 1L, 25L, 1L, 25L, 1L, 7L, 13L, 19L, 1L, 7L, 13L, 
19L, 1L, 7L, 13L, 19L, 25L, 31L, 1L, 25L, 1L, 25L, 1L, 25L, 1L, 
25L, 1L, 25L, 1L, 1L, 1L, 1L, 7L, 13L, 19L, 25L, 31L, 1L, 1L, 
1L, 7L, 1L, 7L, 13L, 19L, 25L, 1L, 1L, 1L, 1L, 1L, 7L, 13L, 19L, 
25L, 1L, 7L, 13L, 19L, 1L, 1L, 1L, 1L, 1L, 7L, 13L, 19L, 25L, 
1L, 7L, 13L, 1L, 25L, 1L, 25L, 1L, 7L, 13L, 19L, 25L, 31L, 37L, 
1L, 7L, 13L, 19L, 25L, 31L, 37L, 43L, 1L, 7L, 13L, 19L, 1L, 7L, 
13L, 19L, 1L, 7L, 13L, 1L, 25L, 1L, 25L, 1L, 1L, 1L, 1L, 1L, 
1L, 1L, 1L, 1L, 7L, 13L, 19L, 25L, 31L, 1L, 7L, 13L, 19L, 25L, 
1L, 7L, 13L, 19L, 25L, 1L, 7L, 13L, 19L, 25L, 1L, 1L, 1L, 1L, 
1L, 1L, 1L, 1L, 7L, 1L, 7L, 1L, 1L, 7L, 13L, 19L, 25L, 1L, 7L, 
13L, 19L, 25L, 1L, 7L, 13L, 19L, 25L, 1L, 7L, 13L, 1L, 7L, 13L, 
19L, 25L, 1L, 8L, 25L, 31L, 37L, 43L, 49L, 55L, 61L, 67L, 73L, 
79L, 85L, 91L, 97L, 103L, 109L, 115L, 1L, 7L, 25L, 28L, 31L, 
37L, 43L, 49L, 55L, 61L, 67L, 73L, 79L, 85L, 91L, 97L, 103L, 
109L, 115L, 121L, 127L, 133L, 139L, 145L, 151L, 157L, 163L, 1L, 
7L, 13L, 1L, 7L, 13L, 1L, 25L, 1L, 25L, 1L, 1L, 1L, 25L, 1L, 
7L, 13L, 19L, 1L, 7L, 13L, 19L, 25L, 1L, 1L, 1L, 13L, 1L, 7L, 
13L, 19L, 1L, 7L, 13L, 19L, 25L, 31L, 1L, 7L, 13L, 19L, 25L, 
31L, 1L, 7L, 13L, 19L, 25L, 31L, 1L, 7L, 13L, 19L, 25L, 31L, 
1L, 7L, 13L, 19L, 25L, 31L, 1L, 7L, 13L, 19L, 25L, 1L, 7L, 13L, 
19L, 25L, 31L, 1L, 7L, 13L, 19L, 25L, 31L, 1L, 7L, 13L, 19L, 
25L, 31L, 1L, 7L, 13L, 19L, 1L, 7L, 13L, 19L, 25L, 31L, 1L, 1L, 
7L, 13L, 19L, 1L, 7L, 13L, 19L, 25L, 31L, 1L, 25L, 1L, 25L, 1L, 
25L, 1L, 25L, 1L, 25L, 1L, 1L, 1L, 1L, 7L, 13L, 19L, 25L, 31L, 
1L, 1L, 7L, 13L, 19L, 25L, 1L, 7L, 13L, 19L, 1L, 1L, 1L, 1L, 
1L, 7L, 13L, 19L, 25L, 1L, 7L, 1L, 25L, 1L, 25L, 1L, 7L, 13L, 
19L, 25L, 31L, 37L, 1L, 7L, 13L, 19L, 25L, 31L, 37L, 43L, 1L, 
7L, 13L, 19L, 1L, 7L, 13L, 19L, 1L, 7L, 13L, 1L, 25L, 1L, 25L, 
1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 7L, 13L, 19L, 25L, 31L, 1L, 
7L, 13L, 19L, 25L, 1L, 7L, 13L, 19L, 25L, 1L, 7L, 13L, 19L, 25L, 
1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 7L, 1L, 8L, 25L, 31L, 37L, 43L, 
49L, 55L, 61L, 67L, 73L, 79L, 85L, 91L, 97L, 103L, 109L, 115L, 
121L, 127L, 133L, 139L, 1L, 8L, 26L, 29L, 31L, 37L, 43L, 49L, 
55L, 61L, 67L, 73L, 79L, 85L, 91L, 97L, 103L, 109L, 115L, 121L, 
22L, 22L, 22L, 22L, 22L, 22L, 23L, 22L, 22L, 23L, 23L, 23L, 22L, 
22L, 22L, 22L, 22L, 22L, 22L, 22L, 22L, 22L, 22L, 1L, 8L, 27L, 
33L, 48L, 63L, 78L, 93L, 108L, 123L, 138L, 152L, 167L, 182L, 
197L, 212L, 227L, 242L, 257L, 272L, 287L, 302L, 317L, 1L, 8L, 
26L, 41L, 56L, 71L, 86L, 101L, 116L, 131L, 146L, 161L, 175L, 
190L, 205L, 220L, 235L, 250L, 265L, 280L, 295L, 306L, 322L, 337L, 
352L, 366L, 381L, 396L, 411L, 426L, 441L, 456L, 471L, 51L, 21L, 
15L, 27L, 15L, 45L, 27L, 51L, 37L, 31L, 1L, 8L, 25L, 30L, 37L, 
43L, 49L, 55L, 61L, 67L, 73L, 79L, 85L, 91L, 97L, 103L, 109L, 
115L, 121L, 127L, 133L, 139L, 145L, 151L, 1L, 8L, 25L, 31L, 37L, 
43L, 49L, 55L, 61L, 67L, 73L, 79L, 85L, 91L, 97L, 103L, 109L, 
115L, 121L, 127L, 133L, 139L, 145L, 151L, 157L, 163L, 169L, 175L, 
181L, 187L, 193L, 199L), TYPE = c("SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "CUL", "CUL", 
"CUL", "CUL", "CUL", "CUL", "CUL", "CUL", "CUL", "CUL", "CUL", 
"CUL", "CUL", "CUL", "CUL", "CUL", "CUL", "CUL", "ECO", "ECO", 
"ECO", "ECO", "ECO", "ECO", "ECO", "ECO", "ECO", "ECO", "ECO", 
"ECO", "ECO", "ECO", "ECO", "ECO", "ECO", "ECO", "ECO", "ECO", 
"ECO", "ECO", "ECO", "ECO", "ECO", "ECO", "ECO", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "CUL", 
"CUL", "CUL", "CUL", "CUL", "CUL", "CUL", "CUL", "CUL", "CUL", 
"CUL", "CUL", "CUL", "CUL", "CUL", "CUL", "CUL", "CUL", "CUL", 
"CUL", "CUL", "CUL", "ECO", "ECO", "ECO", "ECO", "ECO", "ECO", 
"ECO", "ECO", "ECO", "ECO", "ECO", "ECO", "ECO", "ECO", "ECO", 
"ECO", "ECO", "ECO", "ECO", "ECO", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", "SPE", 
"SPE", "CUL", "CUL", "CUL", "CUL", "CUL", "CUL", "CUL", "CUL", 
"CUL", "CUL", "CUL", "CUL", "CUL", "CUL", "CUL", "CUL", "CUL", 
"CUL", "CUL", "CUL", "CUL", "CUL", "CUL", "ECO", "ECO", "ECO", 
"ECO", "ECO", "ECO", "ECO", "ECO", "ECO", "ECO", "ECO", "ECO", 
"ECO", "ECO", "ECO", "ECO", "ECO", "ECO", "ECO", "ECO", "ECO", 
"ECO", "ECO", "ECO", "ECO", "ECO", "ECO", "ECO", "ECO", "ECO", 
"ECO", "ECO", "ECO", "FLX", "FLX", "FLX", "FLX", "FLX", "FLX", 
"FLX", "FLX", "SOL", "SOL", "CUL", "CUL", "CUL", "CUL", "CUL", 
"CUL", "CUL", "CUL", "CUL", "CUL", "CUL", "CUL", "CUL", "CUL", 
"CUL", "CUL", "CUL", "CUL", "CUL", "CUL", "CUL", "CUL", "CUL", 
"CUL", "ECO", "ECO", "ECO", "ECO", "ECO", "ECO", "ECO", "ECO", 
"ECO", "ECO", "ECO", "ECO", "ECO", "ECO", "ECO", "ECO", "ECO", 
"ECO", "ECO", "ECO", "ECO", "ECO", "ECO", "ECO", "ECO", "ECO", 
"ECO", "ECO", "ECO", "ECO", "ECO", "ECO")), .Names = c("MODEL", 
"PNAME", "FORMAT", "LPOS1", "TYPE"), row.names = c(NA, -766L), class = "data.frame")
    fmt.out = vector(length=length(prm.name),mode='character')
    for(i in 1:length(prm.name)){
        fmt.out[i]=with(fmt,
            FORMAT[MODEL==model[i]&TYPE==type[i]&PNAME==prm.name[i]])
    }
    return(fmt.out)
}

