get.widths <- function(vnames,type='filex'){
    if(type=='filex'){
        widths = unlist(lapply(vnames,FUN=function(x){
            width = switch(x,
                       CU=2,FL=2,SA=2,IC=2,MP=2,MI=2,MF=2,MR=2,MC=2,
                        MT=2,ME=2,MH=2,SM=2,CR=2,A=2,C=2,F=2,H=2,L=2,
                        N=2,O=2,P=2,R=2,T=2,PEOPLE=120,ADDRESS=120,
                        SITE=120,PAREA=6,HARM=120,INGENO=6,CNAME=120,
                        ID_FIELD=8,WSTA=8,SLTX=4,ID_SOIL=10,FLNAME=120,
                        XCRD=15,YCRD=15,ELEV=9,AREA=17,SANAME=120,
                        ICNAME=120,PLNAME=120,I=2,IRNAME=120,
                        FERNAME=120,TNAME=120,R=2,HNAME=120,GENERAL=11,
                        SNAME=25,OPTIONS=11,METHODS=11,MANAGEMENT=11,
                        OUTPUTS=11,PLANTING=11,IRRIGATION=11,
                        NITROGEN=11,RESIDUES=11,HARVEST=11,
                        5)
                return(width)
        }))
    } else if(type=='cul'){
        widths = unlist(lapply(vnames,FUN=function(x){
            width = switch(x,
                       'VAR#'=6,'VAR-NAME'=16,'EXP#'=6,'ECO#'=6,
                       'P1V'=6,'P1D=6',
                       CU=2,FL=2,SA=2,IC=2,MP=2,MI=2,MF=2,MR=2,MC=2,
                        MT=2,ME=2,MH=2,SM=2,CR=2,A=2,C=2,F=2,H=2,L=2,
                        N=2,O=2,P=2,R=2,T=2,PEOPLE=120,ADDRESS=120,
                        SITE=120,PAREA=6,HARM=120,INGENO=6,CNAME=120,
                        ID_FIELD=8,WSTA=8,SLTX=4,ID_SOIL=10,FLNAME=120,
                        XCRD=15,YCRD=15,ELEV=9,AREA=17,SANAME=120,
                        ICNAME=120,PLNAME=120,I=2,IRNAME=120,
                        FERNAME=120,TNAME=120,R=2,HNAME=120,GENERAL=11,
                        SNAME=25,OPTIONS=11,METHODS=11,MANAGEMENT=11,
                        OUTPUTS=11,PLANTING=11,IRRIGATION=11,
                        NITROGEN=11,RESIDUES=11,HARVEST=11,
                        5)
                return(width)
        }))
    }
    return(widths)
}

