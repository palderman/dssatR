get.pos.names <- function(filex,start,end){
    svnames = gsub('\\.','',unlist(strsplit(gsub('@',' ',filex[start]),'  *')))
    svnames = svnames[!nchar(svnames)==0]
    widths = get.widths(svnames)
    if('TNAME'%in%svnames&'CU'%in%svnames){
        widths[svnames=='TNAME'] = 25
    }
    widths[nchar(svnames)>1] = widths[nchar(svnames)>1] + 1
    widths[svnames%in%c('OPTIONS','METHODS','MANAGEMENT','OUTPUTS',
                        'PLANTING','IRRIGATION','NITROGEN','RESIDUES',
                        'HARVEST','ID_SOIL','SANAME')] = 
            widths[svnames%in%c('OPTIONS','METHODS','MANAGEMENT','OUTPUTS',
                        'PLANTING','IRRIGATION','NITROGEN','RESIDUES',
                        'HARVEST','ID_SOIL','SANAME')]+1
    if(any(sum(widths)!=nchar(filex[(start+1):end]))){
        nlines = end-start
        diffs = nchar(filex[(start+1):end])-sum(widths)
        widths = matrix(nrow=nlines,rep(widths,nlines),byrow=T)
        pos1 = widths
        for (j in 1:nlines){
            if(diffs[j]<0){
                widths[j,ncol(widths)] = widths[j,ncol(widths)] + diffs[j]
            }else if(diffs[j]<widths[j,ncol(widths)]){
                widths[j,ncol(widths)] = widths[j,ncol(widths)] + diffs[j]
            }else{
                for(k in seq(ncol(widths),1)){
                    if(diffs[j]>=widths[j,k]){
                        diffs[j] = diffs[j] - widths[j,k]
                        widths[j,k] = 0
                    }else{
                        widths[j,k] = widths[j,k] - diffs[j]
                        break
                    }
                }
            }
            if(ncol(pos1)>1){
                pos1[j,2:ncol(pos1)] = cumsum(widths[j,1:(ncol(widths)-1)])+1
            }
            pos1[j,1] = 1
        }
        pos2 = pos1 + widths - 1
    }else{
        pos1 = widths
        pos1[2:length(widths)] = cumsum(widths[1:(length(widths)-1)])+1
        pos1[1] = 1
        pos2 = pos1 + widths - 1
    }
    svnames = gsub('\\.','',svnames)
    return(list(pos1,pos2,svnames))
}

