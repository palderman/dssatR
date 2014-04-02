get.obs.pred <- function(variable,sqno=NULL,trno=NULL,run=NULL,add=NULL){ 
    if(!exists('output')&!exists('PlantGro')&
       !exists('Evaluate')&!exists('Summary')){
        stop('Model outputs have not been loaded.')
    }
    for(file in c('output','PlantGro','Evaluate','Summary')){
        if (exists(file)){
            output=get(file)
            if(variable%in%colnames(output)){
                break
            }else if(file=='Evaluate'&
              paste(variable,'S',sep='')%in%colnames(output)){
                colnames(output)=gsub(paste(variable,'S',sep=''),
                  variable,colnames(output))
                break
            }
        }
    }
    if(!variable%in%colnames(output)){
        stop(paste(variable,'not found in any loaded model output file.'))
    }
    if(!exists('filet')&!exists('filea')){
        stop('Neither File T nor File A have been loaded.')
    }
    for( file in c('filet','filea')){
        if (exists(file)){
            fileat=get(file)
            if(variable%in%colnames(fileat)) break
        }
    }
    if(!variable%in%colnames(fileat)){
        stop(paste(variable,'not found in File A or File T.'))
    }
    if (is.null(sqno)){
        if (is.null(trno)&is.null(run)){
            trno = as.integer(as.character(levels(as.factor(output$TRNO))))
        }else{
            if(!is.null(run)){
                trno = as.integer(as.character(levels(as.factor(output[output$RUN%in%run,]$TRNO))))
            }
        }
        if('DATE'%in%colnames(fileat)){
            obs = fileat[!is.na(fileat[,variable])&fileat$TRNO%in%trno,
                c('TRNO','DATE',variable)]
        }else{
            obs = fileat[!is.na(fileat[,variable])&fileat$TRNO%in%trno,
                c('TRNO',variable)]
        }
    }else{
        if('DATE'%in%colnames(fileat)){
            obs = fileat[!is.na(fileat[,variable])&fileat$SQNO%in%sqno,
              c('SQNO','DATE',variable)]
        }else{
            obs = fileat[!is.na(fileat[,variable])&fileat$SQNO%in%sqno,
              c('SQNO',variable)]
        }
    }
    pred = obs
    pred[,3] = 0
    for (i in 1:nrow(pred)){
        if(is.null(sqno)){
            if('DATE'%in%colnames(output)){
                pred[i,] = na.omit(output[round(output$DATE,'days')%in%round(pred$DATE[i],'days')&
                          output$TRNO%in%pred$TRNO[i],
                          c('TRNO','DATE',variable)])
            }else{
                pred[i,] = na.omit(output[output$TRNO%in%pred$TRNO[i],
                          c('TRNO',variable)])
            }
        }else{
            if('DATE'%in%colnames(output)){
                pred[i,] = na.omit(output[round(output$DATE,'days')%in%round(pred$DATE[i],'days')&
                          output$SQNO%in%pred$SQNO[i],
                          c('SQNO','DATE',variable)])
            }else{
                pred[i,] = na.omit(output[output$SQNO%in%pred$SQNO[i],
                          c('SQNO',variable)])
            }
        }
    }
#    obs = obs[,3]
#    pred = pred[,3]
    for (i in names(add)){
        assign(i,rep(add[[i]],nrow(obs)))
    }
    colnames(obs)[ncol(obs)] = 'obs'
    colnames(pred)[ncol(pred)] = 'pred'
#    opdata = merge(obs,pred,all=T)
    opdata=cbind(obs,pred)
    if(length(add)>0){
        opdata = do.call('data.frame',lapply(c('opdata',names(add)),as.name))
      }
    opdata = opdata[,!colnames(opdata)%in%c('SQNO','TRNO','DATE')]
    return(opdata)
}

