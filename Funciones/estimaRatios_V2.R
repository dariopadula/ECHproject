estimaRatios_V2 = function(varInteres = 'pobre',
                           filtro = NULL,
                           disenio = disImp,
                           varsGroup = c('anio','semestre'),
                           varFila = 'municipio',
                           varCol = 'rEdades4',
                           tipoBase = 'per',
                           tipoVar,
                           funcion,
                           id_ind = '1') {
  
  disAux = disenio
  
  
  if(filtro == "") filtro = NULL
  if(varCol == "") varCol = NULL
  if(varFila == "") varFila = NULL
  
  varsGroup = strsplit(varsGroup,',| |;')[[1]]
  varsGroup = varsGroup[varsGroup != '']
  
  if(!is.null(filtro)) {
    evalFiltro = paste0('subset(disAux, ',filtro,')')
    disAux = eval(parse(text = evalFiltro))  
  }
  
  ################################
  ### Indica situacion de variables
  indSit = sum(c(!is.null(varCol),!is.null(varFila)))
  ##################################
  resAux = list()
  ##################################
  
  formMean = as.formula(paste0('~',varInteres))
  
  allVarsGroup = c(varsGroup,varFila,varCol)
  formBy = as.formula(paste0('~',paste(allVarsGroup,collapse = '+')))
  
  cc = svyby(formMean,formBy,disAux,svymean)
  cc = cc %>% select(-se)
  if(tipoVar == 'dic') cc[,c(varInteres)] = round(100*cc[,c(varInteres)],1)
  
  resAux[[length(resAux) + 1]] = cc
  
  nomCols = names(cc)
  ###### Total para quitando la variable fila
  if(indSit == 2) {
    allVarsGroup = c(varsGroup,varCol)
    formBy = as.formula(paste0('~',paste(allVarsGroup,collapse = '+')))
    
    ccC = svyby(formMean,formBy,disAux,svymean)
    ccC = ccC %>% select(-se)
    if(tipoVar == 'dic') ccC[,c(varInteres)] = round(100*ccC[,c(varInteres)],1)
    
    ccC[,c(varFila)] = 'Total'
    
    resAux[[length(resAux) + 1]] = ccC
    
    
    allVarsGroup = c(varsGroup,varFila)
    formBy = as.formula(paste0('~',paste(allVarsGroup,collapse = '+')))
    
    ccF = svyby(formMean,formBy,disAux,svymean)
    ccF = ccF %>% select(-se)
    if(tipoVar == 'dic') ccF[,c(varInteres)] = round(100*ccF[,c(varInteres)],1)
    
    ccF[,c(varCol)] = 'Total'
    
    resAux[[length(resAux) + 1]] = ccF
  }

  ###### Total para quitando ambas
  if(!(is.null(varFila) & is.null(varCol))) {
    allVarsGroup = c(varsGroup)
    formBy = as.formula(paste0('~',paste(allVarsGroup,collapse = '+')))
    
    ccFC = svyby(formMean,formBy,disAux,svymean)
    ccFC = ccFC %>% select(-se)
    if(tipoVar == 'dic') ccFC[,c(varInteres)] = round(100*ccFC[,c(varInteres)],1)
    
    ccFC[,c(varCol)] = 'Total'
    ccFC[,c(varFila)] = 'Total'
    
    resAux[[length(resAux) + 1]] = ccFC
  }
  
   
  ############################################# 
  #############################################
  res = do.call(rbind, lapply(resAux,function(xx) xx[,nomCols]))
  # res = rbind(cc,ccF[,names(cc)],ccC[,names(cc)],ccFC[,names(cc)])
  
  ##############################################
  ###### Arma data frame comun
  nomRes = c("varInteres","varG01","opcG01","varG02","opcG02","varG03","opcG03",
             "varFila","opcFila","varCol","opcCol","est","tipoBase","tipoVar","funcion","id_ind")
  
  dfRes = data.frame(matrix(NA,nrow = nrow(res),ncol = length(nomRes)))
  colnames(dfRes) = nomRes
  
  dfRes[,"varInteres"] = varInteres
  for(hh in 1:length(varsGroup)) {
    dfRes[,paste0("varG0",hh)] = varsGroup[hh]
    dfRes[,paste0("opcG0",hh)] = res[,varsGroup[hh]]
  }
  
  
  
  if(!is.null(varFila)) {
    dfRes[,"opcFila"] = res[,varFila] 
    dfRes[,"varFila"] = varFila
  } else {
    dfRes[,"varFila"] = ""
    dfRes[,"opcFila"] = 'Total'
  }
  
  
  if(!is.null(varCol)) {
    dfRes[,"varCol"] = varCol
    dfRes[,"opcCol"] = res[,varCol]
  } else {
    dfRes[,"varCol"] = ""
    dfRes[,"opcCol"] = 'Total'
  }
  
  
  dfRes[,"est"] = res[,varInteres]
  dfRes[,"tipoBase"] = tipoBase
  dfRes[,"id_ind"] = id_ind
  dfRes[,"tipoVar"] = tipoVar
  dfRes[,"funcion"] = funcion
  
  
  return(dfRes)  
}

