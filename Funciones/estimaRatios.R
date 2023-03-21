estimaRatios = function(varUp,varDown,disenio,varBy,filtro,nomIndic) {
  
  disAux = disenio
  if(!is.null(varDown)) disAux = subset(disenio, get(varDown) == 1)
  
  if(!is.null(filtro)) {
    evalFiltro = paste0('subset(disAux, ',filtro,')')
    disAux = eval(parse(text = evalFiltro))  
  }
  
  formMean = as.formula(paste0('~',varUp))
  formBy = as.formula(paste0('~',paste(varBy,collapse = '+')))
  
  cc = svyby(formMean,formBy,disAux,svymean)
  cc[,c(varUp,'se')] = round(100*cc[,c(varUp,'se')],1)
  
  cc = cc %>% rename(!!nomIndic := all_of(varUp))
  
  if(length(varBy) >= 1 & nrow(cc) > 1) {
    
    ### Total 
    
    total = svymean(formMean,disAux)
    Total = round(100*as.numeric(total[varUp]),1)
    
    ### Marginales FIlas
    formByR = as.formula(paste0('~',paste(varBy[1],collapse = '+')))
    margRow = svyby(formMean,formByR,disAux,svymean)
    margRow[,c(varUp,'se')] = round(100*margRow[,c(varUp,'se')],1)
    margRow = margRow %>% rename(!!nomIndic := all_of(varUp))
    
    margROW = margRow[,nomIndic]
    
    if(length(varBy) == 2) {
      cc <- cc %>% pivot_wider(., id_cols = !!varBy[1],names_from = !!varBy[2], values_from = !!nomIndic)
      
      ccc = cbind(cc,Total = margROW)
      ### Marginales Columnas
      formByC = as.formula(paste0('~',paste(varBy[2],collapse = '+')))
      margCol = svyby(formMean,formByC,disAux,svymean)
      margCol[,c(varUp,'se')] = round(100*margCol[,c(varUp,'se')],1)
      margCol = margCol %>% rename(!!nomIndic := all_of(varUp))
      
      
      cccF  = rbind(ccc,c(NA,margCol$TD,Total))
      
      cccF[is.na(cccF[,1]),1] = 'Total'
      
    } else {
      
      
      ccc = data.frame(cc)
      rownames(ccc) = ccc[,1]
      ccc = ccc %>% select(-one_of('se')) 
      
      cccF = rbind(ccc,c(NA,Total))
      cccF[is.na(cccF[,1]),1] = 'Total'
      
    }
  } else {
    cccF = cc
  }
  
  return(cccF)  
}
