integraCuadros = function(datEstim, # Estimaciones
                          id_Indicador,
                          varAggr = 'opcG02', # variables para quitar y luego agregar los datos
                          addTotal = FALSE,
                          filtroKeep = NULL,
                          varsJoin = c('opcG01','opcFila'),
                          accionInt = 'cbind', # poner cbind si se quieren pegar uno al lado del otro
                          colsDrop = NULL,
                          opcGroupNames = NULL,
                          opcFilaName = NULL,
                          restColsNames = NULL,
                          nround = 1,
                          flipp = F,
                          ordenCols = NULL,
                          simple = F) { 
  
  if(is.null(accionInt) | accionInt == '') {
    res = armaCuadros(datEstim,
                      id_Indicador,
                      varAggr,
                      addTotal,
                      filtroKeep,
                      nround,
                      flipp,
                      ordenCols,
                      simple)
  } else {
    resList = sapply(id_Indicador,function(xx) {
      resAux = armaCuadros(datEstim,
                        id_Indicador = xx,
                        varAggr,
                        addTotal,
                        filtroKeep,
                        nround,
                        flipp,
                        ordenCols,
                        simple)
      list(resAux)
    })
    
   for(ii in 2:length(id_Indicador)) {
     resList[[ii]] = resList[[ii]] %>% select(-any_of(varsJoin))
   }
    
    res = do.call(cbind,resList)
  }
  
  if(!is.null(colsDrop)) {
    res = res[,!colnames(res) %in% colsDrop]
  }
  
  
  if(!is.null(opcGroupNames)) colnames(res)[grep('opcG0',colnames(res))] = opcGroupNames
  if(!is.null(opcFilaName)) colnames(res)[colnames(res) == 'opcFila'] = opcFilaName
  if(!is.null(restColsNames)) colnames(res) = c(opcGroupNames,opcFilaName,restColsNames)
  
  
  return(res)
}
