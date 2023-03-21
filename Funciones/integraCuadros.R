integraCuadros = function(datEstim, # Estimaciones
                          id_Indicador,
                          varAggr = 'opcG02', # variables para quitar y luego agregar los datos
                          addTotal = FALSE,
                          filtroKeep = NULL,
                          varsJoin = c('opcG01','opcFila'),
                          accionInt = 'cbind') { # poner cbind si se quieren pegar uno al lado del otro
  
  if(is.null(accionInt) | accionInt == '') {
    res = armaCuadros(datEstim,
                      id_Indicador,
                      varAggr,
                      addTotal,
                      filtroKeep)
  } else {
    resList = sapply(id_Indicador,function(xx) {
      resAux = armaCuadros(datEstim,
                        id_Indicador = xx,
                        varAggr,
                        addTotal,
                        filtroKeep)
      list(resAux)
    })
    
   for(ii in 2:length(id_Indicador)) {
     resList[[ii]] = resList[[ii]] %>% select(-any_of(varsJoin))
   }
    
    res = do.call(cbind,resList)
  }
  
  return(res)
}
