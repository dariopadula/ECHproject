armaCuadros = function(datEstim, # Estimaciones
                       id_Indicador,
                       varAggr = 'opcG02', # variables para quitar y luego agregar los datos
                       addTotal = FALSE,
                       filtroKeep = NULL, # Filtro como string para capturar las estimaciones que se quieran
                       # flipTab = FALSE # Indica si hay que trasponer la tabla
                       nround,
                       flipp = F,
                       ordenCols = NULL,
                       simple = F) {
  
  
  long_table = estimAll %>% filter(id_ind %in% c(id_Indicador))
  
  if(!is.null(varAggr)) {
    varsAgrup = colnames(datEstim)[!colnames(datEstim) %in% c('est',varAggr)] 
    
    long_table = long_table %>% 
      group_by_at(vars(varsAgrup)) %>% 
      summarise(est = mean(est,na.rm = T)) %>% 
      ungroup() %>% 
      suppressWarnings() %>% suppressMessages()
  }
  
  long_table = long_table %>% mutate(est = round(est,nround))
  
  varsSelect = c('varInteres','opcG01','opcG02','opcFila','opcCol','est','label')
  
  long_table = long_table %>% 
    select(any_of(varsSelect)) %>% 
    mutate(label = ifelse(is.na(label),varInteres,label))
  
  
  if(!simple) {
    varsID = c('opcG01', 'opcG02','opcFila')
    wide_table <- long_table %>%
      pivot_wider(names_from = label, values_from = est, 
                  id_cols = any_of(varsID))
  } else {
    varsID = c('opcG01', 'opcG02')

    # if(length(id_Indicador) > 1) {
    #   longSplit = split(long_table,long_table$varInteres)
    #   longSplit = lapply(longSplit,function(xx) {
    #     yy <- xx %>%  
    #       pivot_wider(names_from = 'opcFila', values_from = est, 
    #                   id_cols = any_of(varsID))
    #   })
    #   
    #   for(ii in 2:length(id_Indicador)) {
    #     longSplit[[ii]] = longSplit[[ii]] %>% select(-any_of(varsJoin))
    #   }
    #   wide_table = do.call(cbind,longSplit)
    # } else {
      wide_table <- long_table %>%  
        pivot_wider(names_from = 'opcFila', values_from = est, 
                    id_cols = any_of(varsID))  
  }
  
  ####### Agrega la columa de 100 en caso de las distribuciones
  if(addTotal)  wide_table = wide_table %>% mutate(Total = 100)
  
  
  if(flipp) {  
    Cols = colnames(wide_table)[(which(colnames(wide_table) == 'opcFila')+1):ncol(wide_table)]
    
    wide_table = wide_table %>% pivot_longer(cols = any_of(Cols)) %>% 
      pivot_wider(names_from = opcFila,values_from = value,id_cols = c(opcG01,name)) %>% 
      rename('opcFila' = 'name')
  }
  
  #### Ordena columnas
  if(!is.null(ordenCols)) {
    Cols = colnames(wide_table)[(which(colnames(wide_table) == 'opcFila')+1):ncol(wide_table)]
    CorsOrd = c(colnames(wide_table)[!colnames(wide_table) %in% Cols],Cols[ordenCols])
    wide_table = wide_table[,CorsOrd]
  }
  
  ####### Aplica el filtro en caso que sea necesario
  if(!is.null(filtroKeep)) {
    wide_table = wide_table %>% filter(eval(parse(text = filtroKeep)))
  }

return(wide_table)
}
