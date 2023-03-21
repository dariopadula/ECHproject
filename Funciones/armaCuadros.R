armaCuadros = function(datEstim, # Estimaciones
                       id_Indicador,
                       varAggr = 'opcG02', # variables para quitar y luego agregar los datos
                       addTotal = FALSE,
                       filtroKeep = NULL # Filtro como string para capturar las estimaciones que se quieran
                       # flipTab = FALSE # Indica si hay que trasponer la tabla
                       ) {
  
  
  long_table = estimAll %>% filter(id_ind %in% c(id_Indicador))
  
  if(!is.null(varAggr)) {
    varsAgrup = colnames(datEstim)[!colnames(datEstim) %in% c('est',varAggr)] 
    long_table = long_table %>% 
      group_by_at(vars(varsAgrup)) %>% 
      summarise(est = mean(est,na.rm = T)) %>% 
      ungroup() %>% 
      suppressWarnings() %>% suppressMessages()
  }
  
  
  varsSelect = c('varInteres','opcG01','opcG02','opcFila','opcCol','est','label')
  
  long_table = long_table %>% 
    select(any_of(varsSelect)) %>% 
    mutate(label = ifelse(is.na(label),varInteres,label))
  
  varsID = c('opcG01', 'opcG02','opcFila')
  wide_table <- long_table %>%
    pivot_wider(names_from = label, values_from = est, 
                id_cols = any_of(varsID))
  
  ####### Agrega la columa de 100 en caso de las distribuciones
  if(addTotal) wide_table = wide_table %>% mutate(Total = 100)
  ####### Aplica el filtro en caso que sea necesario
  if(!is.null(filtroKeep)) {
    wide_table = wide_table %>% filter(eval(parse(text = filtroKeep)))
  }

return(wide_table)
}
