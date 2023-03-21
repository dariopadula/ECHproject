estimaDist = function(varInteres,
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
  if(length(varsGroup) > 1) {
    disAux$variables$id_group = apply(disAux$variables[,varsGroup],1,function(xx) paste(xx,collapse = '_'))
    id_group = 'id_group'
  } else {
    id_group = varsGroup
  }
  
  
  #################################
  allVarsGroup = c(varFila,varCol,id_group)
  # if(!distFila)  allVarsGroup = c(varCol,varFila,id_group)
  
  formTable = as.formula(paste0('~',paste(allVarsGroup,collapse = '+')))
  
  cc = svytable(formTable,disAux)
  
  nDims = dim(cc)
  nomTabs = attr(cc,'dimnames')$id_group
  
  
  nomRes = c("varInteres","varG01","opcG01","varG02","opcG02","varG03","opcG03",
             "varFila","opcFila","varCol","opcCol","est","tipoBase","tipoVar","funcion","id_ind")
  
  
  
  for(ii in nomTabs) {
    aux = cc[,,ii]
    aux = rbind(aux,colSums(aux))
    auxP = data.frame(round(100*prop.table(aux,1),3))
    
    colsPiv = colnames(auxP)
    
    if(length(varsGroup)>1) {
      opcG = do.call(c,strsplit(ii,'_'))
      
      for(jj in 1:length(opcG)) {
        auxP[,paste0('varG0',jj)] =  varsGroup[jj]
        auxP[,paste0('opcG0',jj)] =  opcG[jj]
      }
      } else {
        auxP[,paste0('varG0',1)] =  varsGroup
        auxP[,paste0('opcG0',1)] =  ii
      }
      
    
    
    
    rownames(auxP)[nrow(auxP)] = 'Total'
    auxP[,'varFila'] = varFila
    auxP[,'opcFila'] = rownames(auxP)
    auxP[,'varCol'] = varCol
    

    long_table = auxP %>%
      pivot_longer(
        cols = starts_with("X"),
        names_to = "opcCol",
        names_prefix = "X",
        values_to = "est",
        values_drop_na = TRUE
      )
    
    
    long_table[,"tipoBase"] = tipoBase
    long_table[,"id_ind"] = id_ind
    long_table[,"varInteres"] = varInteres
    long_table[,"tipoVar"] = tipoVar
    long_table[,"funcion"] = funcion
    
    varsFill = nomRes[!nomRes %in% names(long_table)]
    if(length(varsFill) > 0) for(ff in varsFill) long_table[,ff] = NA
    
    resAux[[length(resAux) + 1]] = long_table[,nomRes]
  }
  
  dfRes = as_tibble(data.table::rbindlist(resAux))

  
  return(dfRes)  
}
