loadDataECH = function(xx,fileFix,varsSacar,cczMuni) {
  

    if(xx <= 2019) {
      if(xx != 2009) {
        load(paste0(fileFix,xx,'/Bases Procesadas/Bases',xx,'_Uruguay.RData'))
      } else {
        load(paste0(fileFix,xx,'/Bases Procesadas/Bases2009_MC_SSZ.RData'))
      }
      
      varsKeep =  c("SECC","SEGM","ZONA","SSZ","ZONA_LEGAL","CCZ","MUNICIPIO")
      if(xx == 2009) varsKeep = c("pobre2006","indi2006",varsKeep)
      
      cNamesPH = colnames(basePH)
      sacarPosPH = unique(c(cNamesPH[which(cNamesPH == "tipohog"):length(cNamesPH)],varsSacar))
      sacarPosPH = sacarPosPH[!sacarPosPH %in% varsKeep]
      
      cNamesH = colnames(baseH)
      sacarPosH = unique(c(cNamesH[which(cNamesH == "linea"):length(cNamesH)],varsSacar))
      sacarPosH = sacarPosH[!sacarPosH %in% varsKeep]
      
      baseH = baseH %>% select(-one_of(sacarPosH)) %>% suppressWarnings()
      basePH = basePH %>% select(-one_of(sacarPosPH)) %>% suppressWarnings()
      
      colnames(baseH)[grep('INDIG|indi2006',colnames(baseH),ignore.case = T)] = 'Indigente'
      colnames(baseH)[grep('POBRE',colnames(baseH),ignore.case = T)] = 'Pobre'
      colnames(baseH)[grep('PESOANO',colnames(baseH),ignore.case = T)] = 'w'
      
      colnames(basePH)[grep('INDIG|indi2006',colnames(basePH),ignore.case = T)] = 'Indigente'
      colnames(basePH)[grep('POBRE',colnames(basePH),ignore.case = T)] = 'Pobre'
      colnames(basePH)[grep('PESOANO',colnames(basePH),ignore.case = T)] = 'w'
      
      #################################################
      ###### Arma Base PH Y H
      colsJoin = c('CORRELATIV',colnames(baseH)[!colnames(baseH) %in% colnames(basePH)])
      datos = basePH %>% left_join(baseH %>% select(one_of(colsJoin))) %>%
        suppressMessages()
      
      colnames(datos) = tolower(colnames(datos))
      
      
      res = agregaVariables(datos = datos,
                            varJoin = 'correlativ',
                            anio = xx,
                            semestre = 1,
                            perEst = 1,
                            varPeso = 'w',
                            cczMuni = cczMuni)
      
      
    }
    ####################################################
    ####### ECH 2020
    
    if(xx == 2020) {
      load(paste0(fileFix,xx,'/Bases Originales/HyP_2020_Terceros.RData'))
      
      datos = f
      
      
      colnames(datos)[grep('indigente_06',colnames(datos),ignore.case = T)] = 'Indigente'
      colnames(datos)[grep('pobre_06',colnames(datos),ignore.case = T)] = 'Pobre'
      
      colnames(datos) = tolower(colnames(datos))
      
      res = agregaVariables(datos,
                            varJoin = 'numero',
                            anio = xx,
                            semestre = 1,
                            perEst = 3,
                            varPeso = 'pesomen',
                            cczMuni = cczMuni)
    }
    
    ####################################################
    ####### ECH 2021 0 MAS
    
    if(xx >= 20211) {
      xxAux = as.numeric(substr(xx,1,4))
      semestre = as.numeric(substr(xx,5,5))
      load(paste0(fileFix,xxAux,'/dat',xx,'S.RData'))
      
      datos = base
      
      
      colnames(datos)[grep('indig',colnames(datos),ignore.case = T)] = 'Indigente'
      colnames(datos)[grep('pobre',colnames(datos),ignore.case = T)] = 'Pobre'
      
      colnames(datos) = tolower(colnames(datos))
      
      
      # if(semestre == 2 & xxAux == 2021) {
      #   datos$id
      # }
      varJoin = 'id'
      varPeso = 'w_sem'
      perEst = 2
      if(semestre == 1 & xxAux == 2021) {
        varJoin = 'numero'
        varPeso = 'pesomen'
        perEst = 3
      }
      
      res = agregaVariables(datos,
                            varJoin = varJoin,
                            anio = xxAux,
                            semestre = semestre,
                            perEst = perEst,
                            varPeso = varPeso,
                            cczMuni = cczMuni)
    }
    
    
    print(xx)
    
    list(nomVHPH = colnames(res[[1]]),
        datos = res[[1]],
        dfChequeo = res[[2]])

}



