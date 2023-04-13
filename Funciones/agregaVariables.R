agregaVariables = function(datos, 
                           varJoin = 'correlativ',
                           anio,
                           semestre,
                           perEst = 1,
                           varPeso = 'PESOANO',
                           cczMuni) {
  
    #########################################
    #########################################
    # varJoin: variable que identifica al hogar (correlativ)
    # anio: anio de la encuesta
    # semestre: para las encuestas a partir del 2021 que viene por semestre 1 vale para anual o semestre 1
    # perEst: indica como hacer la estimación:
      # 1 = por anio
      # 2 = por semestre
      # 3 = por mes
      # Imputamos una variable que indica que tomar como referencia para estimar y luego promediar
    # varPeso: nombre de la variable que se usa como peso para ponderar.
    #########################################
    #########################################
    
    ##### Transformo las variables de diseno
    
    datos[,'aa_semestre'] = semestre
    datos[,'aa_indBase'] = paste(anio,semestre,sep = '_')
    datos[,'correlativ'] = as.numeric(gsub(' ','',as.character(as.numeric(datos[,varJoin]))))
    datos[,'aa_correlativ'] = paste(anio,semestre,datos[,'correlativ'],sep = '_')
    datos[,'aa_w'] = datos[,varPeso]
    
    datos[,'aa_perEst'] = 1
    datos[,'aa_anio'] = anio
    if(perEst == 2) datos[,'aa_perEst'] = as.numeric(as.character(semestre))
    if(perEst == 3) datos[,'aa_perEst'] = as.numeric(as.character(datos$mes))
    
    
    
    
   ############################################
   #### Homogeiniza las variables de pobreza e indigencia
    
    ###################################
    ### LISTA DE DATA FRAME DE CONSTRUCCION DE VARIABLES
    
    dfChek = list()
    ColNames = colnames(datos)
    
    ### Para guardar todas las variables nuevas
    new_cols_all = character()
    
    ##### Homogeiniza variables
    varsKeepsTransf = c('dpto','mes')
    
    for(ii in varsKeepsTransf[varsKeepsTransf %in% ColNames]) {
      nomAux = paste0('aa_',ii)
      datos[,nomAux] = as.numeric(as.character(datos[,ii]))
    }
    
    
    varsKeeps = c('nper','e26','e27','e30','pobpcoac','pobre','indigente','ht11','ht13','ht19')
    
    for(ii in varsKeeps[varsKeeps %in% ColNames]) {
      nomAux = paste0('aa_',ii)
      datos[,nomAux] = as.numeric(as.character(datos[,ii]))
    }

    ## zona legal
    if(!'zona_legal' %in% ColNames) {
      datos[,'aa_ccz'] = as.numeric(gsub('CCZ|ccz','',datos[,'ccz'])) 
    } else {
      datos[,'aa_ccz'] = as.numeric(gsub('CCZ|ccz','',datos[,'zona_legal'])) 
    }
    
    
    ## Municipio
    if(!'municipio' %in% ColNames) {
      datos = datos %>% left_join(cczMuni) %>% suppressMessages()
      datos[,'aa_municipio'] = datos[,'municipio']
    } else {
      datos[,'aa_municipio'] = datos[,'municipio']
    }
    
    
    ## Barrio
    
    refCod = intersect(c("barrio","codbarrio"),ColNames)
    datos[,'aa_codbarrio'] = NA
    if(length(refCod) > 0) {
      datos[,'aa_codbarrio'] = datos[,refCod]
    }
    
    refCod = intersect(c("nombarrio","nombrebarr"),ColNames)
    datos[,'aa_nombarrio'] = NA
    if(length(refCod) > 0) {
      datos[,'aa_nombarrio'] = datos[,refCod]
    }
    
    #############################################################
    #############################################################
    #############################################################
    
    ###### Tipo de hogar
    
    varsRef = paste0('e30')
    new_cols = c('tipohog','tipoh','tipoh4')
    new_cols_all = c(new_cols_all,new_cols)
    
    chk = dfChekVars(varsRef,ColNames,new_cols,length(dfChek),datos)
    dfChek[[length(dfChek) + 1]] = chk[[1]]
    ind = chk[[2]]
    
    if(ind) {
      dd = datos %>% select(correlativ,e30) %>%
        mutate(e30 = as.character(e30)) %>%
        fastDummies::dummy_cols(.,select_columns = 'e30') %>%
        select(-e30) %>%
        group_by(correlativ) %>%
        summarise_all(.,max) %>% 
        ungroup() %>% 
        mutate(espos = e30_2,
               hijot = e30_3 + e30_4 + e30_5,
               otfam = e30_6 + e30_7+ e30_8 + e30_9 + e30_10 + e30_11 + e30_12,
               nopar = e30_13,
               tipohog = ifelse(espos == 0 & hijot == 0 & otfam == 0 & nopar == 0,1,
                          ifelse(espos == 0 & hijot == 0 & otfam>= 1 & nopar == 0,2,
                           ifelse(espos == 0 & hijot == 0 & otfam == 0 & nopar>= 1,3,
                            ifelse(espos == 0 & hijot == 0 & otfam>= 1 & nopar>= 1,4,
                             ifelse(espos == 0 & hijot>= 1 & otfam == 0 & nopar == 0,5,
                              ifelse(espos == 0 & hijot>= 1 & otfam>= 1 & nopar == 0,6,
                               ifelse(espos == 0 & hijot>= 1 & otfam == 0 & nopar>= 1,7,
                                ifelse(espos == 0 & hijot>= 1 & otfam>= 1 & nopar>= 1,8,
                                 ifelse(espos>= 1 & hijot == 0 & otfam == 0 & nopar == 0,9,
                                  ifelse(espos>= 1 & hijot == 0 & otfam>= 1 & nopar == 0,10,
                                    ifelse(espos>= 1 & hijot == 0 & otfam == 0 & nopar>= 1,11,
                                      ifelse(espos>= 1 & hijot == 0 & otfam>= 1 & nopar>= 1,12,
                                        ifelse(espos>= 1 & hijot>= 1 & otfam == 0 & nopar == 0,13,
                                          ifelse(espos>= 1 & hijot>= 1 & otfam>= 1 & nopar == 0,14,
                                            ifelse(espos>= 1 & hijot>= 1 & otfam == 0 & nopar>= 1,15,
                                              ifelse(espos>= 1 & hijot>= 1 & otfam>= 1 & nopar>= 1,16,99)))))))))))))))),
               tipoh =  ifelse(tipohog==1,1,
                        ifelse(tipohog>=2&tipohog<=4,2,
                        ifelse(tipohog==5,3,
                        ifelse(tipohog>=6&tipohog<=8,4,
                        ifelse(tipohog==9,5,
                        ifelse(tipohog>=10&tipohog<=12,6,
                        ifelse(tipohog==13,7,
                        ifelse(tipohog>=14&tipohog<=16,8,0)))))))),
               
               tipoh4 = ifelse(tipoh>=1&tipoh<=2,1,
                        ifelse(tipoh>=3&tipoh<=4,2,
                        ifelse(tipoh>=5&tipoh<=6,3,
                        ifelse(tipoh>=7&tipoh<=8,4,0)))))
      
      ############################################
      datos = datos %>% left_join(dd %>% select(correlativ,tipohog,tipoh,tipoh4)) %>%
        suppressMessages()
    } else {
      datos <- cbind(datos, setNames(replicate(length(new_cols), NA, simplify = FALSE), new_cols))
  
    }
    
  
    
  ############################################
  ###### DERECHO DE SALUD
  
    new_cols = c('SPubl','SPriv','SPublSM','SeguroPP',
                 'OtroSalud','EM','SoloEM',
                 'SinCob','Cobertura')
    new_cols_all = c(new_cols_all,new_cols)
    
    if(anio >= 2021) {
      
      varsRef = c('e45_cv','e46_cv')
      
      chk = dfChekVars(varsRef,ColNames,new_cols,length(dfChek),datos)
      dfChek[[length(dfChek) + 1]] = chk[[1]]
      ind = chk[[2]]
      
      
      if(ind) {
      datos = datos %>% 
        mutate(SPubl = ifelse(e45_cv %in% c(1,4:6),1,0),
               SPriv = ifelse(e45_cv %in% c(2),1,0),
               SPublSM = ifelse(SPubl== 1 & SPriv == 0,1,0),
               SeguroPP = ifelse(e45_cv %in% c(3),1,0),
               OtroSalud = NA,
               EM = ifelse(e46_cv == 1,1,0),
               SoloEM = ifelse((SPubl + SPriv + SeguroPP) == 0 & EM == 1,1,0),
               SinCob = ifelse(e45_cv %in% c(7),1,0),
               Cobertura =  ifelse(SinCob==1,1,
                              ifelse(SoloEM==1,2,
                                 ifelse(SPubl==1,3,
                                   ifelse(SPriv==1,4,
                                    ifelse(SeguroPP==1,5,9))))))
      }
      } else {
      
      varsRef = c(paste0('e45_',c(1:7)),'e46')
      
      chk = dfChekVars(varsRef,ColNames,new_cols,length(dfChek),datos)
      dfChek[[length(dfChek) + 1]] = chk[[1]]
      ind = chk[[2]]
      

      if(ind) {
        datos = datos %>% 
          mutate(SPubl = case_when(e45_1 == 1 | e45_4 == 1 | e45_5 == 1 | e45_6 == 1 ~ 1,TRUE ~ 0),
                 SPriv = case_when(e45_2 == 1 ~ 1, TRUE ~ 0),
                 SPublSM = case_when(SPubl== 1 & SPriv == 0 ~ 1, TRUE ~ 0),
                 SeguroPP = case_when(e45_3 == 1~ 1,TRUE ~ 0),
                 OtroSalud = case_when(e45_7 == 1 ~ 1,TRUE ~ 0),
                 EM = case_when(e46 == 1 ~ 1, TRUE ~ 0),
                 SoloEM = ifelse((SPubl + SPriv + SeguroPP) == 0 & EM == 1,1,0),
                 SinCob = case_when((SPubl+SPriv+SeguroPP+OtroSalud+EM) == 0 ~ 1,TRUE ~ 0),
                 Cobertura =  ifelse(SinCob==1,1,
                                     ifelse(SoloEM==1,2,
                                            ifelse(SPubl==1,3,
                                                   ifelse(SPriv==1,4,
                                                          ifelse(SeguroPP==1,5,9))))))
      } else {
        datos <- cbind(datos, setNames(replicate(length(new_cols), NA, simplify = FALSE), new_cols))
        
      }
    }
  
  
  
  ### Tramos de edad
    varsRef = c('e27')
    
    new_cols = c('TramoEdad','TrEdP','Mayor14','menor18','edad_6_15',
                 'Mayor25','edad_3_cat','Menor6','rangoEd','rangoEd2',
                 'rangoEd3','rEdades4','rEdades5','rEdadesINE')
    new_cols_all = c(new_cols_all,new_cols)
    
    chk = dfChekVars(varsRef,ColNames,new_cols,length(dfChek),datos)
    dfChek[[length(dfChek) + 1]] = chk[[1]]
    ind = chk[[2]]
    
    if(ind) {  
    datos = datos %>% mutate(
      TramoEdad = case_when(e27 <= 4 ~ 1,
                            e27 > 4 & e27 <= 12 ~ 2,
                            e27 > 12 & e27 <= 14 ~ 3,
                            e27 > 14 & e27 <= 24 ~ 4,
                            e27 > 24 & e27 <= 59 ~ 5,
                            TRUE ~ 6),
      TrEdP = case_when(e27 <= 4 ~ 1,
                            e27 > 4 & e27 <= 12 ~ 2,
                            e27 > 12 & e27 <= 17 ~ 3,
                            e27 > 17 & e27 <= 59 ~ 4,
                            TRUE ~ 5),
      Mayor14 = case_when(e27 >= 14 ~ 1,
                          TRUE ~ 0),
      menor18 = case_when(e27 < 18 ~ 1,
                          TRUE ~ 0),
      edad_6_15 = case_when(e27 < 16  & e27 > 5 ~ 1,
                            TRUE ~ 0),
      Mayor25 = case_when(e27 >= 25 ~ 1,
                          TRUE ~ 0),
      edad_3_cat = case_when(e27 < 18 ~ 1,
                    e27 >= 18 & e27 < 65 ~ 2,
                    TRUE ~ 3),
      Menor6 = case_when(e27 < 6 ~ 1,
                         TRUE ~ 0),
      rangoEd = case_when(e27 < 5 ~ 1,
                          e27 >= 5 & e27 < 15 ~ 2,
                          e27 >= 15 & e27 < 50 ~ 3,
                          e27 >= 50 & e27 < 60 ~ 4,
                          e27 >= 60 ~ 5,
                          TRUE ~ 0),
      rangoEd2 = case_when(e27 < 15 ~ 1,
                          e27 >= 15 & e27 < 60 ~ 2,
                          e27 >= 60 ~ 3,
                          TRUE ~ 0),
      rangoEd3 = case_when(e27 < 5 ~ 1,
                           e27 >= 5 & e27 < 13 ~ 2,
                           e27 >= 13 & e27 < 19 ~ 3,
                           e27 >= 19 & e27 < 60 ~ 4,
                           e27 >= 60 ~ 5,
                           TRUE ~ 0),
      rEdades4 = case_when(e27 < 5 ~ 1,
                           e27 >= 5 & e27 < 13 ~ 2,
                           e27 >= 13 & e27 < 60 ~ 3,
                           e27 >= 60 ~ 4,
                           TRUE ~ 0),
      rEdades5 = case_when(e27 < 15 ~ 1,
                           e27 >= 15 & e27 < 21 ~ 2,
                           e27 >= 21 & e27 < 30 ~ 3,
                           e27 >= 30 & e27 < 40 ~ 4,
                           e27 >= 40 ~ 5,
                           TRUE ~ 0),
      rEdadesINE = case_when(e27 < 6 ~ 1,
                             e27 >= 6  & e27 < 13 ~ 2,  
                             e27 >= 13  & e27 < 18 ~ 3,
                             e27 >= 18 & e27 < 65 ~ 4,
                             e27 >= 65 ~ 5,
                             TRUE ~ 0))
    } else {
      datos <- cbind(datos, setNames(replicate(length(new_cols), NA, simplify = FALSE), new_cols))
  }
  
  
  #### Habitaciones y personas (baseH)
  
  # PPorHab= personas por habitacion para dormir
  # HabTot = total de habitaciones sin contar bano ni cocina
  # PPorHab_R = Personas por habitaciones residenciales
  # PPorH_D = PPorHab recodificado en 3
  # PPorH_R = PPorHab recodificado en 3
  
  
  
  varsRef = c('ht19','d10','d9')
  
  new_cols = c('PPorHab','HabTot','PPorHab_R','PPorH_D','PPorH_R','hacinamiento')
  new_cols_all = c(new_cols_all,new_cols)
  
  chk = dfChekVars(varsRef,ColNames,new_cols,length(dfChek),datos)
  dfChek[[length(dfChek) + 1]] = chk[[1]]
  ind = chk[[2]]
  
  if(ind) {
    datos = datos %>%  mutate(
      PPorHab = ht19/d10,
      HabTot =  d9 + d10,
      PPorHab_R = ht19/HabTot,
      PPorH_D = case_when(PPorHab < 2 ~ 1,
                          PPorHab >= 2 & PPorHab < 3 ~ 2,
                          PPorHab >= 3 ~ 3,
                          TRUE ~ 0),
      PPorH_R = case_when(PPorHab_R < 2 ~ 1,
                          PPorHab_R >= 2 & PPorHab_R < 3 ~ 2,
                          PPorHab_R >= 3 ~ 3,
                          TRUE ~ 0),
      hacinamiento = case_when(PPorHab_R > 2 ~ 1,TRUE ~ 0)
    )
  } else {
    datos <- cbind(datos, setNames(replicate(length(new_cols), NA, simplify = FALSE), new_cols))
    
  }
  
  
  
  ### Llegada del agua
  varsRef = c('d12')
  
  new_cols = c('llegaAgua')
  new_cols_all = c(new_cols_all,new_cols)
  
  chk = dfChekVars(varsRef,ColNames,new_cols,length(dfChek),datos)
  dfChek[[length(dfChek) + 1]] = chk[[1]]
  ind = chk[[2]]
  
  if(ind) {
    datos = datos %>%  mutate(
          llegaAgua = case_when(d12 == 1 ~ 1,
                                d12 %in% c(2,3) ~ 2,
                                d12 == 4 ~ 3,
                                TRUE ~ 0))
  } else {
    datos <- cbind(datos, setNames(replicate(length(new_cols), NA, simplify = FALSE), new_cols))
  
  }
  
  
  ### Servicio sanitario incompleto
  varsRef = c('d13')
  
  new_cols = c('servSanInco')
  new_cols_all = c(new_cols_all,new_cols)
  
  chk = dfChekVars(varsRef,ColNames,new_cols,length(dfChek),datos)
  dfChek[[length(dfChek) + 1]] = chk[[1]]
  ind = chk[[2]]
  
  if(ind) {
    datos = datos %>%  mutate(
      servSanInco = case_when( d13 %in% c(2,3) ~ 1, TRUE ~ 0))
  } else {
    datos <- cbind(datos, setNames(replicate(length(new_cols), NA, simplify = FALSE), new_cols))
    
  }
  
  ### Vivienda adecuada (baseH)
  
  varsRef = c('c2','c3','c4')
  
  new_cols = c('VivAdec')
  new_cols_all = c(new_cols_all,new_cols)
  
  chk = dfChekVars(varsRef,ColNames,new_cols,length(dfChek),datos)
  dfChek[[length(dfChek) + 1]] = chk[[1]]
  ind = chk[[2]]
  
  if(ind) {
    datos = datos %>%  mutate(
      Paredes = case_when(c2 == 1 ~ 1,
                          c2 %in% c(2,3) ~ 20,
                          c2 %in% c(4:6) ~ 300,
                          TRUE ~ 0),
      Techos = case_when(c3 %in% c(1,2,5) ~ 1,
                          c3 %in% c(3) ~ 20,
                          c3 %in% c(4,6) ~ 300,
                          TRUE ~ 0),
      Pisos = case_when(c4 %in% c(1,2) ~ 1,
                         c4 %in% c(3) ~ 20,
                         c4 %in% c(4:5) ~ 300,
                         TRUE ~ 0),
      sumV = (Pisos+Techos+Paredes),
      VivAdec = case_when(sumV <= 3 ~ 1,
                          sumV > 20 & sumV <= 60 ~ 2,
                          sumV >= 300 ~ 3,
                        TRUE ~ 0)) %>% 
      select(-c(Paredes,Pisos,Techos,sumV))
  } else {
    datos <- cbind(datos, setNames(replicate(length(new_cols), NA, simplify = FALSE), new_cols))
  
  }
    
  ### Condiciones del hogar (baseH)
  varsRef = paste0('c5_',c(1:5))
  
  new_cols = c('Humedad','Grietas','Luz','Derrumbe','NoProblem')
  new_cols_all = c(new_cols_all,new_cols)
  
  chk = dfChekVars(varsRef,ColNames,new_cols,length(dfChek),datos)
  dfChek[[length(dfChek) + 1]] = chk[[1]]
  ind = chk[[2]]
  
  if(ind) {
    datos = datos %>% mutate(
      Humedad = case_when(c5_1 == 1 | c5_2 == 1 | c5_12 == 1 ~ 1,
                          TRUE ~ 0),
      Grietas = case_when(c5_3 == 1 | c5_5 == 1 | c5_6 == 1 | c5_7 == 1 ~ 1,
                          TRUE ~ 0),
      Luz = case_when(c5_4 == 1 | c5_8 == 1 | c5_9 == 1 ~ 1,
                          TRUE ~ 0),
      Derrumbe = case_when(c5_10 == 1 | c5_11 == 1 ~ 1,
                      TRUE ~ 0),
      NoProblem = case_when((Humedad+Grietas+Luz+Derrumbe) == 0 ~ 1,
                           TRUE ~ 0)
      
    )
  } else {
    datos <- cbind(datos, setNames(replicate(length(new_cols), NA, simplify = FALSE), new_cols))
  }
  
  ###### Mujeres jefas de hogar (baseH)
  
  varsRef = c('e30','e26')
  
  new_cols = c('HMJefas','Menor6')
  new_cols_all = c(new_cols_all,new_cols)
  
  chk = dfChekVars(varsRef,ColNames,new_cols,length(dfChek),datos)
  dfChek[[length(dfChek) + 1]] = chk[[1]]
  ind = chk[[2]]
  
  if(ind) {
    datos = datos %>% mutate(
      HMJefas = case_when(e30 == 1 & e26 == 2 ~ 1,
                          TRUE ~ 0))
    
    ##### Identifico hogares con menores de 6 anios
    datos = datos %>% group_by_at(vars(varJoin)) %>% 
      mutate(Menor6 = case_when(sum(Menor6) > 0 ~ 1,
                            TRUE ~ 0)) %>% 
      ungroup()
  } else {
    datos <- cbind(datos, setNames(replicate(length(new_cols), NA, simplify = FALSE), new_cols))
  }
  
  ##### Confort (baseH)
  
  varsRef = paste0('d21_',c(12:18))
  
  new_cols = c('Cable','Fono','Internet','Compu','Lavarropa','Lavavaj','Micro','Auto')
  new_cols_all = c(new_cols_all,new_cols)
  
  chk = dfChekVars(varsRef,ColNames,new_cols,length(dfChek),datos)
  dfChek[[length(dfChek) + 1]] = chk[[1]]
  ind = chk[[2]]
  
  if(ind) {
    datos = datos %>% mutate(
      Cable = case_when(d21_7 == 1 ~ 1,
                          TRUE ~ 0),
      Fono = case_when(d21_17 == 1 ~ 1,
                       TRUE ~ 0),
      Internet = case_when(d21_16 == 1 ~ 1,
                       TRUE ~ 0),
      Compu = case_when(d21_15 == 1 ~ 1,
                           TRUE ~ 0),
      Lavarropa = case_when(d21_10 == 1 ~ 1,
                        TRUE ~ 0),
      Lavavaj = case_when(d21_12 == 1 ~ 1,
                        TRUE ~ 0),
      Micro = case_when(d21_13 == 1 ~ 1,
                        TRUE ~ 0),
      Auto = case_when(d21_18 == 1 ~ 1,
                        TRUE ~ 0))
  } else {
    datos <- cbind(datos, setNames(replicate(length(new_cols), NA, simplify = FALSE), new_cols))
    
  }
  
  ## Tenencia de vivienda (baseH)
  
  varsRef = c('d8_1')
  
  new_cols = c('TenViv')
  new_cols_all = c(new_cols_all,new_cols)
  
  chk = dfChekVars(varsRef,ColNames,new_cols,length(dfChek),datos)
  dfChek[[length(dfChek) + 1]] = chk[[1]]
  ind = chk[[2]]
  
  
  if(ind) {
    datos = datos %>% mutate(
      TenViv = case_when(d8_1 %in% c(1,2) ~ 1,
                         d8_1 %in% c(3,4) ~ 2,
                         d8_1 %in% c(5) ~ 3,
                         d8_1 %in% c(6:8) ~ 4,
                         d8_1 %in% c(9) ~ 5,
                         TRUE ~ 0))
  } else {
    datos <- cbind(datos, setNames(replicate(length(new_cols), NA, simplify = FALSE), new_cols))
  }
  
  
  
  ### Variables de empleo
  varsRef = c('pobpcoac','f73','f72_2','f85','f98','g126_1','g134_1','subempleo')
  
  new_cols = c('ocupados','desocupado','pobEcAc','pEdTrab','categOcup',
               'condActividad','ramaAct','horasTr01','horasTr02','horasTr0T',
               'horasTrabT_Rec','ingSueldoTr01','ingSueldoTr02','ingSueldoTrTot',
               'ingSueldoXhora01','ht11_svl','sinIngreso','subempleo')
  new_cols_all = c(new_cols_all,new_cols)
  
  chk = dfChekVars(varsRef,ColNames,new_cols,length(dfChek),datos)
  dfChek[[length(dfChek) + 1]] = chk[[1]]
  ind = chk[[2]]
  
  if(ind) {
    datos = datos %>% mutate(
           ocupados = as.numeric(pobpcoac == 2),
           desocupado = as.numeric(pobpcoac %in% c(3:5)),
           pobEcAc = as.numeric(pobpcoac %in% c(2:5)),
           pEdTrab = as.numeric(pobpcoac %in% c(2:11)),
           condActividad = as.numeric(case_when(pobpcoac  %in% c(3:5) ~ '3',
                                     pobpcoac  %in% c(6) ~ '4',
                                     pobpcoac  %in% c(7) ~ '5',
                                     pobpcoac  %in% c(8:10) ~ '6',
                                     pobpcoac  %in% c(11) ~ '7',
                                     TRUE ~ as.character(pobpcoac))),
           categOcup = as.numeric(case_when(f73  %in% c(5:6) ~ '5',
                                 f73  %in% c(7:8) ~ as.character(f73 - 1),
                                 TRUE ~ as.character(f73))),
           horasTr01 = as.numeric(case_when(is.na(f85) ~ '0',
                                            TRUE ~ as.character(f85))),
           horasTr02 = as.numeric(case_when(is.na(f98) ~ '0',
                                            TRUE ~ as.character(f98))),
           horasTrabT = horasTr01 + horasTr02,
           horasTrabT_Rec = case_when(horasTrabT < 21 ~ 1,
                                      horasTrabT >= 21 & horasTrabT < 41 ~ 2,
                                      horasTrabT >= 41 & horasTrabT < 61 ~ 3,
                                      horasTrabT >= 61 ~ 4,
                                      TRUE ~ 0),
           ingSueldoTr01 = as.numeric(case_when(is.na(g126_1) ~ '0',
                                                TRUE ~ as.character(g126_1))),
           ingSueldoTr02 = as.numeric(case_when(is.na(g134_1) ~ '0',
                                                TRUE ~ as.character(g134_1))),
           ingSueldoTrTot = ingSueldoTr01 + ingSueldoTr02,
           ramaChar = as.character(f72_2),
           ramaAct = case_when(nchar(ramaChar) == 3 ~ '0',
                               nchar(ramaChar) == 4 ~ substr(ramaChar,1,1)),
           ingSueldoXhora01 = case_when(categOcup %in% c(1,2) & ingSueldoTr01 > 0 ~ ingSueldoTr01/(4*horasTr01),
                                        TRUE ~ 0),
           ht11_svl = ht11 - ht13,
           sinIngreso = as.numeric(ingSueldoTrTot == 0 & f124_1 == 2 & f124_2 == 2 & f124_3 == 2 & (f73 %in% c(0,7))),
           subempleo = as.numeric(subempleo %in% c(1,100)))
    
    if(anio > 20211) {
      datos = datos %>% mutate(categOcup = as.numeric(case_when(f73  %in% c(9) ~ '5',
                                                     f73  %in% c(7:8) ~ as.character(f73 - 1),
                                                     TRUE ~ as.character(f73))))
    }
    
  } else {
    datos <- cbind(datos, setNames(replicate(length(new_cols), NA, simplify = FALSE), new_cols))
  }
  
  
  
  
  ## Derecho de jubilacion
  varsRef = c('f82','f96')
  
  new_cols = c('Der_Jubil')
  new_cols_all = c(new_cols_all,new_cols)
  
  chk = dfChekVars(varsRef,ColNames,new_cols,length(dfChek),datos)
  dfChek[[length(dfChek) + 1]] = chk[[1]]
  ind = chk[[2]]
  
  
  if(ind) {
    datos = datos %>% mutate(
      Der_Jubil = as.numeric(f82 %in% c(1) | f96 == 1))
  } else {
    datos <- cbind(datos, setNames(replicate(length(new_cols), NA, simplify = FALSE), new_cols))
  }
  
  #### Educacion
  varsRef = paste0('e51_',c(8:11))
  
  new_cols = c('Post','TNoUniv','Univ','Magist','EnsTecn','BachTec',
               'BachSec','CicBas','PrimEsp','PrimCom','SiEscuel',
               'SiPresco','asistEdu','TotEduc','TotEducRe','TotEducRe4','primarIncomp','primCompCBinc',
               'CBincomp','BachIncom','SecundCom','TotEducMay12','CPropMen12',
               'NoEsc15_65','NoEdObli','actividad','actividad_4C','clima_edu','años_esp_est',
               'años_rez_est','rezagoEsc','rezago_4Ca')
  
  new_cols_all = c(new_cols_all,new_cols)
  
  chk = dfChekVars(varsRef,ColNames,new_cols,length(dfChek),datos)
  dfChek[[length(dfChek) + 1]] = chk[[1]]
  ind = chk[[2]]
  
  if(ind) {
  
    if(anio <= 2020 | (anio == 2021 & semestre == 1)) {
    
      
        datos = datos %>% mutate(
          Post    = ifelse(e51_11 >= 9,0,e51_11),
          TNoUniv = ifelse(e51_10 >= 9,0,e51_10),
          Univ    = ifelse(e51_9 >= 9,0,e51_9),
          Magist  = ifelse(e51_8 >= 9,0,e51_8),
          EnsTecn = ifelse(e51_7 >= 9,0,e51_7),
          BachTec = ifelse(e51_6 >= 9,0,e51_6),
          BachSec = ifelse(e51_5 >= 9,0,e51_5),
          CicBas  = ifelse(e51_4 >= 9,0,e51_4),
          PrimEsp = ifelse(e51_3 >= 9,0,e51_3),
          PrimCom = ifelse(e51_2 >= 9,0,e51_2))
        
        if(anio <= 2010) {
          datos = datos %>% mutate(
            SiEscuel = case_when(e49 == 1 & (e51_3 + e51_2) > 0 ~ 1,TRUE ~ 0),
            SiPresco = case_when(e49 == 1 & e51_1 > 0 ~ 1,TRUE ~ 0),
            asistEdu = as.numeric(e49 == 1))  
        } else {
          datos = datos %>% mutate(
            SiEscuel = case_when(e197 == 1 ~ 1,TRUE ~ 0),
            SiPresco = case_when(e193 == 1 ~ 1,TRUE ~ 0),
            asistEdu = as.numeric(e193 == 1 | e197 == 1 | e201 == 1 | e212 == 1 | e215 == 1 | e218 == 1 | e221 == 1 | e224 == 1)
            )
        }
        
    
    } else {
      datos = datos %>% mutate(
        Post    = ifelse(e51_11 >= 9,0,e51_11),
        TNoUniv = ifelse(e51_10 >= 9,0,e51_10),
        Univ    = ifelse(e51_9 >= 9,0,e51_9),
        Magist  = ifelse(e51_8 >= 9,0,e51_8),
        EnsTecn = ifelse(e51_6a >= 9,0,e51_6a),
        BachTec = ifelse(e51_6 >= 9,0,e51_6),
        BachSec = ifelse(e51_5 >= 9,0,e51_5),
        CicBasC  = ifelse(e51_4_a >= 9,0,e51_4_a),
        CicBasT  = ifelse(e51_4_b >= 9,0,e51_4_b),
        CicBas = CicBasC + CicBasT,
        PrimEsp = ifelse(e51_3 >= 9,0,e51_3),
        PrimCom = ifelse(e51_2 >= 9,0,e51_2),
        SiEscuel = case_when(e579 %in% c(2,3) ~ 1,TRUE ~ 0),
        SiPresco = case_when(e579 == 1 ~ 1,TRUE ~ 0),
        asistEdu = as.numeric(e49 == 3)
        )
      
      
    }
    
    datos = datos %>% mutate(
      TotEduc = Post + TNoUniv + Univ + Magist + EnsTecn + BachTec + BachSec + CicBas + PrimEsp + PrimCom,
      TotEducRe = case_when(TotEduc < 9 ~ 1,
                            TotEduc >= 9 & TotEduc < 12 ~ 2,
                            TRUE ~ 3),
      TotEducRe4 = case_when(TotEduc < 6 ~ 1,
                            TotEduc >= 6 & TotEduc < 10 ~ 2,
                            TotEduc >= 10 & TotEduc < 13 ~ 3,
                            TRUE ~ 4),
      primarIncomp = as.numeric(TotEduc < 6),
      primCompCBinc = as.numeric(TotEduc >= 6 & TotEduc < 9),
      CBincomp = as.numeric(TotEduc < 9),
      BachIncom = as.numeric(TotEduc < 12),
      SecundCom = as.numeric(TotEduc >= 12),
      TotEducMay12 = as.numeric(TotEduc > 12),
      CPropMen12 = as.numeric(f73 == 5 & TotEduc < 12),
      NoEsc15_65 = case_when(e27 >= 15 & e27 <= 65 & TotEduc < 6 ~ 1, TRUE ~ 0),
      NoEdObli = case_when(TotEduc < 9 ~ 1, TRUE ~ 0),
      actividad = case_when(ocupados == 0 & asistEdu == 1 ~ 1,
                            ocupados == 1 & asistEdu == 0 ~ 2,
                            ocupados == 1 & asistEdu == 1 ~ 3,
                            ocupados == 0 & asistEdu == 0 & pobEcAc == 1 ~ 4,
                            ocupados == 0 & asistEdu == 0 & pobEcAc == 0 ~ 5,
                            TRUE ~ 0),
      actividad_4C = case_when(actividad %in% c(4,5) ~ 4,
                               TRUE ~ actividad))
    
    ##### Clima educativo
    dd = datos %>% mutate(ed_21_65 = as.numeric(e27 >= 21 & e27 <= 65)) %>% 
      group_by_at(vars(varJoin)) %>% 
      mutate(ind21_65 = as.numeric(sum(ed_21_65) > 0)) %>% 
      ungroup() %>% 
      filter(ed_21_65 == 1) %>% 
      group_by_at(vars(varJoin)) %>% 
      summarise(prom_est_21_65 = mean(TotEduc,na.rm = T),
                n = n()) %>% 
      ungroup() %>% 
      mutate(clima_edu = case_when(prom_est_21_65 < 9 ~ 1,
                                   prom_est_21_65 >= 9 & prom_est_21_65 <= 12 ~ 2,
                                   prom_est_21_65 > 12 ~ 3,
                                   TRUE ~ 0))
    
    varsUnir = c(varJoin,'clima_edu')
    datos = datos %>% left_join(dd %>% select(any_of(varsUnir))) %>% 
      suppressMessages()
    
    #### Rezago educativo
    
    datos = datos %>% mutate(
      años_esp_est = ifelse(e27 <= 15 & e27 >= 8,e27 - 7,0),
      años_rez_est = ifelse(e27 <= 15 & e27 >= 8,años_esp_est - TotEduc,0),
      rezagoEsc = case_when(años_rez_est <= 0 ~ 0,TRUE ~ 1),
      rezago_4Ca = case_when(años_rez_est <= 0 ~ 0,
                           años_rez_est <= 1 & años_rez_est > 0 ~ 1,
                           años_rez_est <= 2 & años_rez_est > 1 ~ 2,
                           TRUE ~ 3)
    )
    
  } else {
    
    datos <- cbind(datos, setNames(replicate(length(new_cols), NA, simplify = FALSE), new_cols))
  }
  
  ##################################################
  ####### Renombre las variables creadas
  for(ii in new_cols_all) {
    colnames(datos)[colnames(datos) == ii] = paste0('aa_',ii)
  }
  
  
  #############################################
  #############################################
  #############################################
  list(datos = datos, chequeo = do.call(rbind,dfChek))
  
}


  
