rm(list = ls())



library(tidyverse)
library(survey)
library(laeken)

library(ggplot2)
library(plotly)

library(data.table)
library(parallel)
########################################
##### funciones
fun = dir('Funciones/')
fun = fun[grep('.R',fun,ignore.case = T)]
for(ii in fun) source(paste0('Funciones/',ii))
########################################
##### Carga los datos de montevideo
load('BasesR/00_AAA_datos_red_Mont.Rdata')

##### Palabra clave
palClave = 'Municipios'
#### Carga los indicadores y sus caracteristicas
refInd = read.table(paste0('Indicadores/ind_',palClave,'.csv'),sep = ';',header = T)
#### Etiquetas
etiq = read.table('Etiquetas/etiquetas.csv',sep = ';',header = T,encoding = 'latin1')
##### Diseno 
disImp = svydesign(id=~1, weights =~w, data = datos_red_Mont)

#########################################
######## Estima en paralelo

funPar = function(ii) {
  
  varInteres = refInd[ii,'varInteres']
  filtro = refInd[ii,'filtro']
  varsGroup = refInd[ii,'varsGroup']
  varFila = refInd[ii,'varFila']
  varCol = refInd[ii,'varCol']
  tipoBase = refInd[ii,'tipoBase']
  tipoVar = refInd[ii,'tipoVar']
  funcion = refInd[ii,'funcion']
  id_ind = refInd[ii,'id_ind']
  
  if(funcion != 'dist') {
    rr = estimaRatios_V2(varInteres,
                         filtro,
                         disenio = disImp,
                         varsGroup,
                         varFila,
                         varCol,
                         tipoBase,
                         tipoVar,
                         funcion,
                         id_ind)  
  } else {
    rr = estimaDist(varInteres,
                         filtro,
                         disenio = disImp,
                         varsGroup,
                         varFila,
                         varCol,
                         tipoBase,
                         tipoVar,
                         funcion,
                         id_ind)
  }
  
  return(rr)
}

trials = 1:nrow(refInd)

cl <- makeCluster(detectCores())
clusterExport(cl, c("refInd","disImp","estimaRatios_V2","estimaDist"))
clusterEvalQ(cl, {
  library(tidyverse)
  library(survey)})

tini = Sys.time()
system.time({
  results <- parallel::parLapply(cl,trials,funPar)
})


stopCluster(cl)

tfin = Sys.time()
difftime(tfin,tini,units = 'secs')

###### TOdas los indicadores juntos
estimAll = as_tibble(data.table::rbindlist(results))

##########################################
####### Guarda las estimaciones
save(estimAll,file = paste0('Resultados/01_AAA_resultsAll_',palClave,'.RData'))

### Pega etiquetas
estimAll = estimAll %>% left_join(etiq %>% 
                                    mutate(opcVar = as.character(opcVar)) %>% select(-desc), by = c('varCol' = 'varNom','opcCol' = 'opcVar' ))






integraCuadros(datEstim = estimAll, 
               id_Indicador = c(6,7,5),
               varAggr = 'opcG02',
               addTotal = FALSE,
               filtroKeep = 'opcG01 == 2021',
               varsJoin = c('opcG01','opcFila'),
               accionInt = 'cbind') 


integraCuadros(datEstim = estimAll, 
               id_Indicador = c(26:27),
               varAggr = 'opcG02',
               addTotal = FALSE,
               filtroKeep = 'opcG01 == 2019',
               varsJoin = c('opcG01','opcFila'),
               accionInt = '') 

#### cuadro

long_table = estimAll %>% filter(opcG01 ==  2019 & id_ind %in% c(1)) %>% 
  select(varInteres,opcG01,opcG02,opcFila,opcCol,est)

wide_table <- long_table %>%
  pivot_wider(names_from = opcCol, values_from = est, 
              id_cols = c(opcG01, opcG02,opcFila))


wide_table %>% filter(opcG01 == 2020 & opcG02 %in% c(1))
wide_table %>% filter(opcG02 %in% c(1))


#########################################
######




long_table = rr %>% select(opcG01,opcG02,opcFila,opcCol,est)

wide_table <- long_table %>%
  pivot_wider(names_from = opcCol, values_from = est, 
              id_cols = c(opcG01, opcG02,opcFila))


wide_table %>% filter(opcG01 == 2019 & opcG02 %in% c(1))


wide_table %>% filter(opcG01 == 2021 & opcG02 %in% c(2))

