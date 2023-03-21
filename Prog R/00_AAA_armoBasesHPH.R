rm(list = ls())



library(tidyverse)
library(survey)
library(laeken)
library(dplyr)

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
##### Anios
anios = c(2009:2020,20211,20212)


varsSacar = c("linea","linea_v_1","indi","indi_v_1","li","lp","linea2006","lineapc",
              "pobre_v_2006","Group.1","pobre","PPorHab_R","pobre2006","indi2006",
              "pobre_v_1","SECC_num","SEGM_num","ZONA_num")

# fileFix = '../../DARIO/IM/TODO ECH Leticia/Bases ECH/Bases '
fileFix = 'D:/DARIO/IM/TODO ECH Leticia/Bases ECH/Bases '

###############################
##### para pegar los municipios
load('cczMuni.Rdata')

###########################################
## Proceso las bases
#####################################################

system.time({
  basesHPH_map = anios %>% map(function(xx) loadDataECH(xx,fileFix,varsSacar,cczMuni))
})

names(basesHPH_map) = anios
#####################################################
#####################################################
## Captura nombre de las bases
listNomHPH = lapply(basesHPH_map,function(xx) xx$nomVHPH)
#### TOdas las variables
allVarsHPH = unique(do.call(c,listNomHPH))
allVarsHPH = allVarsHPH[order(allVarsHPH)]

### Arma base con todas las variable posibles
basesAllHPH = lapply(basesHPH_map,function(xx) {
  
  aux = xx$datos
  dfAux = data.frame(matrix(NA,nrow = nrow(aux),ncol = length(allVarsHPH)))
  colnames(dfAux) = allVarsHPH
  
  dfAux[,colnames(aux)] = aux
  dfAux
})

names(basesAllHPH) = anios

###################################################
###################################################
###### Arma Data frame con varibles por anio (para saber cuales son compartidas
dfVarAnios = data.frame(matrix(NA,length(allVarsHPH),length(anios) + 1))
colnames(dfVarAnios) = c('codVar',anios)
rownames(dfVarAnios) = allVarsHPH
dfVarAnios[,'codVar'] = allVarsHPH

for(ii in names(basesAllHPH)) {
  aux = basesAllHPH[[ii]]
  indEstan = round(100*colSums(is.na(aux))/nrow(aux))
  dfVarAnios[names(indEstan),ii] = indEstan
}

####################################
#### Uno todas las bases
basesHPH_red = lapply(basesAllHPH, function(xx) {
  xx[,grep('aa_',colnames(xx))]
})

#############################
#### Base reducida larga
datos_red = as_tibble(data.table::rbindlist(basesHPH_red))
colnames(datos_red) = gsub('aa_','',colnames(datos_red))
#### Base para montevideo
datos_red_Mont = datos_red %>%  filter(dpto == 1)

#####################################################
######## GUardo las bases procesadas

save(basesHPH_map,file = 'BasesR/00_AAA_basesHPH_map.Rdata')
save(basesAllHPH,file = 'BasesR/00_AAA_basesAllHPH.Rdata')
save(datos_red,file = 'BasesR/00_AAA_datos_red.Rdata')
save(datos_red_Mont,file = 'BasesR/00_AAA_datos_red_Mont.Rdata')

