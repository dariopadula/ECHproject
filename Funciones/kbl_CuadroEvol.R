
kbl_CuadroEvol = function(cc,
                          header = NULL,
                          nColGroup = 1,
                          Caption = 'Total de problemas',
                          nFilas = 3) {
  
  
  if(!is.null(nColGroup)) {
    RefGrupos = data.frame(table(cc[,nColGroup]))
    colnames(RefGrupos) = c('Grupo','Tot')
    RefGrupos$finG = cumsum(RefGrupos$Tot)
    RefGrupos$iniG = c(1,RefGrupos$finG[1:(nrow(RefGrupos)-1)] + 1)
    rownames(RefGrupos) = RefGrupos[,'Grupo']
    
    dd = cc[,-nColGroup]
    colnames(dd) = colnames(cc)[-nColGroup]
    
    nGrupos = nrow((unique(cc[,nColGroup])))
    if(is.null(nGrupos)) nGrupos = length((unique(cc[,nColGroup])))
    seqTot = nFilas*(1:nGrupos)
  } else {
    dd = cc
  }
  
  

  align = paste(c('l',rep('r',(ncol(dd)-1))),collapse = '')
  
  ccc = kbl(dd,caption = Caption, format.args = list(decimal.mark = ","),
            booktabs = T, align = align, linesep = '') %>%
    row_spec(c(nrow(cc)), bold = T, color = "black") %>%
    row_spec(0, bold = T, color = "black",font_size = 15)
  
  
  if(!is.null(header)) {
    linaHeader = paste0('ccc %>% add_header_above(c(',header,'), bold = T)')
    ccc = eval(parse(text = linaHeader))
  }
  
    
    if(!is.null(nColGroup)) ccc = ccc %>% row_spec(seqTot, bold = T)

    ccc = ccc %>% kable_classic_2("striped",full_width = T)
  
    
    if(!is.null(nColGroup)) {
      rownames(RefGrupos)  %>%
        purrr::walk(function(df) {
          ccc <<- ccc %>%
            pack_rows(df, RefGrupos[df,'iniG'], RefGrupos[df,'finG'])
        })
    }
    return(ccc)
}
