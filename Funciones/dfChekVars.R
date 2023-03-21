dfChekVars = function(varsRef,ColNames,new_cols,ID_bloque,datos) {
  
  vFaltan = varsRef[!varsRef %in% colnames(datos)]
  
  ind = sum(ColNames %in% varsRef) == length(varsRef)
  Creada = 'No'
  if(ind)  Creada = 'Si'

    df = data.frame(VarCreada = new_cols,
                  Creada = Creada,
                  varBloque = paste(varsRef,collapse = ', '),
                  varsFaltan = paste(vFaltan,collapse = ', '),
                  idbloque = ID_bloque + 1)
  
  list(df,ind)
}
