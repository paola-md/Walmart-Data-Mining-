

distrib <- function(x){
  tablita <- table(x)
  df <- cbind(tablita,round(prop.table(tablita)*100,4))
  colnames(df) <- c('Frecuencia','Porcentaje')
  df
}

distrib_val <- function(x,n){
  do.call(rbind,lapply(x[n],distrib))
}

distrib_val(train,5)

