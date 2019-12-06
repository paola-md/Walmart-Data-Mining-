#Clean
colnames(imports85_data) <- imports85_clean_colnames(imports85_colnames)

#Recorre las columnas incorrectas
imports85_data <- recorre_errores(imports85_data)

#Cambia palabras númericas por números
imports85_data  <- imports85_data %>% 
  mutate_at(c(6,16),funs(string_to_number(.)))

#Codifica las columas de factores
imports85_data  <- imports85_data %>% 
  mutate_at(c(3,4,5, 7,8,9,15,16,18),funs(as.factor))

#Codifica las columas numericas
imports85_data  <- imports85_data %>% 
  mutate_at(c(1,2,6,10,11,12,13,14,17,19,20,21,22,23,24,25,26),funs(as.numeric))

#Quita filas con más de 10% de na
imports85_data_borrados <- rechaza_na_rows_guarda(imports85_data, 0.1)
imports85_data <- rechaza_na_rows_borra(imports85_data, 0.1)

#Rellena valores faltantes
imports85_data <- indica_missing(imports85_data)
imports85_data <- rellena_na(imports85_data, "correlacion")