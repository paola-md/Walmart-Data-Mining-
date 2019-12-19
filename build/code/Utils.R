  rm(list = ls())

instalar <- function(paquete) {
  
  if (!require(paquete,character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)) {
    install.packages(as.character(paquete), dependecies = TRUE, repos = "http://cran.us.r-project.org")
    library(paquete, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
  }
}

paquetes <- c('lubridate', 'magrittr', 'ggvis', 'dplyr', 'tidyr', 'readr', 'rvest',
              'ggplot2', 'stringr', 'ggthemes', 'googleVis', 'shiny', 'tibble', 'vcd', 'vcdExtra',
              'GGally', 'readODS', 'readxl', "RSQLite", "gridExtra", "GDAdata", "UsingR", "MASS", "ash",
              "ggplot2movies", "effects", "likert", "waffle", "extrafont", "funModeling")


lapply(paquetes, instalar)

########################################################
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
########################################################
# Carga

load_train <- function(){
  if(!file.exists('walmart.rds')){
    data <- read_csv("./../data/train.csv")
    saveRDS(data, "./../data/walmart.rds")
    print('walmart.rds se bajó y guardó\n')
  }
  else{
    warning('walmart.rds ya existe\n')
    data <- readRDS("./../data/walmart.rds")
  }
  
  return(data)
}

load_test <- function(){

  if(!file.exists('walmart_test.rds')){
    data <- read_csv("./../data/test.csv")
    saveRDS(data, "./../data/walmart_test.rds")
    print('walmart_test.rds se bajó y guardó\n')
  }
  else{
    warning('walmart_test.rds ya existe\n')
    data <- readRDS("./../data/walmart_test.rds")
  }
  
  return(data)
}


## PREPARE

string_to_number <- function(x){
  tolower(x) %>% 
  str_replace_all("two","2") %>%
  str_replace_all("three","3") %>%
  str_replace_all("four","4") %>%
  str_replace_all("five","5") %>%
  str_replace_all("six","6") %>%
  str_replace_all("eight","8") %>%
  str_replace_all("twelve","12") %>%
  str_replace_all("98|70|234|156|122|108","NA") 
  #as.numeric()
}


## CLEAN

imports85_clean_colnames <- function(x){
  str_replace_all(tolower(x),"/|-",'_')
}


recorre_errores <- function(df) {
 
  a<- df[grepl("dohc", df$curb_weight), ]
  b <- df[grepl("ohcv", df$curb_weight), ]
  c <- df[grepl("dohcv", df$curb_weight), ]
  d <- df[grepl("ohcf", df$curb_weight), ]
  e <- df[grepl("ohc", df$curb_weight), ]
  f <- df[grepl("l", df$curb_weight), ]
  g <- df[grepl("rotor", df$curb_weight), ]
  
  row_corridas <- rbind(a,b,c,d,e,f,g)
  
  row_corridas$aux <- substr(row_corridas$curb_weight,5,9)
  row_corridas$curb_weight <- substr(row_corridas$curb_weight,1,4)
  

  row_corridas[,c(16:26)] <- row_corridas[,c(15:25)]
  row_corridas$engine_type <- row_corridas$aux

  
  correccion <- row_corridas[,c(1:26)]
  
  sin_errores <- df[-as.numeric(rownames(row_corridas)), ]
  df <- rbind(sin_errores, correccion)
  df
  
}

rechaza_na_rows_guarda <- function(df, cutoff){
  #Calcula porcentaje
  df %>% 
    mutate(
      missing = rowSums(is.na(.))/length(.)
    ) %>% 
    filter(missing >cutoff)

}

rechaza_na_rows_borra <- function(df, cutoff){
  #Calcula porcentaje
  df %>% 
    mutate(
      missing = rowSums(is.na(.))/length(.)
    ) %>% 
    filter(missing <cutoff) ##filtra
}


indica_missing<- function(df){
  #Guardamos los valores que
  columnas_missings <- df[sapply(df, function(x) any(is.na(x)))]

  columnas_missings$num_of_doors <- as.numeric(as.character(columnas_missings$num_of_doors))

  
  dummy_missings <- columnas_missings %>% 
    mutate_all(
      ~replace(., !is.na(.), 0)
    ) %>% 
    mutate_all(
      ~replace(., is.na(.), 1)
    )
  
  colnames(dummy_missings) <- c("miss_normalized_losses", "miss_num_doors", "miss_bore", "miss_stroke", "miss_price")
  
  df <- df %>% bind_cols(dummy_missings, id=NULL)
  df
}

########################################################

#Missings
#####
fill_na_corr <- function(prueba) {
  prueba_num <- prueba %>% 
    select_if(is.numeric) 
  
  vars_missing <- colnames(prueba_num)[colSums(is.na(prueba_num)) > 0]
  
  tam <- length(vars_missing)
  for (i in 1:tam){
    var_actual <- vars_missing[i]
    
    var_corr <- cor(as.matrix(prueba_num)) %>%
      as.data.frame() %>%
      mutate(var1 = rownames(.)) %>%
      gather(var2, value, -var1) %>%
      arrange(desc(value)) %>% 
      filter(var1!=var2, var1 == var_actual) %>% 
      select(var2) %>% 
      filter(row_number()==1) %>% 
      .[[1]]
    
    #prueba_num <- prueba_num[complete.cases(prueba_num), ]
    modelo <- lm(get(var_actual) ~ get(var_corr), data=prueba)
    nueva_var <- predict(modelo, prueba)
    prueba[["nueva"]] <- nueva_var
    prueba$nueva[!is.na(prueba[[var_actual]])] = prueba[[var_actual]][!is.na(prueba[[var_actual]])]
    
    prueba[[var_actual]] = prueba$nueva
    prueba <- prueba %>% select(-nueva)
    
  }
  
  prueba
}

fill_na_vecinos <- function(prueba,k) {
  prueba_num <- prueba %>% 
    select_if(is.numeric)
  
  row_missing <- prueba_num[rowSums(is.na(prueba_num)) > 0,]
  row_completas <- prueba_num[rowSums(is.na(prueba_num)) == 0,]
  tam_row <- nrow(row_missing)
  
  for (row in 1:tam_row){
    indice <- row 
    interes <- row_missing[indice ,]
    
    tam <- nrow(prueba_num)
    prueba_dist <- prueba_num
    for (i in 1:tam){
      prueba_dist$distancia[i] <- dist(rbind(interes, prueba_num[i,]))
    }
    
    kvecinos <- prueba_dist %>% 
      filter(distancia >0) %>%
      arrange(desc(distancia)) %>% 
      filter(row_number()<k + 1) %>% 
      summarise_all(
        ~mean(., na.rm=TRUE)
      )
    
    vars_missing <- colnames(interes)[colSums(is.na(interes)) > 0]
    
    tam <- length(vars_missing)
    for (i in 1:tam){
      interes[vars_missing[i]] <-  kvecinos[vars_missing[i]]
    }
    
    row_completas <- rbind(interes, row_completas)
  }
  
  row_completas
}

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


rellena_na <- function(df, tipo, valor = 0){
  if (tipo == "valor"){
    df_clean <- df %>% mutate_if(
      is.numeric,
      ~ replace(.,is.na(.),valor)
    )
  }
  if (tipo == "medidas"){
    df_clean <-df %>% mutate_if(
      is.numeric, ~ replace(.,is.na(.),mean(., na.rm=TRUE))
    )  %>% mutate_if(
      is.factor,       ~ replace(.,is.na(.),Mode(.))
    ) %>% 
      mutate_if(
        is.integer, ~replace(., is.na(.),median(., na.rm=TRUE))
      )%>% 
      mutate_if(
        is.character, ~replace(., is.na(.),Mode(.))
      )
  }
  if (tipo == "correlacion"){
    df_clean <-fill_na_corr(df) 
  }
  if (tipo == "vecinos"){
    df_clean <-fill_na_vecinos(df)
  }
  df_clean 
  
}




#################################################################
#Grafica
#################################################################


crea_grafica_base <- function(df, lista_vars) {
  numeric_vars <- c()
  factor_vars <- c()
  j <- 1
  k <- 1
  for (i in 1:length(lista_vars)){
    vari <- lista_vars[i]
    #Both factors
    variable_interes <- eval(substitute(vari), df)
    if(is.numeric(df[[variable_interes]])){
      numeric_vars <- append(numeric_vars, vari)
    }
    else{
      factor_vars <- append(factor_vars, vari)
      
    }
  }
  print(numeric_vars)
  print(factor_vars)
  empieza_cat <- 1
  
  if(length(lista_vars)==1){
    if (length(numeric_vars)==1){
      var1 <- numeric_vars[1]
      variable_interes1 <- eval(substitute(var1), df)
      hist(df[variable_interes1],  col = 'darkgray', border = 'white')
    }
    else{
      var1 <- factor_vars[1]
      variable_interes1 <- eval(substitute(var1), df)
      
      counts <- table(df[variable_interes1])
      barplot(counts, horiz=TRUE)
    }
  }
}


crea_univariado <- function(df, lista_vars){
  #Checar si es discreta o continua
  variable_interes <- eval(substitute(lista_vars), df)
  print(variable_interes)
  if(is.integer(df[[variable_interes]])){
    num_classes <- length(unique(df[[variable_interes]]))
    print(num_classes)
    if (num_classes<10){
      nueva_grafica <- ggplot(df, aes(x=.data[[lista_vars]])) +
        geom_histogram()
    }
    else 
      nueva_grafica <- ggplot(df, aes(x=.data[[lista_vars]])) +
        geom_density()
    }
  
    else if (is.numeric(df[[variable_interes]])){
      print("Es numerica")
      nueva_grafica <- ggplot(df, aes(x=.data[[lista_vars]])) +
        geom_density()
    }
   else{
    print("Es factor o caracter")
    nueva_grafica <- crea_grafica_factor_uni(df,lista_vars)
  }
  nueva_grafica
}

crea_grafica_factor_uni <- function(df, lista_vars){
  print(lista_vars)
  vars_sym <- enquo(lista_vars)
  
  datos <- df %>%
    group_by_at( vars(!!vars_sym)) %>%
    dplyr::summarise(count = n()) %>%
    arrange(desc(count))
  
  nueva_grafica <- ggplot(datos) +  
    geom_bar(aes(x=reorder(.data[[lista_vars]], count),  y = count),
             stat="identity", fill="gray") +
    coord_flip() +
    theme_hc()  
  nueva_grafica
}



crea_bivariado <- function(df, lista_vars){
  
  #Checar si es discreta o continua
  var1 <- lista_vars[1]
  var2 <- lista_vars[2]
  
  #Both factors
  variable_interes1 <- eval(substitute(var1), df)
  variable_interes2 <- eval(substitute(var2), df)
  
  if (!is.numeric(df[[variable_interes1]])){
    if (!is.numeric(df[[variable_interes2]])){
      vars_sym <- enquo(variable_interes1)
      
      nueva_grafica <- ggplot(df) +  
        geom_bar(aes(x=.data[[variable_interes1]]),
                 stat="count", fill="gray") +
        facet_grid(~.data[[variable_interes2]]) +
        theme_hc()  
    }
    else{ #1 es factor, 2 no
      vars_sym <- enquo(variable_interes1)
      
      num_classes <- length(unique(df[[variable_interes2]]))
      print(num_classes)
      
      if (num_classes<5){
        nueva_grafica <- ggplot(df, aes(x = .data[[variable_interes2]])) +  
          geom_density() +
          facet_grid(~.data[[variable_interes1]]) +
          theme_hc()  
      }
      else{
        nueva_grafica <- ggplot(df, aes(x= reorder(.data[[variable_interes1]], -.data[[variable_interes2]], median, na.rm = TRUE), 
                                        y = .data[[variable_interes2]])) +  
          geom_boxplot() +
          theme_hc() 
      }
      
    }
    
  }
  else{
    if (!is.numeric(df[[variable_interes2]])){
      nueva_grafica <- ggplot(df, aes(y = .data[[variable_interes1]],
                                      x= reorder(.data[[variable_interes2]]), -.data[[variable_interes1]], median)) +  
        geom_boxplot() +
        theme_hc()  
 
    }
    else{
      nueva_grafica <- ggplot(df, aes(x = .data[[variable_interes1]],
                                      y = .data[[variable_interes2]])) +  
        geom_point()+
        geom_smooth()
      
    }
    
  }
  
  nueva_grafica
}



crea_grafica <- function(df, lista_vars){
  num_vars <- length(lista_vars)
  if (num_vars == 1){ #Univariado: Histograma
    nueva_grafica <- crea_univariado(df, lista_vars)
  }
  else if (num_vars ==2){
    nueva_grafica <-  crea_bivariado(df, lista_vars)
  }
  else if (num_vars>2){
    nueva_grafica <-  crea_multivariado(df, lista_vars)
  }
  nueva_grafica +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          axis.title = element_text(size=15), axis.text =  element_text(size=9), axis.text.x = element_text(angle=90)) 
}




crea_multivariado <- function(df, lista_vars) {
  numeric_vars <- c()
  factor_vars <- c()
  j <- 1
  k <- 1
  for (i in 1:length(lista_vars)){
    vari <- lista_vars[i]
    variable_interes <- eval(substitute(vari), df)
    if(is.numeric(df[[variable_interes]])){
      numeric_vars <- append(numeric_vars, vari)
    }
    else{
      factor_vars <- append(factor_vars, vari)
      
    }
  }
  
  print(numeric_vars)
  print(factor_vars)
  
  if (length(numeric_vars)==2){
    var1 <- numeric_vars[1]
    var2 <- numeric_vars[2]
    var3 <- factor_vars[1]
    
    variable_interes1 <- eval(substitute(var1), df)
    variable_interes2 <- eval(substitute(var2), df)
    variable_interes3 <- eval(substitute(var3), df)
    
    nueva_grafica <- ggplot(df,  aes(x =.data[[variable_interes1]],
                                     y =.data[[variable_interes2]]))+  
                                      geom_point(aes(color=.data[[variable_interes3]]))+
                                      geom_smooth()
                                      
      
  }
  else if (length(numeric_vars)==1){
    var1 <- numeric_vars[1]
    var2 <- factor_vars[1]
    var3 <- factor_vars[2]
    
    variable_interes1 <- eval(substitute(var1), df)
    variable_interes2 <- eval(substitute(var2), df)
    variable_interes3 <- eval(substitute(var3), df)
    
    if (variable_interes3 == "make"){
      variable_interes2 <- eval(substitute(var3), df)
      variable_interes3 <- eval(substitute(var2), df)
    }
    
    nueva_grafica <- ggplot(df, aes(y = .data[[variable_interes1]],
                                    x= .data[[variable_interes2]])) +  
                            geom_boxplot() +
                            facet_grid(~.data[[variable_interes3]])
                      
  }
  else{
    var1 <- factor_vars[1]
    var2 <- factor_vars[2]
    var3 <- factor_vars[3]
    
    variable_interes1 <- eval(substitute(var1), df)
    variable_interes2 <- eval(substitute(var2), df)
    variable_interes3 <- eval(substitute(var3), df)
    
    nueva_grafica <- ggplot(df, aes(fill=.data[[variable_interes1]], x=.data[[variable_interes2]])) + 
      geom_bar(position="stack")+ 
      facet_grid(~.data[[variable_interes3]])  
  }
  nueva_grafica <- nueva_grafica + theme_hc()
  nueva_grafica
}


