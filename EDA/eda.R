library(tidyverse)
library(magrittr)
library(readr)
library(purrr)
library(arules)

# df <- read_csv('train.csv', col_types = 'ddcdicd') %>% 
#   purrr::set_names(nm = c('visita_tipo','visita_ticket','visita_dia_semana',
#                    'producto_identificador','producto_cantidad',
#                    'producto_departamento','producto_categoria')) %>% 
#   mutate_if(is.character, tolower) %>% 
#   mutate(producto_departamento = ifelse(producto_departamento =='null',NA,
#                                         producto_departamento)) %>% 
#   mutate_at(vars(visita_tipo:producto_identificador,
#               producto_departamento,producto_categoria),
#          function(x){factor(x, levels = unique(x))}) 
# 
# df %>% head
# 
# df %>% saveRDS('walmart_train.RDS')


############ EDA valores faltantes

df <- readRDS('www/walmart_train.RDS')

#Valores faltantes
apply(is.na(df),2,sum)

#Valores únicos por columna
apply(df,2,function(x){length(unique(x))})

#Valores únicos con faltantes
apply(df %>% filter(is.na(producto_categoria)),2,function(x){length(unique(x))})
#el 3% aprox de los tickets tiene productos faltantes pertenecientes a casi todas las categorias
#pero los productos faltantes son sólo de dos departamenteos
df %>% 
  filter(is.na(producto_categoria)) %>% 
  select(producto_departamento) %>% 
  unique

#mas de la mitad de los faltantes son productos del departamento de pharma
apply(is.na(df %>% filter(producto_departamento =='pharmacy rx')),2,sum)

#zoom departamento pharma
apply(df %>% filter(!is.na(producto_identificador),
                    producto_departamento =='pharmacy rx'),2,
      function(x){length(unique(x))})



df %>%
  group_by(visita_tipo) %>% 
  tally

################### EDA Distribución de datos

#Los datos están mal balanceados
df %>%
  group_by(visita_tipo) %>% 
  tally

df %>% 
  ggplot(aes(x= visita_tipo))+
  geom_histogram(stat = 'count')

df %>% 
  ggplot(aes(x= visita_dia_semana))+
  geom_histogram(stat = 'count')

df %>% 
  ggplot(aes(x= producto_departamento))+
  geom_histogram(stat = 'count')

#Zoom a pharmacy
df %>% 
  filter(!is.na(producto_identificador),producto_departamento=='pharmacy rx') %>% 
  ggplot(aes(x= visita_tipo))+
  geom_histogram(stat = 'count')

df %>% 
  filter(!is.na(producto_identificador),producto_departamento=='pharmacy rx') %>% 
  ggplot(aes(x= producto_identificador))+
  geom_histogram(stat = 'count')


#MKT BKT
tt <- df %>% 
  filter(!is.na(producto_identificador)) %>% 
  sample_n(size = floor(nrow(df)*.001))

lis1 <- split(tt$producto_identificador, tt$visita_ticket)

trans.1 <- as(lis1, 'list')

num_canastas <- length(trans.1)

# Anàlisis de reglas de asociación--------------------------------------

reglas.1 <- apriori(trans.1, parameter=list(supp = 0.01, conf = 0.1, 
                                            target = 'rules', ext = T,  
                                            minlen = 2,
                                            maxlen = 2))

inspect(reglas.1 %>% head)

# df_1 <- sort(reglas.1, by = 'confidence') %>%
#   DATAFRAME 
# 
# df_2 <- df_1 %>% select(LHS, RHS, lhs.support, confidence, support) %>%
#   head(100) %>%
#   mutate(lhs.base = num_canastas*lhs.support) %>%
#   mutate(conf.ee = sqrt(confidence*(1-confidence)/lhs.base)) %>%
#   mutate_if(is.numeric, funs(round(., 2))) 
# 
# DT::datatable(df_2)



# 
df_1 <- sort(reglas.1, by = 'lift') %>%
  DATAFRAME %>% dplyr::filter(lift >= 1.4) 

df_1 %>% copiar_tabla
# 
# 
# head(quality(reglas.1))

sub.reglas_menos <- subset(reglas.1, lift < 1)

inspect(sub.reglas_menos) %>%  copiar_tabla()


sub.reglas_entre <- subset(reglas.1, (1 < lift)& (lift< 2))

inspect(sub.reglas_entre) %>%  copiar_tabla()

sub.reglas <- subset(reglas.1, lift > 2)

inspect(sub.reglas) %>%  copiar_tabla()


# Visualizaciones ---------------------------------------------------------




plot(sub.reglas, interactive = T)

plot(reglas.1, method="grouped")

plot(reglas.1, method="graph")

plotly_arules(reglas.1, colors=c('red','gray'))

plotly_arules(sub.reglas, colors=c('red','gray'))


df_reglas <- reglas.1 %>%
  DATAFRAME %>%
  rename(from=LHS, to=RHS) %>% 
  as_data_frame


graph_1 <- as_tbl_graph(df_1) %>%
  mutate(centrality = centrality_degree(mode = "all")) 

gg <-ggraph(graph_1, layout = 'fr', start.temp=100) +
  geom_edge_link(aes(alpha=lift), 
                 colour = 'red',
                 arrow = arrow(length = unit(4, 'mm'))) + 
  geom_node_point(aes(size = centrality, colour = centrality)) + 
  geom_node_text(aes(label = name), size=4,
                 colour = 'gray20', repel=TRUE) +
  theme_graph()


ggsave("/graphs/grafica_reglas_asociacion.pdf", plot = gg, device = "pdf", useDingbats = FALSE)

reglas_f2 <- subset(reglas.1, lift > 3, confidence > 0.4)

df_reglas <- reglas_f2 %>%
  DATAFRAME %>% 
  rename(from=LHS, to=RHS) %>% 
  as_data_frame

graph_1 <- as_tbl_graph(df_reglas) %>%
  mutate(centrality = centrality_degree(mode = 'all'))

ggraph(graph_1, layout = 'fr', start.temp=100) +
  geom_edge_link(aes(alpha=lift), colour = 'red',arrow = arrow(length = unit(4, 'mm'))) + 
  geom_node_point(aes(size = centrality, colour = centrality)) + 
  geom_node_text(aes(label = name), size=4,
                 colour = 'gray20', repel=TRUE) +
  theme_graph()
