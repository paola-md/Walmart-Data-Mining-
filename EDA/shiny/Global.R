instalar <- function(paquete) {
  
  if (!require(paquete,character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)) {
    install.packages(as.character(paquete), dependecies = TRUE, repos = "http://cran.us.r-project.org")
    library(paquete, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
  }
}

paquetes <- c('shiny','shinydashboard', 'shinyWidgets','shinyBS',
              'shinycssloaders', 'RPostgres','data.table','tidyr',
              'dplyr','reshape2','stringr','plotly','DT','lubridate',
              'magrittr')

lapply(paquetes, instalar)
library(readr)
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyWidgets)
library(shinyBS)
library(RPostgres)
library(data.table)
library(tidyr)
library(dplyr)
library(stringr)
library(reshape2)
library(plotly)
library(DT)
library(lubridate)
library(magrittr)
library(ggplot2)
library(rlang)
library(hdrcde)
library(GGally)
# dev.off()

source('source/SideBarMenuUI.R',
       local=TRUE,encoding = "utf-8")
source('source/TabUI.R',
       local=TRUE,encoding = "utf-8")

comprss <- function(tx) { 
  div <- findInterval(as.numeric(gsub("\\,", "", tx)), 
                      c(1, 1e3, 1e6, 1e9, 1e12) )
  paste(round( as.numeric(gsub("\\,","",tx))/10^(3*(div-1)), 2), 
        c("","K","M","B","T")[div] )}



#MENUS UI
categoricas <- c('visita_tipo','visita_dia_semana','producto_cantidad',
                 'producto_departamento')

numericas <- read_rds('www/walmart_train.RDS') %>% 
  names 

mixtas <- read_rds('www/walmart_train.RDS') %>% 
  # select_if(function(x) is.numeric(x)) %>% 
  names 

data <- read_rds('www/walmart_train.RDS')

tz <- Sys.timezone()

f <- list(
  family = "Courier New, monospace",
  size = 18,
  color = "#000066"
)


######Time series X axis config for Long Time Series
x_long <- list(
  title = "Fecha",
  titlefont = f,
  rangeselector = list(
    buttons = list(
      list(
        count = 1,
        label = "Minuto",
        step = "minute",
        stepmode = "backward"),
      list(
        count = 1,
        label = "Hora",
        step = "hour",
        stepmode = "backward"),
      list(
        count = 1,
        label = "Hoy",
        step = "day",
        stepmode = "backward"),
      list(
        count = 7,
        label = "1 Semana",
        step = "day",
        stepmode = "backward"),
      list(
        count = 1,
        label = "1 Mes",
        step = "month",
        stepmode = "backward"),
  list(label="Toda",step = "all"))),
  rangeslider = list(type = "date")
)
  
  


x_short <-   list(
  title = "Fecha",
  titlefont = f,
  rangeselector = list(
    buttons = list(
      list(
        count = 3,
        label = "3 meses",
        step = "month",
        stepmode = "backward"),
      list(
        count = 6,
        label = "6 meses",
        step = "month",
        stepmode = "backward"),
      list(
        count = 1,
        label = "1 año",
        step = "year",
        stepmode = "backward"),
      list(
        count = 1,
        label = "YTD",
        step = "year",
        stepmode = "todate"),
      list(step = "all"))),
  rangeslider = list(type = "date")
)