#!/usr/bin/env Rscript

library(feather)
library(readr)

# Abrimos los archivos descargados
test <- read_csv("test.csv")
train <- read_csv("train.csv")


# Escribe los archivos descargados en formato feather
write_feather(train, "train.feather")
write_feather(test, "test.feather")