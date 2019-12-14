####---- Scrip para descargar los datos de Walmart desde Kaggle ----####

#!/bin/bash

# Descarga de datos de Kaggle usando cookies de un cierto usuario
wget -x --load-cookies cookies.txt "https://www.kaggle.com/c/4654/download-all"

# Descomprimimos archivos de Walmart
unzip www.kaggle.com/c/4654/download-all
unzip sample_submission.csv.zip
unzip train.csv.zip
unzip -P Work4WalmarT test.csv.zip # Se agrega password para test set

# Eliminamos directorio descargado por wget
rm -r www.kaggle.com
rm test.csv.zip
rm train.csv.zip
rm sample_submission.csv.zip
