####---- Scrip para descargar los datos de Walmart desde Kaggle ----####

#!/bin/bash

# Descarga de datos de Kaggle en la carpeta data (usando cookies de un cierto usuario)
wget -x --directory-prefix=../../data --load-cookies cookies.txt "https://www.kaggle.com/c/4654/download-all"

# Nos cambiamos de directorio
cd ../../data

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

# Generamos archivos de datos en formato feather

chmod +x writing_feather.R
./writing_feather.R
