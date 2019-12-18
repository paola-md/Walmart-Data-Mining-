# Recoleccion de datos de Walmart

De acuerdo a la documentación disponible[^1], uno de los requisitos necesarios para descargar los datos de visitas de los clientes de Walmart, es aportar la credenciales de un usuario registrado en el Kaggle.

En este caso, para facilitar este proceso, se implementó un programa en Bash, denominado download_extract_data.sh, el cual aprovecha la herramienta Wget de UNIX, para realizar la descarga de los datos del sitio electrónico en comento, recibiendo un archivo de configuración con las credenciales de autenticación de un cierto usuario (archivo cookies.txt).

Como resultado, dentro de la carpeta /data se obtiene los siguientes archivo en formato .csv:

* **test.csv:** el cual contiene los datos del conjunto de entrenamiento,
* **train.csv:** relativo a los datos de entrenamiento, y finalmente
* **sample_submission.csv:** es un ejemplo del formato en que se deben aportar los datos al sistema de Kaggle para la evaluación del desempeño del modelo de clasificación propuesto para el problema que nos ocupa.

[^1]: Véase https://github.com/Kaggle/kaggle-api


## Instrucciones para descarga de datos

1. Debemos darle permisos de ejecución al script de Bash desde la terminal:

```
chmod +x download_extract_data.sh
```
2. Posteriormente ejecutamos dicho programa:

```
./download_extract_data.sh
```

3. Como se ha mencionado, el resultado de la ejecución de este script es la descarga de tres archivos dentro de la carpeta /data (train.csv, test.csv y sample_submission.csv).
