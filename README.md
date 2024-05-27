# BDML_2024_PS_3

---

# Descripción del Proyecto

La vivienda no sólo representa un requisito esencial para la felicidad humana, sino que también se ha convertido en un factor determinante de la pobre can be fixed by multidimensional en Colombia. Dado este contexto, adquirir vivienda se ha vuelto uno de los principales objetivos de las familias en Colombia, tanto por necesidad como por inversión. Sin embargo, decidir dónde y qué comprar no es una tarea sencilla.

Este proyecto se centra en desarrollar varios modelos de machine learning que predigan el precio de la vivienda en Chapinero (Bogotá), un distrito conocido por tener los precios por metro cuadrado más altos de la ciudad. Utilizamos diversos modelos, incluyendo regresión lineal, ElasticNet, bosques aleatorios, XGBoost y redes neuronales, con la mayoría de las validaciones cruzadas incorporando correlación espacial. El mejor modelo resultó ser XGBoost, optimizado para minimizar el error absoluto medio (MAE), que fue de 194,265,003.

Este trabajo se estructura de la siguiente manera: se inicia presentando los datos y la construcción de las variables predictoras. Posteriormente, se resumen brevemente los modelos de predicción desarrollados. Luego se detalla el mejor modelo predictor y, finalmente, se concluye sobre el ejercicio realizado.

## Paquetes Requeridos y Prerrequisitos

Los siguientes paquetes de R son necesarios para manejar el análisis de datos, la visualización y el modelado estadístico:

- **Gestión de Paquetes:**
  - `pacman`: Utilizado para cargar y gestionar otros paquetes de R de manera eficiente.

- **Manipulación e Importación/Exportación de Datos:**
  - `rio`: Facilita la importación y exportación de datos.
  - `dplyr`, `tidyverse`: Para manipulación de datos y operaciones ordenadas.
  - `recipes`: Prepara los datos para el modelado y análisis estadístico.

- **Visualización de Datos:**
  - `ggplot2`: Crea gráficos complejos a partir de datos en un marco de datos.
  - `gridExtra`, `corrplot`: Visualiza datos faltantes y crea gráficos de correlación.

- **Análisis Estadístico y Modelado:**
  - `stargazer`: Produce código LaTeX para tablas de regresión y estadísticas de resumen bien formateadas.
  - `MASS`, `glmnet`, `caret`: Incluye diversas herramientas estadísticas para modelos lineales y generalizados lineales y métodos de aprendizaje automático.
  - `rattle`: Proporciona una interfaz gráfica de usuario para ciencia de datos en R.
  - `car`: Compañero de regresión aplicada, ofrece modelos de regresión avanzados.

- **Aprendizaje Automático y Ajuste de Modelos:**
  - `parsnip`, `tidymodels`, `tune`, `dials`: Funciones modernas de modelado que simplifican el proceso de modelado.
  - `class`: Funciones para clasificación, incluyendo el vecino más cercano.
  - `leaps`: Funciones para la selección de subconjuntos de regresión.

- **Datos Espaciales y Sistemas de Información Geográfica:**
  - `sf`, `osmdata`, `lwgeom`, `spatialsample`: Manejo, procesamiento y muestreo de datos espaciales.

- **Web Scraping:**
  - `rvest`: Simplifica el web scraping al leer y manipular documentos XML y HTML.

## Instrucciones de Instalación

1. **Instalar R:** Descarga e instala R desde el sitio web de CRAN.
2. **Instalar Paquetes:**
   - Instala cualquier paquete faltante utilizando el siguiente comando de R:
     ```R
     if(!require(pacman)) install.packages("pacman")
     require(pacman)
     p_load(rio, tidyverse, skimr, gridExtra, corrplot, stargazer, MASS, rvest, dplr, ggplot2, visdat, caret, sf, osmdata, tidymodels, parsnip, glmnet, rattle, spatialsample, recipes, lwgeom, class, dials, car, leaps, tune, tidymodels)
     ```

## Uso:
**Configuración y Manipulación Inicial de Datos**

El script comienza limpiando el entorno y configurando los paquetes necesarios utilizando el paquete pacman para una gestión eficiente. Luego, determina el sistema operativo del usuario y establece el directorio de trabajo correspondiente para manejar archivos de datos específicos según las configuraciones del usuario.
Generación del DataFrame

Las primeras 85 líneas del archivo inicial script son cruciales, ya que incluyen la importación de datos, funciones de preprocesamiento y la imputación de datos faltantes. Estas líneas deben ejecutarse primero para generar el DataFrame requerido para los análisis subsiguientes. Los archivos de limpiza y generación de dato espaciales son diferentes a el archivo principal, llamado script. 

Después de la fila 139, cada sección se ejecuta de manera independiente:

    Existe una sección de análisis visual de los datos y, después, una sección independiente para la generación de cada uno de los modelos.

Este enfoque modular ayuda en la gestión eficiente de scripts largos, especialmente cuando se centra en partes específicas del análisis de datos o cuando se necesita volver a ejecutar solo ciertas secciones debido a actualizaciones iterativas en el flujo de trabajo de análisis.

## Frameworks, Librerías y Programas:

    Los scripts del proyecto fueron llevados a cabo en R.
    El documento final que consolida las conclusiones y el análisis fue realizado en LaTex.

##

 Autores

- María Camila Arias
- Martín Velásquez
- Mario Velásquez

Este archivo README debería proporcionar una guía clara y exhaustiva para cualquier usuario interesado en ejecutar o entender mejor este proyecto de investigación.
