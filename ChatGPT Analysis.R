########################################################################################################################################################
######################################################### IMPORTACION Y LECTURA DE LOS DATOS ###########################################################
########################################################################################################################################################

# Cargar el conjunto de datos 
setwd("~/Desktop/5 ICADE/TFG ANALYTICS")
raw_data <- read.csv("Twitter Jan Mar.csv")

# Cargar librerias necesarias
library(tidyverse)
library(ggplot2)
library(lubridate)
library(textcat)
library(devtools)
library(usethis)
library(stringr)
library(profvis)
library(dplyr)
library(tm)
library(parallel)

# Visualizar las primeras filas del conjunto de datos
head(raw_data)

# Resumen estadístico del conjunto de datos
summary(raw_data)

# Información sobre las columnas del conjunto de datos
str(raw_data)

# Crear dataframe con las columnas necesarias y asociandoles un nombre
# Han sido utilizadas todas las columnas que contenía el dataset original
df <- data.frame(
  id = raw_data$id,
  text = raw_data$content,
  user = raw_data$username,
  fecha = raw_data$date,
  retweet_number = raw_data$retweet_count,
  like_number = raw_data$like_count
)

#Crear funcion para extraer hashtags
# La función denominada "extract_hashtags" toma como argumento un "tweet". Se buscan hastags dentro de cada tweet (#) y al encontrarlos, se almacenan
# en una cadena. En caso de que ninguno ((length(hashtags) = 0)) fuera hallado para esa línea, se completará con NULL.
extract_hashtags <- function(tweet) {
  hashtags <- unlist(regmatches(tweet, gregexpr("#\\w+", tweet)))
  if (length(hashtags) > 0) {
    return(paste(hashtags, collapse = ', '))
  } else {
    return(NULL)
  }
}

# Aplicar la función a la columna de tweets y crear una nueva columna 'hashtags'
df <- df %>% mutate(hashtags = sapply(df$text, extract_hashtags))

# Función para extraer menciones de usuarios de un tweet
# La funciona toma prácticamente la misma forma que la utilizada para los hashtags, pero en este caso la búsqueda se realiza sobre el símbolo (@)
extract_mentions <- function(tweet) {
  mentions <- unlist(regmatches(tweet, gregexpr("@\\w+", tweet)))
  if (length(mentions) > 0) {
    return(paste(mentions, collapse = ', '))
  } else {
    return(NULL)
  }
}

# Aplicar la función a la columna de tweets y crear una nueva columna 'mentions'
df <- df %>% mutate(mentions = sapply(df$text, extract_mentions))

# Asegurar que las fechas están siendo tratadas con el formato correcto, añadiendo columnas para mes y día
# No se añade columna relativa al año dado que todos los posts son de 2023
df$fecha <- as.Date(raw_data$date, format = "%Y-%m-%d")
df$month <- format(df$fecha, "%m")
df$day <- format(df$fecha, "%d")
df$dia_semana <- weekdays(df$fecha)

# Crear dataframe con resultados
dias <- unique(df$dia_semana)
freq_semanal <- table(df$dia_semana)
freq_semanal <- as.data.frame(freq_semanal)

# Identificar el idioma de cada post con la funcion textcat, a la vez que es añadida una columa al df con el output
df <- df %>% mutate(idioma = sapply(text, function(x) textcat(x)))

# Ahora se cuenta con un dataset más completo, limpio y ordenado. Además, las variables tienen ahora el formato adecuado 
sapply(df, class)

########################################################################################################################################################
######################################################## ANALISIS DESCRIPTIVO DE LOS DATOS #############################################################
########################################################################################################################################################

###################################################### 1.- COMPROBACION LONGITUD DE LOS TWEETS #########################################################

# Añadir una nueva columna 'longitud' que contiene la longitud de cada tweet
df$longitud <- nchar(df$text)
summary(df$longitud)
str(df$longitud)

# Crear un histograma para visualizar la distribución de la longitud de los tweets
hist(df$longitud, col = "skyblue", main = "Distribución de Longitud de Tweets", xlab = "Longitud del Tweet")

# Encontrar tweets con carcateres >280 caracteres (longitud maxima de los posts en X)
tweets_mayores_280 <- df[df$longitud > 280, ]
# Existen 45.000 resgistros con +280 caracteres
summary(tweets_mayores_280)
# Son identificados como hilos de tweets y por lo tanto conservados dado que contendran inforemacion relevante para el analisis
str(tweets_mayores_280)

# Crear dataframe con resultados
longitudes <- unique(df$longitud)
freq_long <- table(df$longitud)
freq_long <- as.data.frame(freq_long)

# Exportar csv para graficar resultados de manera externa a RStudio
write.csv2(freq_long, "freq_long.csv", row.names = FALSE)

###################################################### 2.- ANALISIS TEMPORAL DE LOS TWEETS #########################################################

# Resumen de la frecuencia temporal
summary(df$fecha)
#Los tweets estan recopilados del 4 de enero de 2023 al 29 de marzo de 2023
df$fecha <- as.Date(df$fecha)

# Resumen de la frecuencia temporal agrupada por día
tweets_bydate<-df %>% 
  group_by(fecha) %>% 
  summarise(count = n())

summary(tweets_bydate)

# Eliminar filas con NA
tweets_bydate <- na.omit(tweets_bydate)

# Verificar nuevamente tus datos
summary(tweets_bydate)

# Crear dataframe con resultados
fechas <- unique(df$fecha)
freq_fecha <- table(df$fecha)
freq_fecha <- as.data.frame(freq_fecha)

#Identificar la longitud del dataset en términos de tiempo
num_fechas <- length(unique(tweets_bydate$fecha))
print(num_fechas)

# Obtener el rango de fechas
fecha_inicio <- as.Date("2023-01-04")
fecha_fin <- as.Date("2023-03-29")

# Crear serie temporal con el recuento de tweets y especificando el inicio
ST <- ts(tweets_bydate$count, frequency = 7)
plot(ST)

library(seasonal)

# Descomponer la serie temporal
ST.componentes <- seas(serie_temporal)
plot(descomposicion)

###################################################### 3.- ANALISIS IDIOMA DE LOS TWEETS #########################################################

# Crear dataframe con resultados
languages <- unique(df$idioma)
freq_lang <- table(df$idioma)
freq_lang <- as.data.frame(freq_lang)
write.csv2(freq_lang, "freq_lang.csv", row.names = FALSE)

###################################################### 4.- ANALISIS EMOTICONOS DE LOS TWEETS #########################################################

# Frecuencias de emojis
devtools::install_github("hadley/emo", force = TRUE)

library(emo)

#Crear dataframe con los 20 emoticonos más presentes en el corpus
emojis <- df %>%
  mutate(emoji = ji_extract_all(text)) %>%
  unnest(cols = c(emoji)) %>%
  count(emoji, sort = TRUE) %>%
  top_n(20)

#Visualizar resultados
summary(emojis)

###################################################### 5.- ANALISIS HASHATGS DE LOS TWEETS #########################################################

#Listar los hashtags que aparecen en el corpus
all_hashtags <- unlist(regmatches(df$hashtags,  gregexpr('\\w+', df$hashtags)))
freq_count <- as.data.frame(table(all_hashtags))

#Se ordenan los hashtags segun su presencia en el corpus, permitiendo identificar los mas frecuentes
freq_count <- freq_count[order(freq_count$Freq, decreasing = TRUE),]
total_different_hashtags <- nrow(freq_count)

#Quitar los que contienen palabras obvias
freq_count <- freq_count[!grepl("\\bChatGPT\\b|\\bchatgpt\\b|\\bchatGPT\\b|\\bGPT\\b", freq_count$all_hashtags),]

#Visualizar 25 hashtags
top25hashtags <- head(freq_count, 25)
top25hashtags

#Visualizar 40 hashtags
top40hashtags <- head(freq_count, 40)
top40hashtags

###################################################### 6.- ANALISIS RETWEETS DE LOS TWEETS #########################################################

#Contabilizar el numero de retweets existente en el corpus y agruparlos por este criterio
tweets_bynumberretweets <- df %>% 
  group_by(retweet_number) %>% 
  summarise(count = n())

tweets_bynumberretweets

###################################################### 7.- ANALISIS LIKES DE LOS TWEETS #########################################################

#Contabilizar numero de likes por post y agruparlos por este criterio
tweets_bynumberlikes <- df %>% 
  group_by(like_number) %>% 
  summarise(count = n())

tweets_bynumberlikes

###################################################### 8.- EXPORTANDO EL DATASET PRELIMINAR #########################################################

df <- apply(df, 2, as.character)
write.csv2(df, "df_completo.csv", row.names = FALSE)
df <- as.data.frame(df)

###################################################### 9.- SELECCION DEL IDIOMA #########################################################

#Debido a la necesidad de utilizar diccionarios de un unico idioma para los siguientes pasos de este estudio, se decide conservar
# unicamente aquellos tweets con idioma ingles (o equivalente)

#La siguientes lineas, evalua idiomas equivalentes al ingres para confirmar si pueden ser considerados o no
tweets_en_escoces <- df[df$idioma == "scots", "text"]
head(tweets_en_escoces)
#Si, se considera ingles
tweets_en_irish <- df[df$idioma == "irish", "text"]
tweets_en_irish
#Si, se considera ingles
tweets_en_max <- df[df$idioma == "manx", "text"]
tweets_en_max
#Si, se considera ingles. 
tweets_en_welsh <- df[df$idioma == "welsh", "text"]
tweets_en_welsh
#Si, se considera ingles
tweets_en_breton <- df[df$idioma == "breton", "text"]
tweets_en_breton
#Si, se considera ingles
tweets_en_scots2 <- df[df$idioma == "scots_gaelic", "text"]
tweets_en_scots2
#Si, se considera ingles
tweets_en_fri <- df[df$idioma == "frisian", "text"]
tweets_en_fri
#Si, se considera ingles
tweets_en_fri2 <- df[df$idioma == "middle_frisian", "text"]
tweets_en_fri2
#Si, se considera ingles

###################################################### 10.- DATASET EN INGLES #########################################################

#Se genera un dataset que conserve unicamente aquellos tweets de idioma ingles o los considerados como equivalentes
df_lang <- subset(df, idioma %in% c("irish", "scots", "welsh", "breton", "english", "manx", "scots_gaelic", "frisian", "middle_frisian"))
# Conservamos 459.699 registros

summary(df_lang)

#Exportar dataset FINAL
df_lang <- apply(df_lang, 2, as.character)
write.csv2(df_lang, "df_ingles.csv", row.names = FALSE)
df_lang <- as.data.frame(df_lang)

########################################################################################################################################################
######################################################## PRE-PROCESAMIENTO #############################################################
########################################################################################################################################################

###################################################### 11.- PARA MODELADO TÓPICOS #########################################################

# Primer pre-procesamiento
clean <- function(x) {
  x <- gsub("http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+", "", x)  # Eliminar URLs
  x <- gsub("@\\w+", "", x)  # Eliminar mentions
  x <- gsub("#\\w+", "", x)  # Eliminar hashtags
  x <- gsub("\\d+\\w*\\d*", "", x) # Eliminar números y palabras con números
  x <- gsub("[[:punct:]]", " ", x)  # Eliminar puntuación
  x <- tolower(x) #Normalizar
  x <- iconv(x, "latin1", "ASCII", sub = "")  # Eliminar acentos
  x <- removeNumbers(x) #Eliminar números
  x <- removeWords(x, stopwords("en")) #Eliminar stopwords
  x <- gsub('\\bRT\\b', '', x)  # Remove RT
  x <- gsub('\\b\\w{1,2}\\s', '', x) # Eliminar palabras de dos o menos caracteres seguidas de un espacio
  x <- gsub('\\s+', ' ', str_trim(x))  #Transformar más de un espacio en uno único
  return(x)
}

# Exportar dataset pre-procesado
df_lang$cleaned_text <- clean(df_lang$text)
length(df_lang$cleaned_text)
write.csv2(df_lang, "df_all.csv", row.names = FALSE)

# Exportar dataset pre-procesado y eliminando aquellas columnas no relevantes al pasar a Python
drop <- c("hashtags", "mentions", "text")
df_filtered <- df_lang[, !(names(df_lang) %in% drop)]
write.csv2(df_filtered, "df_filtered.csv", row.names = FALSE)

###################################################### 12.- PARA ANÁLISIS SENTIMIENTOS #########################################################

#La eliminación de elementos como hashtags, URLs, menciones y cifras, 

clean <- function(x) {
  x <- gsub("http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+", "", x)  # Eliminar URLs
  x <- gsub("@\\w+", "", x)  # Eliminar mentions
  x <- gsub("#\\w+", "", x)  # Eliminar hashtags
  x <- gsub("\\b\\d+\\b", "", x)  # Eliminar números y palabras con números
  x <- gsub('\\bRT\\b', '', x)  # Eliminar RT
  x <- gsub('\\b\\w{1,2}\\s', '', x) # Eliminar palabras de dos o menos caracteres seguidas de un espacio
  x <- gsub('\\s+', ' ', str_trim(x))  #Transformar más de un espacio en uno único.
  return(x)
}

#Importar el dataset trabajado en Python tras haber realizado el modelado de topicos
setwd("~/Desktop/5 ICADE/TFG ANALYTICS")
dataset <- read.csv("ChatGPT_Analysis_Lemmas.csv")

# Aplicar la funcion de pre-procesamiento
dataset$text <- clean(dataset$text)
# Exportar dataset pre-procesado
write.csv2(dataset, "df_sent_analysis.csv", row.names = FALSE)

# Exportar dataset pre-procesado y eliminando aquellas columnas no relevantes al pasar a Python
drop <- c("hashtags", "mentions", "text")
df_filtered <- df_lang[, !(names(df_lang) %in% drop)]
write.csv2(df_filtered, "df_filtered.csv", row.names = FALSE)


