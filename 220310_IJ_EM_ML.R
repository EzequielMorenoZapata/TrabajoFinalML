#CARGAR IMPORT.RDATA CON LOS MODELOS
load("import.RData")

#INSTALACION DE PAQUETES
# install.packages("tidyverse")
library(tidyverse)
#install.packages("skimr")
library(skimr)
#install.packages("modeest")
library(modeest)
#install.packages("dummies")
library(dummies)
#install.packages("corrplot")
library(corrplot)
#install.packages("caret")
library(caret)

# Cargar el .csv que contenga el X_test ##OJO CON EL NOMBRE DEL ARCHIVO####
##########################################TIENE QUE LLAMARSE X_test EN LA##
##########################################CARPETA DE TU EQUIPO#############
datasettest = read.csv('X_test.csv')

#EJECUTAR EL MODELO
modeling <- function(xtest, xtrain, MModel, MMcModel, CoModel, DCModel){

dataset <- rbind(xtrain,xtest)

# Eliminamos la variable fecha y hºact.cerca.sem
dataset = dataset[, -c(1,12)]

# eliminamos las dos variables de origen antepasados, una de ellas por tener mas del 70% de datos vacios
# y la otra por tener 80 categorias por lo que no agruparia variables.
dataset = dataset[, -c(3,4)]
#tratamiento de los valores empty en variables categoricas
#por tanto, se anade otra categoria mas como es, Nolose, lo que hara referencia
#a un nuevo factor/codigo
for (i in 1:length(dataset)) {
  dataset[,i] = ifelse(dataset[,i] == "" & is.character(dataset[,i]),
                       c('No lo se'),
                       dataset[,i])
}
# Familiar.miope.magno tiene una categoria mal introducida como es "No lo se, No" en la cual el 
# ultimo no se refiere al siguiente feature en la misma fila
dataset[,19] = ifelse(dataset[,18] == "No lo se, No",
                      c('No'),
                      dataset[,19])
dataset[,18] = ifelse(dataset[,18] == "No lo se, No",
                      c('No lo se'),
                      dataset[,18])

#VOY A ELIMINAR TODOS LOS ESPACIOS
for (i in 1:length(dataset)) {
  dataset[,i] = ifelse(dataset[,i] == "No lo se" ,
                       c('Nolose'),
                       dataset[,i])}

for (i in 1:length(dataset)) {
  dataset[,i] = ifelse(dataset[,i] == "Fototipo 1" ,
                       c('Fototipo1'),
                       dataset[,i])}
for (i in 1:length(dataset)) {
  dataset[,i] = ifelse(dataset[,i] == "Fototipo 2" ,
                       c('Fototipo2'),
                       dataset[,i])}
for (i in 1:length(dataset)) {
  dataset[,i] = ifelse(dataset[,i] == "Fototipo 3" ,
                       c('Fototipo3'),
                       dataset[,i])}
for (i in 1:length(dataset)) {
  dataset[,i] = ifelse(dataset[,i] == "Fototipo 4" ,
                       c('Fototipo4'),
                       dataset[,i])}
for (i in 1:length(dataset)) {
  dataset[,i] = ifelse(dataset[,i] == "Fototipo 5" ,
                       c('Fototipo5'),
                       dataset[,i])}

#se cambia las variables categoricas a factor (DUMMY) Y SE PONE DELANTE EN EL DATASET 
#TODAS LAS VARIABLES NUMERICAS Y DETRAS LAS CATEGORICAS 
#TAMBIEN SE HA CREADO UN DATASET_CHR CON LAS VARIABLES CATEGORICAS
#Y UN DATASET_NUM CON LAS VARIABLES NUMERICAS
dataset_chr <- dataset %>%
  select_if(is.character) %>%
  map(factor) %>%
  data.frame

dataset <- dataset %>%
  select_if(is.numeric) %>%
  cbind(dataset_chr)

dataset_num <- dataset %>%
  select_if(is.numeric) %>%
  data.frame


#paso las dos que considero catategoricas de las numericas
sumar <- dataset_num[,c(10,1)]
dataset_chr <- cbind(sumar,dataset_chr)

#hago la moda de las varaibles categoricas para los valores NAs
for (i in 1:2) {
  dataset_chr[,i] = ifelse(is.na(dataset_chr[,i]),
                           mfv(dataset_chr[,i]),
                           dataset_chr[,i])
}

#genero todas las variables dummy de las categoricas
x <- dataset_chr
for(i in 1:length(x)) {
  nombre.de.variable <- names(x)[i]
  x <- cbind(x,dummy(x[,i], sep = paste(nombre.de.variable)))
}
colnames(x) <- gsub("datasettest","x",colnames(x))

dataset_chr_dummy <- x[,(length(dataset_chr)+1):length(x)]


#en las numericas voy a quitar las que considero categoricas
dataset_num_ok <- dataset_num[,-c(10,1)]

#tratamiento de los valores NA en variables numericas
for (i in 1:length(dataset_num_ok)) {
  dataset_num_ok[,i] = ifelse(is.na(dataset_num_ok[,i]),
                              ave(dataset_num_ok[,i], FUN = function(x) mean(x, na.rm = TRUE)),
                              dataset_num_ok[,i])
}



dataset <- cbind(dataset_num_ok,dataset_chr_dummy)

# hago el log de las numericas
dataset[,1:30] = log(dataset[,1:30]+1)
# Escalado de valores
dataset[,1:30] = scale(dataset[,1:30])


#vuelvo a dejar solameente los datos de Test que Ibon nos pase
testing_set <- dataset[-c(1:132),]


#prediccion de M
y_predM = predict(MModel, type = "raw",
                    newdata = testing_set)

#prediccion de MM
y_predMM = predict(MMcModel, type = "raw",
                    newdata = testing_set)

#prediccion de Combo
y_predC = predict(CoModel, type = "raw",
                    newdata = testing_set)

#prediccion de DCombo
y_predDC = predict(modelDC, type = "raw",
                    newdata = testing_set)

#junto todo en un dataframe y cambio los 0 y los 1 por chr
Y_PRED <- cbind(as.character(y_predM),as.character(y_predMM),as.character(y_predC),as.character(y_predDC))
Y_PRED <- as.data.frame(Y_PRED)
colnames(Y_PRED) <- c("M","MM","Combo","DCombo")
for (i in 1:2) {
  Y_PRED[,i] = ifelse(Y_PRED[,i] == "0" ,
                      c('NO'),
                      c('SI'))}

return(Y_PRED)
}

Y_PRED <- modeling(datasettest, datasetx, modelM, modelMM, modelC, DCModel)

