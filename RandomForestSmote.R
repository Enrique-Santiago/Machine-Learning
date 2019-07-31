#####################################################################
###########      Carga y lectua de librerias a utilizar   ###########
#####################################################################
library(tibble)
library(openxlsx)
library(ca)
library(carData)
library(car)  #Paquete para usar la función Recode
library(xlsx)
library(ggplot2)
library(randomForest)  #Paquete de machine learning con RandomForest
library(ranger)  #Paquete de machine learning con RandomForest usado en caret
library(lattice) #Paquete que necesita la libreria caret
library(caret)  #Paquete de machine learning
library(data.table)  #Paquete para trabajar con data.tables
library(stringr)
library(pROC)
library(Hmisc)
library(gganimate)
library(smotefamily)   #Paquete con técnicas de muestreo para corregir imbalanced data
library(DMwR)   #Paquete con técnicas de muestreo para corregir imbalanced data


#####################################################################
###########            Lectura de los ficheros            ###########
#####################################################################

training_data <- readRDS(file = "C:/Users/U0xxxxx/Documents/Enrique-EMOS/FicherosR/train.rds")
testing_data <- readRDS(file = "C:/Users/U0xxxxx/Documents/Enrique-EMOS/FicherosR/test.rds")

totaldata <- rbind(training_data, testing_data)

summary(totaldata)

#####################################################################
###########        Recodificar la variables EDAD          ###########
#####################################################################
totaldata$RANGOEDAD <- Recode(totaldata$EDADa, 
                              '0:30="Menor de 29"; 30:39="30-39"; 40:49="40-49"; 50:59="50-59"; 60:99="Mayor 60"',
                              levels=c("Menor de 30","30-39","40-49","50-59","Mayor 60"))

totaldata[, RANGOEDAD := as.factor(RANGOEDAD)]



#####################################################################
###########            Variables explicativas             ###########
#####################################################################
vars_SinRedad_SinPesos <- c('EDADa', 'SEXOa', 'ESTRATO','PROXY_0', 
                            'CNAE_AS_1', 'CNAE_AS_2', 'CNAE_AS_3', 
                            'CNO_AS_1', 'CNO_AS_2', 'CNO_AS_3',
                            'F18', 'D28', 'A10_i', 'CCAA'
                            #,'FACTORADULTO'
)

vars_ConRedad_SinPesos <- c('RANGOEDAD', 'SEXOa', 'ESTRATO','PROXY_0', 
                            'CNAE_AS_1', 'CNAE_AS_2', 'CNAE_AS_3', 
                            'CNO_AS_1', 'CNO_AS_2', 'CNO_AS_3',
                            'F18', 'D28', 'A10_i', 'CCAA'
                            #,'FACTORADULTO'
)

vars_SinRedad_ConPesos <- c('EDADa', 'SEXOa', 'ESTRATO','PROXY_0', 
                            'CNAE_AS_1', 'CNAE_AS_2', 'CNAE_AS_3', 
                            'CNO_AS_1', 'CNO_AS_2', 'CNO_AS_3',
                            'F18', 'D28', 'A10_i', 'CCAA'
                            ,'FACTORADULTO'
)

vars_ConRedad_ConPesos <- c('RANGOEDAD', 'SEXOa', 'ESTRATO','PROXY_0', 
                            'CNAE_AS_1', 'CNAE_AS_2', 'CNAE_AS_3', 
                            'CNO_AS_1', 'CNO_AS_2', 'CNO_AS_3',
                            'F18', 'D28', 'A10_i', 'CCAA'
                            ,'FACTORADULTO'
)


#####################################################################
###########       Creación de fichero Test y Train        ###########
#####################################################################
#la función createDataPartition() garantiza una distribución aproximada (reparto estratificado)
set.seed(123)
train_t1 <- createDataPartition(y = totaldata$target1, p = 0.8, list = FALSE, times = 1)
training_list_aux <- totaldata[train_t1, ]
testing_list_t1  <- totaldata[-train_t1, ]


training_list_aux[, IDENTHOGAR := as.factor(IDENTHOGAR)]

as.data.frame(table(training_list_aux$target1))

# Hiperparámetros INICIALES SMOTE
hiperparametros_smote <- expand.grid(perc.over = c(50, 150, 300),
                                     k = c(3, 6),
                                     perc.under = c(25, 200, 400))



x <- nrow(hiperparametros_smote)

train_data_files <- lapply(seq(1, x), function (i){
      SMOTE(target1 ~., training_list_aux, 
            perc.over = hiperparametros_smote[i,1], 
            k = hiperparametros_smote[i,2], 
            perc.under = hiperparametros_smote[i,3])
  
})

names(train_data_files) <- unlist(lapply(seq(1, nrow(hiperparametros_smote)), function (i){
  paste("training_list_t1_", i, sep = "")}))



#============================================================#
#============================================================#
#================                           =================#
#================         FUNCIONES         =================#
#================                           =================#
#============================================================#
#============================================================#

###########             Modelo              ###########
fcn_ModeloRF <- function(FICHEROTRAIN, VREGRE, VRESP) {
         tuneRF(FICHEROTRAIN[, ..VREGRE],
                VRESP, ntreeTry=1000,
                stepFactor=4, improve=0.05, trace=TRUE, plot=FALSE, doBest = TRUE)
}

###########           Predicción  Prob          ###########
fcn_PredicRF <- function(MODELORF, VREGRE) {
                  predict(MODELORF, 
                  testing_list_t1[, ..VREGRE], type = "prob")
}


###########           Predicción          ###########
fcn_PredicRF_Conf <- function(MODELORF, VREGRE) {
  predict(MODELORF, 
          testing_list_t1[, ..VREGRE])
}


###########           Confusión          ###########
fcn_Matriz_Conf <- function(PREDIC) {
confusionMatrix(data = PREDIC, 
                reference = as.factor(datos_test$target1))
}


###########            Resultado            ###########
fcn_ResultRF <- function(PREDICRF) {
  results <- data.table(PobError = PREDICRF, target1 = testing_list_t1$target1)
  results[
    , w := testing_list_t1$FACTORADULTO][
      , m1 := PobError * w][
        , PobError_M := ecdf(m1)(m1)][
          , target1 := as.numeric(target1) - 1][
            , IDENTHOGAR := testing_list_t1$IDENTHOGAR][
              , CNO1_AS_raw := testing_list_t1$CNO1_AS_raw][
                , CNO1_AS_ed := testing_list_t1$CNO1_AS_ed][
                  , CNO2_AS_raw := testing_list_t1$CNO2_AS_raw][
                    , CNO2_AS_ed := testing_list_t1$CNO2_AS_ed]
}



#================     TARGET1 - BALANCED    =================#
#============================================================#
#================          SIN PESOS        =================#
#================          SIN RANGOEDAD    =================#
#============================================================#

###########             Modelos              ###########
model_data_files <- lapply(seq(1, x), function (i){
  fcn_ModeloRF(train_data_files[[i]],
               vars_SinRedad_SinPesos, 
               train_data_files[[i]]$target1)})

saveRDS(object = model_data_files, file = "N:/UDMTD12/Enrique-EMOS/TFM/Random Forest/Modelos_RF_Smote_SinREdad_SinPesos_v2.rds")

###########           Predicción             ###########
predict_data_files <- lapply(seq(1, x), function (i){
  fcn_PredicRF (model_data_files[[i]],vars_SinRedad_SinPesos) })

predict_data_files <- lapply(seq(1, x), function (i){ predict_data_files[[i]][,2] })



###########     Análisis de resultados       ###########
results_data_files <- lapply(seq(1, x), function (i){
  fcn_ResultRF (predict_data_files[[i]]) })


results_data_files_Sort <- lapply(seq(1, x), function (i){
  results_data_files[[i]][order(-PobError)][, priority := .I]
  })

results_data_files_Sort_m1 <- lapply(seq(1, x), function (i){
  results_data_files[[i]][order(-PobError_M)][, priority := .I]
})



###########           Curva ROC         ###########

r1 <- nrow(as.data.frame(table(hiperparametros_smote$perc.over)))
r2 <- nrow(as.data.frame(table(hiperparametros_smote$k)))
r3 <- nrow(as.data.frame(table(hiperparametros_smote$perc.under)))
par(mfrow = c(4, 5))

for(i in 1:x) { 
roc1 <- roc(response = results_data_files[[i]]$target1, 
            predictor = results_data_files[[i]]$PobError)
auc1 <- auc(roc1)
thrsh1 <- coords(roc1, "best", ret = "threshold")
spec1 <- coords(roc1, "best", ret = "spec")
sens1 <- coords(roc1, "best", ret = "sens")
par(mar=c(5.1, 4.1, 10, 2.1))
plot(roc1, col = 'blue', lwd = 3,
     xlim = c(1,0), ylim = c(0,1),
     print.auc = FALSE, auc.polygon = FALSE,
     main = paste0("k=",hiperparametros_smote$k[i],
                   " over=",hiperparametros_smote$perc.over[i],
                   " under=",hiperparametros_smote$perc.under[i]))

rocm1 <- roc(response = results_data_files[[i]]$target1, 
             predictor = results_data_files[[i]]$PobError_M, 
             col = 'red', xlim = c(0,1), ylim = c(0,1))
aucm1 <- auc(rocm1)
thrshm1 <- coords(rocm1, "best", ret = "threshold")
specm1 <- coords(rocm1, "best", ret = "spec")
sensm1 <- coords(rocm1, "best", ret = "sens")
plot(rocm1, add = TRUE, col = 'red', lty = 3, lwd = 3,
     xlim = c(1,0), ylim = c(0,1),
     print.auc = FALSE, auc.polygon = FALSE)
legend(0.5, 0.3, legend = c("Without Sampling Weight", "With Sampling Weight"),
       col = c("blue", "red"), lty=c(1, 3), cex = 0.9, bty = "n")
text(0.95, 0.99, paste0('without:', round(auc1, 2)))
text(0.95, 0.90, paste0('with:', round(aucm1, 2)))
#  minor.tick(nx=10, ny=10)
points(x = spec1, y = sens1, col = 'blue', pch = 19, cex = 1.5)
points(x = specm1, y = sensm1, col = 'red', pch = 19, cex = 1.5)
}


#############
#Matrices de confusión
############
model_data_files <- readRDS(file = "N:/UDMTD12/Enrique-EMOS/TFM/Random Forest/Modelos_RF_Smote_SinREdad_SinPesos.rds")

predict_res_conf <- lapply(seq(1, x), function (i){
  fcn_PredicRF_Conf (model_data_files[[i]],vars_SinRedad_SinPesos) })



Matrices_confusion <- lapply(seq(1, x), function (i){
  fcn_Matriz_Conf (predict_res_conf[[i]]) })



#================     TARGET1 - BALANCED    =================#
#============================================================#
#================          SIN PESOS        =================#
#================          CON RANGOEDAD    =================#
#============================================================#

###########             Modelos              ###########
model_data_files <- lapply(seq(1, x), function (i){
  fcn_ModeloRF(train_data_files[[i]],
               vars_ConRedad_SinPesos, 
               train_data_files[[i]]$target1)})

saveRDS(object = model_data_files, file = "N:/UDMTD12/Enrique-EMOS/TFM/Random Forest/Modelos_RF_Smote_ConREdad_SinPesos.rds")


###########           Predicción             ###########
predict_data_files <- lapply(seq(1, x), function (i){
  fcn_PredicRF (model_data_files[[i]],vars_ConRedad_SinPesos) })

predict_data_files <- lapply(seq(1, x), function (i){ predict_data_files[[i]][,2] })



###########     Análisis de resultados       ###########
results_data_files <- lapply(seq(1, x), function (i){
  fcn_ResultRF (predict_data_files[[i]]) })


results_data_files_Sort <- lapply(seq(1, x), function (i){
  results_data_files[[i]][order(-PobError)][, priority := .I]
})

results_data_files_Sort_m1 <- lapply(seq(1, x), function (i){
  results_data_files[[i]][order(-PobError_M)][, priority := .I]
})



###########           Curva ROC         ###########

r1 <- nrow(as.data.frame(table(hiperparametros_smote$perc.over)))
r2 <- nrow(as.data.frame(table(hiperparametros_smote$k)))
r3 <- nrow(as.data.frame(table(hiperparametros_smote$perc.under)))
par(mfrow = c(4, 5))

for(i in 1:x) { 
  roc1 <- roc(response = results_data_files[[i]]$target1, 
              predictor = results_data_files[[i]]$PobError)
  auc1 <- auc(roc1)
  thrsh1 <- coords(roc1, "best", ret = "threshold")
  spec1 <- coords(roc1, "best", ret = "spec")
  sens1 <- coords(roc1, "best", ret = "sens")
  par(mar=c(5.1, 4.1, 10, 2.1))
  plot(roc1, col = 'blue', lwd = 3,
       xlim = c(1,0), ylim = c(0,1),
       print.auc = FALSE, auc.polygon = FALSE,
       main = paste0("k=",hiperparametros_smote$k[i],
                     " over=",hiperparametros_smote$perc.over[i],
                     " under=",hiperparametros_smote$perc.under[i]))
  
  rocm1 <- roc(response = results_data_files[[i]]$target1, 
               predictor = results_data_files[[i]]$PobError_M, 
               col = 'red', xlim = c(0,1), ylim = c(0,1))
  aucm1 <- auc(rocm1)
  thrshm1 <- coords(rocm1, "best", ret = "threshold")
  specm1 <- coords(rocm1, "best", ret = "spec")
  sensm1 <- coords(rocm1, "best", ret = "sens")
  plot(rocm1, add = TRUE, col = 'red', lty = 3, lwd = 3,
       xlim = c(1,0), ylim = c(0,1),
       print.auc = FALSE, auc.polygon = FALSE)
  legend(0.5, 0.3, legend = c("Without Sampling Weight", "With Sampling Weight"),
         col = c("blue", "red"), lty=c(1, 3), cex = 0.9, bty = "n")
  text(0.95, 0.99, paste0('without:', round(auc1, 2)))
  text(0.95, 0.90, paste0('with:', round(aucm1, 2)))
  #  minor.tick(nx=10, ny=10)
  points(x = spec1, y = sens1, col = 'blue', pch = 19, cex = 1.5)
  points(x = specm1, y = sensm1, col = 'red', pch = 19, cex = 1.5)
}



#############
#Matrices de confusión
############
model_data_files <- readRDS(file = "N:/UDMTD12/Enrique-EMOS/TFM/Random Forest/Modelos_RF_Smote_ConREdad_SinPesos.rds")

predict_res_conf <- lapply(seq(1, x), function (i){
  fcn_PredicRF_Conf (model_data_files[[i]],vars_ConRedad_SinPesos) })



Matrices_confusion <- lapply(seq(1, x), function (i){
  fcn_Matriz_Conf (predict_res_conf[[i]]) })



#================     TARGET1 - BALANCED    =================#
#============================================================#
#================          CON PESOS        =================#
#================          SIN RANGOEDAD    =================#
#============================================================#

###########             Modelos              ###########
model_data_files <- lapply(seq(1, x), function (i){
  fcn_ModeloRF(train_data_files[[i]],
               vars_SinRedad_ConPesos, 
               train_data_files[[i]]$target1)})

saveRDS(object = model_data_files, file = "N:/UDMTD12/Enrique-EMOS/TFM/Random Forest/Modelos_RF_Smote_SinREdad_ConPesos.rds")


###########           Predicción             ###########
predict_data_files <- lapply(seq(1, x), function (i){
  fcn_PredicRF (model_data_files[[i]],vars_SinRedad_ConPesos) })

predict_data_files <- lapply(seq(1, x), function (i){ predict_data_files[[i]][,2] })



###########     Análisis de resultados       ###########
results_data_files <- lapply(seq(1, x), function (i){
  fcn_ResultRF (predict_data_files[[i]]) })


results_data_files_Sort <- lapply(seq(1, x), function (i){
  results_data_files[[i]][order(-PobError)][, priority := .I]
})

results_data_files_Sort_m1 <- lapply(seq(1, x), function (i){
  results_data_files[[i]][order(-PobError_M)][, priority := .I]
})



###########           Curva ROC         ###########

r1 <- nrow(as.data.frame(table(hiperparametros_smote$perc.over)))
r2 <- nrow(as.data.frame(table(hiperparametros_smote$k)))
r3 <- nrow(as.data.frame(table(hiperparametros_smote$perc.under)))
par(mfrow = c(4, 5))

for(i in 1:x) { 
  roc1 <- roc(response = results_data_files[[i]]$target1, 
              predictor = results_data_files[[i]]$PobError)
  auc1 <- auc(roc1)
  thrsh1 <- coords(roc1, "best", ret = "threshold")
  spec1 <- coords(roc1, "best", ret = "spec")
  sens1 <- coords(roc1, "best", ret = "sens")
  par(mar=c(5.1, 4.1, 10, 2.1))
  plot(roc1, col = 'blue', lwd = 3,
       xlim = c(1,0), ylim = c(0,1),
       print.auc = FALSE, auc.polygon = FALSE,
       main = paste0("k=",hiperparametros_smote$k[i],
                     " over=",hiperparametros_smote$perc.over[i],
                     " under=",hiperparametros_smote$perc.under[i]))
  
  rocm1 <- roc(response = results_data_files[[i]]$target1, 
               predictor = results_data_files[[i]]$PobError_M, 
               col = 'red', xlim = c(0,1), ylim = c(0,1))
  aucm1 <- auc(rocm1)
  thrshm1 <- coords(rocm1, "best", ret = "threshold")
  specm1 <- coords(rocm1, "best", ret = "spec")
  sensm1 <- coords(rocm1, "best", ret = "sens")
  plot(rocm1, add = TRUE, col = 'red', lty = 3, lwd = 3,
       xlim = c(1,0), ylim = c(0,1),
       print.auc = FALSE, auc.polygon = FALSE)
  legend(0.5, 0.3, legend = c("Without Sampling Weight", "With Sampling Weight"),
         col = c("blue", "red"), lty=c(1, 3), cex = 0.9, bty = "n")
  text(0.95, 0.99, paste0('without:', round(auc1, 2)))
  text(0.95, 0.90, paste0('with:', round(aucm1, 2)))
  #  minor.tick(nx=10, ny=10)
  points(x = spec1, y = sens1, col = 'blue', pch = 19, cex = 1.5)
  points(x = specm1, y = sensm1, col = 'red', pch = 19, cex = 1.5)
}

#############
#Matrices de confusión
############
model_data_files <- readRDS(file = "N:/UDMTD12/Enrique-EMOS/TFM/Random Forest/Modelos_RF_Smote_SinREdad_ConPesos.rds")

predict_res_conf <- lapply(seq(1, x), function (i){
  fcn_PredicRF_Conf (model_data_files[[i]],vars_SinRedad_ConPesos) })



Matrices_confusion <- lapply(seq(1, x), function (i){
  fcn_Matriz_Conf (predict_res_conf[[i]]) })



#================     TARGET1 - BALANCED    =================#
#============================================================#
#================          CON PESOS        =================#
#================          CON RANGOEDAD    =================#
#============================================================#

###########             Modelos              ###########
model_data_files <- lapply(seq(1, x), function (i){
  fcn_ModeloRF(train_data_files[[i]],
               vars_ConRedad_ConPesos, 
               train_data_files[[i]]$target1)})

saveRDS(object = model_data_files, file = "N:/UDMTD12/Enrique-EMOS/TFM/Random Forest/Modelos_RF_Smote_ConREdad_ConPesos.rds")


###########           Predicción             ###########
predict_data_files <- lapply(seq(1, x), function (i){
  fcn_PredicRF (model_data_files[[i]],vars_ConRedad_ConPesos) })

predict_data_files <- lapply(seq(1, x), function (i){ predict_data_files[[i]][,2] })



###########     Análisis de resultados       ###########
results_data_files <- lapply(seq(1, x), function (i){
  fcn_ResultRF (predict_data_files[[i]]) })


results_data_files_Sort <- lapply(seq(1, x), function (i){
  results_data_files[[i]][order(-PobError)][, priority := .I]
})

results_data_files_Sort_m1 <- lapply(seq(1, x), function (i){
  results_data_files[[i]][order(-PobError_M)][, priority := .I]
})



###########           Curva ROC         ###########

r1 <- nrow(as.data.frame(table(hiperparametros_smote$perc.over)))
r2 <- nrow(as.data.frame(table(hiperparametros_smote$k)))
r3 <- nrow(as.data.frame(table(hiperparametros_smote$perc.under)))
par(mfrow = c(4, 5))

for(i in 1:x) { 
  roc1 <- roc(response = results_data_files[[i]]$target1, 
              predictor = results_data_files[[i]]$PobError)
  auc1 <- auc(roc1)
  thrsh1 <- coords(roc1, "best", ret = "threshold")
  spec1 <- coords(roc1, "best", ret = "spec")
  sens1 <- coords(roc1, "best", ret = "sens")
  par(mar=c(5.1, 4.1, 10, 2.1))
  plot(roc1, col = 'blue', lwd = 3,
       xlim = c(1,0), ylim = c(0,1),
       print.auc = FALSE, auc.polygon = FALSE,
       main = paste0("k=",hiperparametros_smote$k[i],
                     " over=",hiperparametros_smote$perc.over[i],
                     " under=",hiperparametros_smote$perc.under[i]))
  
  rocm1 <- roc(response = results_data_files[[i]]$target1, 
               predictor = results_data_files[[i]]$PobError_M, 
               col = 'red', xlim = c(0,1), ylim = c(0,1))
  aucm1 <- auc(rocm1)
  thrshm1 <- coords(rocm1, "best", ret = "threshold")
  specm1 <- coords(rocm1, "best", ret = "spec")
  sensm1 <- coords(rocm1, "best", ret = "sens")
  plot(rocm1, add = TRUE, col = 'red', lty = 3, lwd = 3,
       xlim = c(1,0), ylim = c(0,1),
       print.auc = FALSE, auc.polygon = FALSE)
  legend(0.5, 0.3, legend = c("Without Sampling Weight", "With Sampling Weight"),
         col = c("blue", "red"), lty=c(1, 3), cex = 0.9, bty = "n")
  text(0.95, 0.99, paste0('without:', round(auc1, 2)))
  text(0.95, 0.90, paste0('with:', round(aucm1, 2)))
  #  minor.tick(nx=10, ny=10)
  points(x = spec1, y = sens1, col = 'blue', pch = 19, cex = 1.5)
  points(x = specm1, y = sensm1, col = 'red', pch = 19, cex = 1.5)
}


#############
#Matrices de confusión
############
model_data_files <- readRDS(file = "N:/UDMTD12/Enrique-EMOS/TFM/Random Forest/Modelos_RF_Smote_ConREdad_ConPesos.rds")

predict_res_conf <- lapply(seq(1, x), function (i){
  fcn_PredicRF_Conf (model_data_files[[i]],vars_ConRedad_ConPesos) })



Matrices_confusion <- lapply(seq(1, x), function (i){
  fcn_Matriz_Conf (predict_res_conf[[i]]) })




#============================================================#
#============================================================#
#================                           =================#
#================          TARGET1          =================#
#================   DATOS NO DISTRIBUIDOS   =================#
#================                           =================#
#============================================================#
#============================================================#


#####################################################################
###########       Creación de fichero Test y Train        ###########
#####################################################################
set.seed(1)
id_train <- sample(1:nrow(totaldata), size = floor(0.8 * nrow(totaldata)))
training_list_t1un <- totaldata[id_train, ]
testing_list_t1un <- totaldata[-id_train,]


#Distribución de la variable respuesta (target1) 
distribucion_train_t1un <- prop.table(table(training_list_t1un$target1))
distribucion_test_t1un  <- prop.table(table(testing_list_t1un$target1))
data.frame(train_t1un = distribucion_train_t1un, test = distribucion_test_t1un )

#================    TARGET1 -UNBALANCED    =================#
#============================================================#
#================          SIN PESOS        =================#
#================          SIN RANGOEDAD    =================#
#============================================================#

###########             Modelo              ###########
set.seed(1)
model_t1un_SinRedad_SinPesos <- tuneRF(training_list_t1un[, ..vars_SinRedad_SinPesos], 
                                       training_list_t1un$target1, ntreeTry=1000, 
                                       stepFactor=4, improve=0.05, trace=TRUE, plot=TRUE, doBest = TRUE)

saveRDS(object = model_t1un_SinRedad_SinPesos, file = "N:/UDMTD12/Enrique-EMOS/TFM/Random Forest/Modelo_RF_SinRedad_SinPesos.rds")


###########           Predicción             ###########
pred_t1un_SinRedad_SinPesos <- predict(model_t1un_SinRedad_SinPesos, 
                                       testing_list_t1un[, ..vars_SinRedad_SinPesos], type = "prob")
pred_t1un_SinRedad_SinPesos <- pred_t1un_SinRedad_SinPesos[, 2]


###########           Análisis de resultados              ###########

results_t1un_SinRedad_SinPesos <- data.table(PobErr_t1un_SinRedad_SinPesos = pred_t1un_SinRedad_SinPesos, 
                                             target1 = testing_list_t1un$target1
)


results_t1un_SinRedad_SinPesos[
  , w := testing_list_t1un$FACTORADULTO][
    , m1 := PobErr_t1un_SinRedad_SinPesos * w][
      , PobErr_t1un_SinRedad_SinPesos_M := ecdf(m1)(m1)][
        , target1 := as.numeric(target1) - 1][
          , IDENTHOGAR := testing_list_t1un$IDENTHOGAR][
            , CNO1_AS_raw := testing_list_t1un$CNO1_AS_raw][
              , CNO1_AS_ed := testing_list_t1un$CNO1_AS_ed][
                , CNO2_AS_raw := testing_list_t1un$CNO2_AS_raw][
                  , CNO2_AS_ed := testing_list_t1un$CNO2_AS_ed]



results_t1un_Sort_SinRedad_SinPesos <- results_t1un_SinRedad_SinPesos[order(-PobErr_t1un_SinRedad_SinPesos)][, priority := .I]
results_t1un_Sort_SinRedad_SinPesos_m1 <- results_t1un_SinRedad_SinPesos[order(-PobErr_t1un_SinRedad_SinPesos_M)][, priority := .I]


###########           Curva ROC         ###########
roc1 <- roc(response = results_t1un_SinRedad_SinPesos$target1, 
            predictor = results_t1un_SinRedad_SinPesos$PobErr_t1un_SinRedad_SinPesos)
auc1 <- auc(roc1)
thrsh1 <- coords(roc1, "best", ret = "threshold")
spec1 <- coords(roc1, "best", ret = "spec")
sens1 <- coords(roc1, "best", ret = "sens")
par(mar=c(5.1, 4.1, 10, 2.1))
plot(roc1, col = 'blue', lwd = 3,
     xlim = c(1,0), ylim = c(0,1),
     print.auc = FALSE, auc.polygon = FALSE,
     main = 'Target1-Unbalanced-Sin RangoEdad-Sin Pesos')

rocm1 <- roc(response = results_t1un_SinRedad_SinPesos$target1, 
             predictor = results_t1un_SinRedad_SinPesos$PobErr_t1un_SinRedad_SinPesos_M, 
             col = 'red', xlim = c(0,1), ylim = c(0,1))
aucm1 <- auc(rocm1)
thrshm1 <- coords(rocm1, "best", ret = "threshold")
specm1 <- coords(rocm1, "best", ret = "spec")
sensm1 <- coords(rocm1, "best", ret = "sens")
plot(rocm1, add = TRUE, col = 'red', lty = 3, lwd = 3,
     xlim = c(1,0), ylim = c(0,1),
     print.auc = FALSE, auc.polygon = FALSE)
legend(0.5, 0.3, legend = c("Without Sampling Weight", "With Sampling Weight"),
       col = c("blue", "red"), lty=c(1, 3), cex = 0.9, bty = "n")
text(0.8, 0.95, paste0('AUC without:', round(auc1, 2)))
text(0.8, 0.9, paste0('AUC with:', round(aucm1, 2)))
#  minor.tick(nx=10, ny=10)
points(x = spec1, y = sens1, col = 'blue', pch = 19, cex = 1.5)
points(x = specm1, y = sensm1, col = 'red', pch = 19, cex = 1.5)


#############
#Matriz de confusión
############
pred_t1un_SinRedad_SinPesos_mc <- predict(model_t1un_SinRedad_SinPesos, 
                                       testing_list_t1un[, ..vars_SinRedad_SinPesos])

confusionMatrix(data = pred_t1un_SinRedad_SinPesos_mc, 
                  reference = as.factor(testing_list_t1un$target1))




#================    TARGET1 -UNBALANCED    =================#
#============================================================#
#================          SIN PESOS        =================#
#================          CON RANGOEDAD    =================#
#============================================================#

###########             Modelo              ###########
set.seed(1)
model_t1un_ConRedad_SinPesos <- tuneRF(training_list_t1un[, ..vars_ConRedad_SinPesos], 
                                       training_list_t1un$target1, ntreeTry=1000, 
                                       stepFactor=4, improve=0.05, trace=TRUE, plot=TRUE, doBest = TRUE)

saveRDS(object = model_t1un_ConRedad_SinPesos, file = "N:/UDMTD12/Enrique-EMOS/TFM/Random Forest/Modelo_RF_ConRedad_SinPesos.rds")


###########           Predicción             ###########
pred_t1un_ConRedad_SinPesos <- predict(model_t1un_ConRedad_SinPesos, 
                                       testing_list_t1un[, ..vars_ConRedad_SinPesos], type = "prob")
pred_t1un_ConRedad_SinPesos <- pred_t1un_ConRedad_SinPesos[, 2]


###########           Análisis de resultados              ###########

results_t1un_ConRedad_SinPesos <- data.table(PobErr_t1un_ConRedad_SinPesos = pred_t1un_ConRedad_SinPesos, 
                                             target1 = testing_list_t1un$target1
)


results_t1un_ConRedad_SinPesos[
  , w := testing_list_t1un$FACTORADULTO][
    , m1 := PobErr_t1un_ConRedad_SinPesos * w][
      , PobErr_t1un_ConRedad_SinPesos_M := ecdf(m1)(m1)][
        , target1 := as.numeric(target1) - 1][
          , IDENTHOGAR := testing_list_t1un$IDENTHOGAR][
            , CNO1_AS_raw := testing_list_t1un$CNO1_AS_raw][
              , CNO1_AS_ed := testing_list_t1un$CNO1_AS_ed][
                , CNO2_AS_raw := testing_list_t1un$CNO2_AS_raw][
                  , CNO2_AS_ed := testing_list_t1un$CNO2_AS_ed]



results_t1un_Sort_ConRedad_SinPesos <- results_t1un_ConRedad_SinPesos[order(-PobErr_t1un_ConRedad_SinPesos)][, priority := .I]
results_t1un_Sort_ConRedad_SinPesos_m1 <- results_t1un_ConRedad_SinPesos[order(-PobErr_t1un_ConRedad_SinPesos_M)][, priority := .I]


###########           Curva ROC         ###########
roc1 <- roc(response = results_t1un_ConRedad_SinPesos$target1, 
            predictor = results_t1un_ConRedad_SinPesos$PobErr_t1un_ConRedad_SinPesos)
auc1 <- auc(roc1)
thrsh1 <- coords(roc1, "best", ret = "threshold")
spec1 <- coords(roc1, "best", ret = "spec")
sens1 <- coords(roc1, "best", ret = "sens")
par(mar=c(5.1, 4.1, 10, 2.1))
plot(roc1, col = 'blue', lwd = 3,
     xlim = c(1,0), ylim = c(0,1),
     print.auc = FALSE, auc.polygon = FALSE,
     main = 'Target1-Unbalanced-Con RangoEdad-Sin Pesos')

rocm1 <- roc(response = results_t1un_ConRedad_SinPesos$target1, 
             predictor = results_t1un_ConRedad_SinPesos$PobErr_t1un_ConRedad_SinPesos_M, 
             col = 'red', xlim = c(0,1), ylim = c(0,1))
aucm1 <- auc(rocm1)
thrshm1 <- coords(rocm1, "best", ret = "threshold")
specm1 <- coords(rocm1, "best", ret = "spec")
sensm1 <- coords(rocm1, "best", ret = "sens")
plot(rocm1, add = TRUE, col = 'red', lty = 3, lwd = 3,
     xlim = c(1,0), ylim = c(0,1),
     print.auc = FALSE, auc.polygon = FALSE)
legend(0.5, 0.3, legend = c("Without Sampling Weight", "With Sampling Weight"),
       col = c("blue", "red"), lty=c(1, 3), cex = 0.9, bty = "n")
text(0.8, 0.95, paste0('AUC without:', round(auc1, 2)))
text(0.8, 0.9, paste0('AUC with:', round(aucm1, 2)))
#  minor.tick(nx=10, ny=10)
points(x = spec1, y = sens1, col = 'blue', pch = 19, cex = 1.5)
points(x = specm1, y = sensm1, col = 'red', pch = 19, cex = 1.5)


#############
#Matriz de confusión
############
pred_t1un_ConRedad_SinPesos_mc <- predict(model_t1un_ConRedad_SinPesos, 
                                       testing_list_t1un[, ..vars_ConRedad_SinPesos])


confusionMatrix(data = pred_t1un_ConRedad_SinPesos_mc, 
                reference = as.factor(testing_list_t1un$target1))





#================    TARGET1 -UNBALANCED    =================#
#============================================================#
#================          CON PESOS        =================#
#================          SIN RANGOEDAD    =================#
#============================================================#

###########             Modelo              ###########
set.seed(1)
model_t1un_SinRedad_ConPesos <- tuneRF(training_list_t1un[, ..vars_SinRedad_ConPesos], 
                                       training_list_t1un$target1, ntreeTry=1000, 
                                       stepFactor=4, improve=0.05, trace=TRUE, plot=TRUE, doBest = TRUE)

saveRDS(object = model_t1un_ConRedad_SinPesos, file = "N:/UDMTD12/Enrique-EMOS/TFM/Random Forest/Modelo_RF_SinRedad_ConPesos.rds")


###########           Predicción             ###########
pred_t1un_SinRedad_ConPesos <- predict(model_t1un_SinRedad_ConPesos, 
                                       testing_list_t1un[, ..vars_SinRedad_ConPesos], type = "prob")
pred_t1un_SinRedad_ConPesos <- pred_t1un_SinRedad_ConPesos[, 2]


###########           Análisis de resultados              ###########

results_t1un_SinRedad_ConPesos <- data.table(PobErr_t1un_SinRedad_ConPesos = pred_t1un_SinRedad_ConPesos, 
                                             target1 = testing_list_t1un$target1
)


results_t1un_SinRedad_ConPesos[
  , w := testing_list_t1un$FACTORADULTO][
    , m1 := PobErr_t1un_SinRedad_ConPesos * w][
      , PobErr_t1un_SinRedad_ConPesos_M := ecdf(m1)(m1)][
        , target1 := as.numeric(target1) - 1][
          , IDENTHOGAR := testing_list_t1un$IDENTHOGAR][
            , CNO1_AS_raw := testing_list_t1un$CNO1_AS_raw][
              , CNO1_AS_ed := testing_list_t1un$CNO1_AS_ed][
                , CNO2_AS_raw := testing_list_t1un$CNO2_AS_raw][
                  , CNO2_AS_ed := testing_list_t1un$CNO2_AS_ed]



results_t1un_Sort_SinRedad_ConPesos <- results_t1un_SinRedad_ConPesos[order(-PobErr_t1un_SinRedad_ConPesos)][, priority := .I]
results_t1un_Sort_SinRedad_ConPesos_m1 <- results_t1un_SinRedad_ConPesos[order(-PobErr_t1un_SinRedad_ConPesos_M)][, priority := .I]


###########           Curva ROC         ###########
roc1 <- roc(response = results_t1un_SinRedad_ConPesos$target1, 
            predictor = results_t1un_SinRedad_ConPesos$PobErr_t1un_SinRedad_ConPesos)
auc1 <- auc(roc1)
thrsh1 <- coords(roc1, "best", ret = "threshold")
spec1 <- coords(roc1, "best", ret = "spec")
sens1 <- coords(roc1, "best", ret = "sens")
par(mar=c(5.1, 4.1, 10, 2.1))
plot(roc1, col = 'blue', lwd = 3,
     xlim = c(1,0), ylim = c(0,1),
     print.auc = FALSE, auc.polygon = FALSE,
     main = 'Target1-Unbalanced-Sin RangoEdad-Con Pesos')

rocm1 <- roc(response = results_t1un_SinRedad_ConPesos$target1, 
             predictor = results_t1un_SinRedad_ConPesos$PobErr_t1un_SinRedad_ConPesos_M, 
             col = 'red', xlim = c(0,1), ylim = c(0,1))
aucm1 <- auc(rocm1)
thrshm1 <- coords(rocm1, "best", ret = "threshold")
specm1 <- coords(rocm1, "best", ret = "spec")
sensm1 <- coords(rocm1, "best", ret = "sens")
plot(rocm1, add = TRUE, col = 'red', lty = 3, lwd = 3,
     xlim = c(1,0), ylim = c(0,1),
     print.auc = FALSE, auc.polygon = FALSE)
legend(0.5, 0.3, legend = c("Without Sampling Weight", "With Sampling Weight"),
       col = c("blue", "red"), lty=c(1, 3), cex = 0.9, bty = "n")
text(0.8, 0.95, paste0('AUC without:', round(auc1, 2)))
text(0.8, 0.9, paste0('AUC with:', round(aucm1, 2)))
#  minor.tick(nx=10, ny=10)
points(x = spec1, y = sens1, col = 'blue', pch = 19, cex = 1.5)
points(x = specm1, y = sensm1, col = 'red', pch = 19, cex = 1.5)


#############
#Matriz de confusión
############
pred_t1un_SinRedad_ConPesos_mc <- predict(model_t1un_SinRedad_ConPesos, 
                                          testing_list_t1un[, ..vars_SinRedad_ConPesos])


confusionMatrix(data = pred_t1un_SinRedad_ConPesos_mc, 
                reference = as.factor(testing_list_t1un$target1))




#================    TARGET1 -UNBALANCED    =================#
#============================================================#
#================          CON PESOS        =================#
#================          CON RANGOEDAD    =================#
#============================================================#

###########             Modelo              ###########
set.seed(1)
model_t1un_ConRedad_ConPesos <- tuneRF(training_list_t1un[, ..vars_ConRedad_ConPesos], 
                                       training_list_t1un$target1, ntreeTry=1000, 
                                       stepFactor=4, improve=0.05, trace=TRUE, plot=TRUE, doBest = TRUE)

saveRDS(object = model_t1un_ConRedad_ConPesos, file = "N:/UDMTD12/Enrique-EMOS/TFM/Random Forest/Modelo_RF_ConRedad_ConPesos.rds")


###########           Predicción             ###########
pred_t1un_ConRedad_ConPesos <- predict(model_t1un_ConRedad_ConPesos, 
                                       testing_list_t1un[, ..vars_ConRedad_ConPesos], type = "prob")
pred_t1un_ConRedad_ConPesos <- pred_t1un_ConRedad_ConPesos[, 2]


###########           Análisis de resultados              ###########

results_t1un_ConRedad_ConPesos <- data.table(PobErr_t1un_ConRedad_ConPesos = pred_t1un_ConRedad_ConPesos, 
                                             target1 = testing_list_t1un$target1
)


results_t1un_ConRedad_ConPesos[
  , w := testing_list_t1un$FACTORADULTO][
    , m1 := PobErr_t1un_ConRedad_ConPesos * w][
      , PobErr_t1un_ConRedad_ConPesos_M := ecdf(m1)(m1)][
        , target1 := as.numeric(target1) - 1][
          , IDENTHOGAR := testing_list_t1un$IDENTHOGAR][
            , CNO1_AS_raw := testing_list_t1un$CNO1_AS_raw][
              , CNO1_AS_ed := testing_list_t1un$CNO1_AS_ed][
                , CNO2_AS_raw := testing_list_t1un$CNO2_AS_raw][
                  , CNO2_AS_ed := testing_list_t1un$CNO2_AS_ed]



results_t1un_Sort_ConRedad_ConPesos <- results_t1un_ConRedad_ConPesos[order(-PobErr_t1un_ConRedad_ConPesos)][, priority := .I]
results_t1un_Sort_ConRedad_ConPesos_m1 <- results_t1un_ConRedad_ConPesos[order(-PobErr_t1un_ConRedad_ConPesos_M)][, priority := .I]


###########           Curva ROC         ###########
roc1 <- roc(response = results_t1un_ConRedad_ConPesos$target1, 
            predictor = results_t1un_ConRedad_ConPesos$PobErr_t1un_ConRedad_ConPesos)
auc1 <- auc(roc1)
thrsh1 <- coords(roc1, "best", ret = "threshold")
spec1 <- coords(roc1, "best", ret = "spec")
sens1 <- coords(roc1, "best", ret = "sens")
par(mar=c(5.1, 4.1, 10, 2.1))
plot(roc1, col = 'blue', lwd = 3,
     xlim = c(1,0), ylim = c(0,1),
     print.auc = FALSE, auc.polygon = FALSE,
     main = 'Target1-Unbalanced-Con RangoEdad-Con Pesos')

rocm1 <- roc(response = results_t1un_ConRedad_ConPesos$target1, 
             predictor = results_t1un_ConRedad_ConPesos$PobErr_t1un_ConRedad_ConPesos_M, 
             col = 'red', xlim = c(0,1), ylim = c(0,1))
aucm1 <- auc(rocm1)
thrshm1 <- coords(rocm1, "best", ret = "threshold")
specm1 <- coords(rocm1, "best", ret = "spec")
sensm1 <- coords(rocm1, "best", ret = "sens")
plot(rocm1, add = TRUE, col = 'red', lty = 3, lwd = 3,
     xlim = c(1,0), ylim = c(0,1),
     print.auc = FALSE, auc.polygon = FALSE)
legend(0.5, 0.3, legend = c("Without Sampling Weight", "With Sampling Weight"),
       col = c("blue", "red"), lty=c(1, 3), cex = 0.9, bty = "n")
text(0.8, 0.95, paste0('AUC without:', round(auc1, 2)))
text(0.8, 0.9, paste0('AUC with:', round(aucm1, 2)))
#  minor.tick(nx=10, ny=10)
points(x = spec1, y = sens1, col = 'blue', pch = 19, cex = 1.5)
points(x = specm1, y = sensm1, col = 'red', pch = 19, cex = 1.5)


#############
#Matriz de confusión
############
pred_t1un_ConRedad_ConPesos_mc <- predict(model_t1un_ConRedad_ConPesos, 
                                       testing_list_t1un[, ..vars_ConRedad_ConPesos])


confusionMatrix(data = pred_t1un_ConRedad_ConPesos_mc, 
                reference = as.factor(testing_list_t1un$target1))




#============================================================#
#============================================================#
#================                           =================#
#================          GRÁFICOS         =================#
#================     EN UNA SOLA SALIDA    =================#
#================                           =================#
#============================================================#
#============================================================#
par(mfrow = c(2, 4))
# Balanced, Sin RangoEdad, SinPeso
roc1 <- roc(response = results_t1_SinRedad_SinPesos$target1, 
            predictor = results_t1_SinRedad_SinPesos$PobErr_t1_SinRedad_SinPesos)
auc1 <- auc(roc1)
thrsh1 <- coords(roc1, "best", ret = "threshold")
spec1 <- coords(roc1, "best", ret = "spec")
sens1 <- coords(roc1, "best", ret = "sens")
par(mar=c(5.1, 4.1, 10, 2.1))
plot(roc1, col = 'blue', lwd = 3,
     xlim = c(1,0), ylim = c(0,1),
     print.auc = FALSE, auc.polygon = FALSE,
     main = 'Target1-Balanced-Sin RangoEdad-Sin Pesos')

rocm1 <- roc(response = results_t1_SinRedad_SinPesos$target1, 
             predictor = results_t1_SinRedad_SinPesos$PobErr_t1_SinRedad_SinPesos_M, 
             col = 'red', xlim = c(0,1), ylim = c(0,1))
aucm1 <- auc(rocm1)
thrshm1 <- coords(rocm1, "best", ret = "threshold")
specm1 <- coords(rocm1, "best", ret = "spec")
sensm1 <- coords(rocm1, "best", ret = "sens")
plot(rocm1, add = TRUE, col = 'red', lty = 3, lwd = 3,
     xlim = c(1,0), ylim = c(0,1),
     print.auc = FALSE, auc.polygon = FALSE)
legend(0.5, 0.3, legend = c("Without Sampling Weight", "With Sampling Weight"),
       col = c("blue", "red"), lty=c(1, 3), cex = 0.9, bty = "n")
text(0.8, 0.95, paste0('AUC without:', round(auc1, 2)))
text(0.8, 0.9, paste0('AUC with:', round(aucm1, 2)))
#  minor.tick(nx=10, ny=10)
points(x = spec1, y = sens1, col = 'blue', pch = 19, cex = 1.5)
points(x = specm1, y = sensm1, col = 'red', pch = 19, cex = 1.5)

# Balanced, Con RangoEdad, SinPeso
roc1 <- roc(response = results_t1_ConRedad_SinPesos$target1, 
            predictor = results_t1_ConRedad_SinPesos$PobErr_t1_ConRedad_SinPesos)
auc1 <- auc(roc1)
thrsh1 <- coords(roc1, "best", ret = "threshold")
spec1 <- coords(roc1, "best", ret = "spec")
sens1 <- coords(roc1, "best", ret = "sens")
par(mar=c(5.1, 4.1, 10, 2.1))
plot(roc1, col = 'blue', lwd = 3,
     xlim = c(1,0), ylim = c(0,1),
     print.auc = FALSE, auc.polygon = FALSE,
     main = 'Target1-Balanced-Con RangoEdad-Sin Pesos')

rocm1 <- roc(response = results_t1_ConRedad_SinPesos$target1, 
             predictor = results_t1_ConRedad_SinPesos$PobErr_t1_ConRedad_SinPesos_M, 
             col = 'red', xlim = c(0,1), ylim = c(0,1))
aucm1 <- auc(rocm1)
thrshm1 <- coords(rocm1, "best", ret = "threshold")
specm1 <- coords(rocm1, "best", ret = "spec")
sensm1 <- coords(rocm1, "best", ret = "sens")
plot(rocm1, add = TRUE, col = 'red', lty = 3, lwd = 3,
     xlim = c(1,0), ylim = c(0,1),
     print.auc = FALSE, auc.polygon = FALSE)
legend(0.5, 0.3, legend = c("Without Sampling Weight", "With Sampling Weight"),
       col = c("blue", "red"), lty=c(1, 3), cex = 0.9, bty = "n")
text(0.8, 0.95, paste0('AUC without:', round(auc1, 2)))
text(0.8, 0.9, paste0('AUC with:', round(aucm1, 2)))
#  minor.tick(nx=10, ny=10)
points(x = spec1, y = sens1, col = 'blue', pch = 19, cex = 1.5)
points(x = specm1, y = sensm1, col = 'red', pch = 19, cex = 1.5)

# Balanced, Sin Rango Edad, Con Pesos
roc1 <- roc(response = results_t1_SinRedad_ConPesos$target1, 
            predictor = results_t1_SinRedad_ConPesos$PobErr_t1_SinRedad_ConPesos)
auc1 <- auc(roc1)
thrsh1 <- coords(roc1, "best", ret = "threshold")
spec1 <- coords(roc1, "best", ret = "spec")
sens1 <- coords(roc1, "best", ret = "sens")
par(mar=c(5.1, 4.1, 10, 2.1))
plot(roc1, col = 'blue', lwd = 3,
     xlim = c(1,0), ylim = c(0,1),
     print.auc = FALSE, auc.polygon = FALSE,
     main = 'Target1-Balanced-Sin RangoEdad-Con Pesos')

rocm1 <- roc(response = results_t1_SinRedad_ConPesos$target1, 
             predictor = results_t1_SinRedad_ConPesos$PobErr_t1_SinRedad_ConPesos_M, 
             col = 'red', xlim = c(0,1), ylim = c(0,1))
aucm1 <- auc(rocm1)
thrshm1 <- coords(rocm1, "best", ret = "threshold")
specm1 <- coords(rocm1, "best", ret = "spec")
sensm1 <- coords(rocm1, "best", ret = "sens")
plot(rocm1, add = TRUE, col = 'red', lty = 3, lwd = 3,
     xlim = c(1,0), ylim = c(0,1),
     print.auc = FALSE, auc.polygon = FALSE)
legend(0.5, 0.3, legend = c("Without Sampling Weight", "With Sampling Weight"),
       col = c("blue", "red"), lty=c(1, 3), cex = 0.9, bty = "n")
text(0.8, 0.95, paste0('AUC without:', round(auc1, 2)))
text(0.8, 0.9, paste0('AUC with:', round(aucm1, 2)))
#  minor.tick(nx=10, ny=10)
points(x = spec1, y = sens1, col = 'blue', pch = 19, cex = 1.5)
points(x = specm1, y = sensm1, col = 'red', pch = 19, cex = 1.5)


roc1 <- roc(response = results_t1_ConRedad_ConPesos$target1, 
            predictor = results_t1_ConRedad_ConPesos$PobErr_t1_ConRedad_ConPesos)
auc1 <- auc(roc1)
thrsh1 <- coords(roc1, "best", ret = "threshold")
spec1 <- coords(roc1, "best", ret = "spec")
sens1 <- coords(roc1, "best", ret = "sens")
par(mar=c(5.1, 4.1, 10, 2.1))
plot(roc1, col = 'blue', lwd = 3,
     xlim = c(1,0), ylim = c(0,1),
     print.auc = FALSE, auc.polygon = FALSE,
     main = 'Target1-Balanced-Con RangoEdad-Con Pesos')

# Balanced, Con Rango Edad, Con Pesos
rocm1 <- roc(response = results_t1_ConRedad_ConPesos$target1, 
             predictor = results_t1_ConRedad_ConPesos$PobErr_t1_ConRedad_ConPesos_M, 
             col = 'red', xlim = c(0,1), ylim = c(0,1))
aucm1 <- auc(rocm1)
thrshm1 <- coords(rocm1, "best", ret = "threshold")
specm1 <- coords(rocm1, "best", ret = "spec")
sensm1 <- coords(rocm1, "best", ret = "sens")
plot(rocm1, add = TRUE, col = 'red', lty = 3, lwd = 3,
     xlim = c(1,0), ylim = c(0,1),
     print.auc = FALSE, auc.polygon = FALSE)
legend(0.5, 0.3, legend = c("Without Sampling Weight", "With Sampling Weight"),
       col = c("blue", "red"), lty=c(1, 3), cex = 0.9, bty = "n")
text(0.8, 0.95, paste0('AUC without:', round(auc1, 2)))
text(0.8, 0.9, paste0('AUC with:', round(aucm1, 2)))
#  minor.tick(nx=10, ny=10)
points(x = spec1, y = sens1, col = 'blue', pch = 19, cex = 1.5)
points(x = specm1, y = sensm1, col = 'red', pch = 19, cex = 1.5)

roc1 <- roc(response = results_t1un_SinRedad_SinPesos$target1, 
            predictor = results_t1un_SinRedad_SinPesos$PobErr_t1un_SinRedad_SinPesos)
auc1 <- auc(roc1)
thrsh1 <- coords(roc1, "best", ret = "threshold")
spec1 <- coords(roc1, "best", ret = "spec")
sens1 <- coords(roc1, "best", ret = "sens")
par(mar=c(5.1, 4.1, 10, 2.1))
plot(roc1, col = 'blue', lwd = 3,
     xlim = c(1,0), ylim = c(0,1),
     print.auc = FALSE, auc.polygon = FALSE,
     main = 'Target1-Unbalanced-Sin RangoEdad-Sin Pesos')

rocm1 <- roc(response = results_t1un_SinRedad_SinPesos$target1, 
             predictor = results_t1un_SinRedad_SinPesos$PobErr_t1un_SinRedad_SinPesos_M, 
             col = 'red', xlim = c(0,1), ylim = c(0,1))
aucm1 <- auc(rocm1)
thrshm1 <- coords(rocm1, "best", ret = "threshold")
specm1 <- coords(rocm1, "best", ret = "spec")
sensm1 <- coords(rocm1, "best", ret = "sens")
plot(rocm1, add = TRUE, col = 'red', lty = 3, lwd = 3,
     xlim = c(1,0), ylim = c(0,1),
     print.auc = FALSE, auc.polygon = FALSE)
legend(0.5, 0.3, legend = c("Without Sampling Weight", "With Sampling Weight"),
       col = c("blue", "red"), lty=c(1, 3), cex = 0.9, bty = "n")
text(0.8, 0.95, paste0('AUC without:', round(auc1, 2)))
text(0.8, 0.9, paste0('AUC with:', round(aucm1, 2)))
#  minor.tick(nx=10, ny=10)
points(x = spec1, y = sens1, col = 'blue', pch = 19, cex = 1.5)
points(x = specm1, y = sensm1, col = 'red', pch = 19, cex = 1.5)


#================    TARGET1 -UNBALANCED    =================#
#============================================================#
#================          SIN PESOS        =================#
#================          CON RANGOEDAD    =================#
#============================================================#
###########           Curva ROC         ###########
roc1 <- roc(response = results_t1un_ConRedad_SinPesos$target1, 
            predictor = results_t1un_ConRedad_SinPesos$PobErr_t1un_ConRedad_SinPesos)
auc1 <- auc(roc1)
thrsh1 <- coords(roc1, "best", ret = "threshold")
spec1 <- coords(roc1, "best", ret = "spec")
sens1 <- coords(roc1, "best", ret = "sens")
par(mar=c(5.1, 4.1, 10, 2.1))
plot(roc1, col = 'blue', lwd = 3,
     xlim = c(1,0), ylim = c(0,1),
     print.auc = FALSE, auc.polygon = FALSE,
     main = 'Target1-Unbalanced-Con RangoEdad-Sin Pesos')

rocm1 <- roc(response = results_t1un_ConRedad_SinPesos$target1, 
             predictor = results_t1un_ConRedad_SinPesos$PobErr_t1un_ConRedad_SinPesos_M, 
             col = 'red', xlim = c(0,1), ylim = c(0,1))
aucm1 <- auc(rocm1)
thrshm1 <- coords(rocm1, "best", ret = "threshold")
specm1 <- coords(rocm1, "best", ret = "spec")
sensm1 <- coords(rocm1, "best", ret = "sens")
plot(rocm1, add = TRUE, col = 'red', lty = 3, lwd = 3,
     xlim = c(1,0), ylim = c(0,1),
     print.auc = FALSE, auc.polygon = FALSE)
legend(0.5, 0.3, legend = c("Without Sampling Weight", "With Sampling Weight"),
       col = c("blue", "red"), lty=c(1, 3), cex = 0.9, bty = "n")
text(0.8, 0.95, paste0('AUC without:', round(auc1, 2)))
text(0.8, 0.9, paste0('AUC with:', round(aucm1, 2)))
#  minor.tick(nx=10, ny=10)
points(x = spec1, y = sens1, col = 'blue', pch = 19, cex = 1.5)
points(x = specm1, y = sensm1, col = 'red', pch = 19, cex = 1.5)


#================    TARGET1 -UNBALANCED    =================#
#============================================================#
#================          CON PESOS        =================#
#================          SIN RANGOEDAD    =================#
#============================================================#
###########           Curva ROC         ###########
roc1 <- roc(response = results_t1un_SinRedad_ConPesos$target1, 
            predictor = results_t1un_SinRedad_ConPesos$PobErr_t1un_SinRedad_ConPesos)
auc1 <- auc(roc1)
thrsh1 <- coords(roc1, "best", ret = "threshold")
spec1 <- coords(roc1, "best", ret = "spec")
sens1 <- coords(roc1, "best", ret = "sens")
par(mar=c(5.1, 4.1, 10, 2.1))
plot(roc1, col = 'blue', lwd = 3,
     xlim = c(1,0), ylim = c(0,1),
     print.auc = FALSE, auc.polygon = FALSE,
     main = 'Target1-Unbalanced-Sin RangoEdad-Con Pesos')

rocm1 <- roc(response = results_t1un_SinRedad_ConPesos$target1, 
             predictor = results_t1un_SinRedad_ConPesos$PobErr_t1un_SinRedad_ConPesos_M, 
             col = 'red', xlim = c(0,1), ylim = c(0,1))
aucm1 <- auc(rocm1)
thrshm1 <- coords(rocm1, "best", ret = "threshold")
specm1 <- coords(rocm1, "best", ret = "spec")
sensm1 <- coords(rocm1, "best", ret = "sens")
plot(rocm1, add = TRUE, col = 'red', lty = 3, lwd = 3,
     xlim = c(1,0), ylim = c(0,1),
     print.auc = FALSE, auc.polygon = FALSE)
legend(0.5, 0.3, legend = c("Without Sampling Weight", "With Sampling Weight"),
       col = c("blue", "red"), lty=c(1, 3), cex = 0.9, bty = "n")
text(0.8, 0.95, paste0('AUC without:', round(auc1, 2)))
text(0.8, 0.9, paste0('AUC with:', round(aucm1, 2)))
#  minor.tick(nx=10, ny=10)
points(x = spec1, y = sens1, col = 'blue', pch = 19, cex = 1.5)
points(x = specm1, y = sensm1, col = 'red', pch = 19, cex = 1.5)


#================    TARGET1 -UNBALANCED    =================#
#============================================================#
#================          CON PESOS        =================#
#================          CON RANGOEDAD    =================#
#============================================================#
###########           Curva ROC         ###########
roc1 <- roc(response = results_t1un_ConRedad_ConPesos$target1, 
            predictor = results_t1un_ConRedad_ConPesos$PobErr_t1un_ConRedad_ConPesos)
auc1 <- auc(roc1)
thrsh1 <- coords(roc1, "best", ret = "threshold")
spec1 <- coords(roc1, "best", ret = "spec")
sens1 <- coords(roc1, "best", ret = "sens")
par(mar=c(5.1, 4.1, 10, 2.1))
plot(roc1, col = 'blue', lwd = 3,
     xlim = c(1,0), ylim = c(0,1),
     print.auc = FALSE, auc.polygon = FALSE,
     main = 'Target1-Unbalanced-Con RangoEdad-Con Pesos')

rocm1 <- roc(response = results_t1un_ConRedad_ConPesos$target1, 
             predictor = results_t1un_ConRedad_ConPesos$PobErr_t1un_ConRedad_ConPesos_M, 
             col = 'red', xlim = c(0,1), ylim = c(0,1))
aucm1 <- auc(rocm1)
thrshm1 <- coords(rocm1, "best", ret = "threshold")
specm1 <- coords(rocm1, "best", ret = "spec")
sensm1 <- coords(rocm1, "best", ret = "sens")
plot(rocm1, add = TRUE, col = 'red', lty = 3, lwd = 3,
     xlim = c(1,0), ylim = c(0,1),
     print.auc = FALSE, auc.polygon = FALSE)
legend(0.5, 0.3, legend = c("Without Sampling Weight", "With Sampling Weight"),
       col = c("blue", "red"), lty=c(1, 3), cex = 0.9, bty = "n")
text(0.8, 0.95, paste0('AUC without:', round(auc1, 2)))
text(0.8, 0.9, paste0('AUC with:', round(aucm1, 2)))
#  minor.tick(nx=10, ny=10)
points(x = spec1, y = sens1, col = 'blue', pch = 19, cex = 1.5)
points(x = specm1, y = sensm1, col = 'red', pch = 19, cex = 1.5)

