#####################################################################
###########      Carga y lectua de librerias a utilizar   ###########
#####################################################################
library(data.table)
library(tibble)
library(openxlsx)
library(ca)
library(carData)
#library(car)
#library(xlsx)
library(ggplot2)
library(randomForest)
library(kernlab)
library(ranger)
library(lattice)
library(caret)
library(e1071)

#Para ejecutar en paralelo
#install.packages("doMC", repos="http://R-Forge.R-project.org")
library(doMC)
library(foreach)
library(iterators)
library(parallel)
registerDoMC(cores = 3)

library(magrittr) #Introduce: "%>%"->"decrease development time and to improve readability and maintainability of code."
library(dplyr) # Uso de la función "gather"
library(tidyr) # Uso de la función "gather"

library(smotefamily)   #Paquete con técnicas de muestreo para corregir imbalanced data
library(DMwR)   #Paquete con técnicas de muestreo para corregir imbalanced data

#####################################################################
###########            Lectura de los ficheros            ###########
#####################################################################

training_list <- readRDS(file = "N:/UDMTD/UDMTD12/Enrique-EMOS/FicherosR/train.rds")
testing_list <- readRDS(file = "N:/UDMTD/UDMTD12/Enrique-EMOS/FicherosR/test.rds")

totaldata <- rbind(training_list, testing_list)



#####################################################################
###########        Recodificar la variables TARGET          ###########
#####################################################################
levels(totaldata$target1) <- make.names(levels(factor(totaldata$target1)))


#####################################################################
###########        Recodificar la variable EDAD          ###########
#####################################################################
# totaldata$RANGOEDAD <- Recode(totaldata$EDADa, 
#                                  '0:30="Menor de 29"; 30:39="30-39"; 40:49="40-49"; 50:59="50-59"; 60:99="Mayor 60"',
#                                  levels = c("Menor de 30","30-39","40-49","50-59","Mayor 60"))

RANGOEDAD <- vector()
RANGOEDAD[totaldata$EDADa < 30] <- 1
RANGOEDAD[totaldata$EDADa >= 30 & totaldata$EDADa < 40] <- 2
RANGOEDAD[totaldata$EDADa >= 40 & totaldata$EDADa < 50] <- 3
RANGOEDAD[totaldata$EDADa >= 50 & totaldata$EDADa < 60] <- 4
RANGOEDAD[totaldata$EDADa >= 60] <- 5
totaldata$RANGOEDAD <- as.factor(RANGOEDAD)
levels(totaldata$RANGOEDAD) <- c("Menor de 30", "30-39", "40-49", "50-59","Mayor 60")
table(totaldata$RANGOEDAD)


#####################################################################
###########       Creación de fichero Test y Train        ###########
#####################################################################
set.seed(123)
train <- createDataPartition(y = totaldata$target1, p = 0.8, list = FALSE, times = 1)
#la funci?n createDataPartition() garantiza una distribuci?n aproximada (reparto estratificado)
datos_train_aux <- totaldata[train, ]
datos_test  <- totaldata[-train, ]

datos_train_aux[, IDENTHOGAR := as.factor(IDENTHOGAR)]

datos_train <- SMOTE(target1 ~., datos_train_aux,
        perc.over = 50,
        k = 6,
        perc.under = 400)
  



#Verificaci?n de que la distribuci?n de la variable respuesta (target1) es similar 
#en el conjunto de entrenamiento y en el de test.
distribucion_train <- prop.table(table(datos_train$target1))
distribucion_test  <- prop.table(table(datos_test$target1))
data.frame(train = distribucion_train, test = distribucion_test )


#####################################################################
###########     Estandarización de datos num?ricos        ###########
#####################################################################
# estandarizador <- preProcess(x = datos_train, method = c("center", "scale"))
# datos_train    <- predict(object = estandarizador, newdata = datos_train)
# datos_test     <- predict(object = estandarizador, newdata = datos_test)
# 

#####################################################################
###########     Modelos que vamos a probar para el CNO    ###########
#####################################################################
library(collapsibleTree)
analisis <- data.frame(
  modelo   = rep(c("SVM", "RandomForest", "Neural Network"), each = 3),
  target = rep(c("Target1", "Target2", "Target3"), times = 3)
)

collapsibleTree(df = analisis,
                hierarchy = c("modelo", "target"),
                collapsed = FALSE,
                zoomable = FALSE,
                fill = c("#cacfd2", "#d98880", "#7fb3d5", "#82e0aa",
                         rep(c("#d98880", "#7fb3d5", "#82e0aa"), each = 3)))

#####################################################################
###########     Variables a considerar para el modelo     ###########
#####################################################################
vars <- c('target1','RANGOEDAD', #'EDADa',
          'SEXOa', 'ESTRATO','PROXY_0', 
          'CNAE_AS_1', 'CNAE_AS_2', 'CNAE_AS_3', 
          'CNO_AS_1', 'CNO_AS_2', 'CNO_AS_3',
          'F18', 'A10_i','A7_2a','CCAA'
          )


#####################################################################
###########            Modelo SVN KERNEL RADIAL           ###########
#####################################################################

# HIPERPARÁMETROS, N?MERO DE REPETICIONES Y SEMILLAS PARA CADA REPETICIÓN
repeticiones_boot <- 100

# Hiperparámetros
hiperparametros <- expand.grid(sigma = c(0.0001, 0.001, 0.01),
                               C = c(1, 10, 50, 100, 250, 500, 700, 1000))

set.seed(123)
seeds <- vector(mode = "list", length = repeticiones_boot + 1)
for (i in 1:repeticiones_boot) {
  seeds[[i]] <- sample.int(1000, nrow(hiperparametros))
}
seeds[[repeticiones_boot + 1]] <- sample.int(1000, 1)

# DEFINICIÓN DEL ENTRENAMIENTO
control_train <- trainControl(method = "boot", number = repeticiones_boot,
                              seeds = seeds, returnResamp = "final",
                              verboseIter = FALSE, allowParallel = TRUE,
                              classProbs = TRUE)
# AJUSTE DEL MODELO
set.seed(342)
svmrad_target1 <- train(
  form = target1 ~ .,
  data = datos_train[,..vars],
  method = "svmRadial",
  tuneGrid = hiperparametros,
  metric = "Accuracy",
  trControl = control_train
)


# REPRESENTACIÓN GRÁFICA
ggplot(svmrad_target1, highlight = TRUE) +
  labs(title = "Evolución del accuracy del modelo SVM Radial") +
  theme_bw()



#####################################################################
###########             Modelo RANDOM FOREST              ###########
#####################################################################


# HIPERPARÁMETROS, NÚMERO DE REPETICIONES Y SEMILLAS PARA CADA REPETICIÓN
repeticiones_boot <- 100

# Hiperparámetros
hiperparametros <- expand.grid(mtry = c(2, 5, 10, 12),
                               min.node.size = c(2, 3, 4, 5, 10),
                               splitrule = "gini")


set.seed(123)
seeds <- vector(mode = "list", length = repeticiones_boot + 1)
for (i in 1:repeticiones_boot) {
  seeds[[i]] <- sample.int(1000, nrow(hiperparametros))
}
seeds[[repeticiones_boot + 1]] <- sample.int(1000, 1)

# DEFINICIÓN DEL ENTRENAMIENTO
control_train <- trainControl(method = "boot", number = repeticiones_boot,
                              seeds = seeds, returnResamp = "final",
                              verboseIter = FALSE, allowParallel = TRUE,
                              classProbs = TRUE)

# AJUSTE DEL MODELO
set.seed(342)
randfor_target1 <- train(
  form = target1 ~ .,
  data = datos_train[,..vars],
  method = "ranger",
  tuneGrid = hiperparametros,
  metric = "Accuracy",
  trControl = control_train,
  # Número de árboles ajustados
  num.trees = 500)


# REPRESENTACIÓN GRÁFICA
ggplot(randfor_target1, highlight = TRUE) +
  labs(title = "Evolución del accuracy del modelo Random Forest") +
  guides(color = guide_legend(title = "mtry"),
         shape = guide_legend(title = "mtry")) +
  theme_bw()




#####################################################################
###########             Modelo NEURAL NETWORK             ###########
#####################################################################

# HIPERPARÁMETROS, NÚMERO DE REPETICIONES Y SEMILLAS PARA CADA REPETICIÓN
repeticiones_boot <- 100

# Hiperparámetros
hiperparametros <- expand.grid(size = c(5, 10, 15, 20, 40),
                               decay = c(0.01, 0.1))

set.seed(123)
seeds <- vector(mode = "list", length = repeticiones_boot + 1)
for (i in 1:repeticiones_boot) {
  seeds[[i]] <- sample.int(1000, nrow(hiperparametros))
}
seeds[[repeticiones_boot + 1]] <- sample.int(1000, 1)

# DEFINICIÓN DEL ENTRENAMIENTO
control_train <- trainControl(method = "boot", number = repeticiones_boot,
                              seeds = seeds, returnResamp = "final",
                              verboseIter = FALSE, allowParallel = TRUE,
                              classProbs = TRUE)

# AJUSTE DEL MODELO
set.seed(342)
nnet_target1 <- train(
  form = target1 ~ .,
  data = datos_train[,..vars],
  method = "nnet",
  tuneGrid = hiperparametros,
  metric = "Accuracy",
  trControl = control_train,
  # Rango de inicialización de los pesos
  rang = c(-0.7, 0.7),
  # Número máximo de pesos
  MaxNWts = 10000,
  # Para que no se muestre cada iteración por pantalla
  trace = FALSE
)

# REPRESENTACIÓN GRÁFICA
ggplot(nnet_target1, highlight = TRUE) +
  labs(title = "Evolución del accuracy del modelo NNET") +
  theme_bw()



#####################################################################
###########             Exportamos los modelos            ###########
#####################################################################
saveRDS(object = svmrad_target1, file = "N:/UDMTD/UDMTD12/Enrique-EMOS/TFM/svmrad_target1_smote_prob")
saveRDS(object = randfor_target1, file = "N:/UDMTD/UDMTD12/Enrique-EMOS/TFM/randfor_target1_prob.rds")
saveRDS(object = nnet_target1, file = "N:/UDMTD/UDMTD12/Enrique-EMOS/TFM/nnet_target1_smote_prob.rds")


#####################################################################
###########             Comparación de modelos            ###########
#####################################################################
svmrad_target1 <- readRDS(file = "N:/UDMTD/UDMTD12/Enrique-EMOS/TFM/svmrad_target1_prob.rds")
randfor_target1 <- readRDS(file = "N:/UDMTD12/Enrique-EMOS/TFM/Random Forest/Modelos_RF_Smote_ConREdad_SinPesos.rds")
nnet_target1 <- readRDS(file = "N:/UDMTD/UDMTD12/Enrique-EMOS/TFM/nnet_target1_prob.rds")

modelos <- list(
  svmrad_target1 = svmrad_target1,
  randfor_target1  = randfor_target1,
  nnet_target1  = nnet_target1
)

resultados_resamples <- resamples(modelos)


# Se trasforma el dataframe devuelto por resamples() para separar el nombre del
# modelo y las métricas en columnas distintas.
metricas_resamples <- resultados_resamples$values %>%
  gather(key = "modelo", value = "valor", -Resample) %>%
  separate(col = "modelo", into = c("modelo", "metrica"),
           sep = "~", remove = TRUE)
# Accuracy y Kappa promedio de cada modelo
promedio_metricas_resamples <- metricas_resamples %>% 
  group_by(modelo, metrica) %>% 
  summarise(media = mean(valor)) %>%
  spread(key = metrica, value = media) %>%
  arrange(desc(Accuracy))

promedio_metricas_resamples

###### gráficos de comparación con las métricas de los modelos
metricas_resamples %>%
  filter(metrica == "Accuracy") %>%
  group_by(modelo) %>% 
  summarise(media = mean(valor)) %>%
  ggplot(aes(x = reorder(modelo, media), y = media, label = round(media, 2))) +
  geom_segment(aes(x = reorder(modelo, media), y = 0,
                   xend = modelo, yend = media),
               color = "grey50") +
  geom_point(size = 8, color = "firebrick") +
  geom_text(color = "white", size = 3) +
  scale_y_continuous(limits = c(0, 1)) +
  # Accuracy basal
  geom_hline(yintercept = 0.156, linetype = "dashed") +
  annotate(geom = "text", y = 0.28, x = 12.5, label = "Accuracy basal") +
  labs(title = "Validación: Accuracy medio repeated-CV",
       subtitle = "Modelos ordenados por media",
       x = "modelo") +
  coord_flip() +
  theme_bw()


metricas_resamples %>% filter(metrica == "Accuracy") %>%
  group_by(modelo) %>% 
  mutate(media = mean(valor)) %>%
  ungroup() %>%
  ggplot(aes(x = reorder(modelo, media), y = valor, color = modelo)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.6) +
  scale_y_continuous(limits = c(0, 1)) +
  # Accuracy basal
  geom_hline(yintercept = 0.156, linetype = "dashed") +
  annotate(geom = "text", y = 0.25, x = 12, label = "Accuracy basal") +
  theme_bw() +
  labs(title = "Validación: Accuracy medio repeated-CV",
       subtitle = "Modelos ordenados por media") +
  coord_flip() +
  theme(legend.position = "none")





#Si se cumple la condición de normalidad, se pueden aplicar un t-test de datos 
#pareados para comparar el accuracy medio de cada modelo.
library(qqplotr)
metricas_resamples %>%
  filter( metrica == "Accuracy") %>%
  ggplot(aes(sample = valor, color = modelo)) +
  stat_qq_band(alpha = 0.5, color = "gray") +
  stat_qq_line() +
  stat_qq_point() +
  theme_bw() +
  theme(legend.position = "none") +
  facet_wrap(~modelo)


#El análisis gráfico no muestra grandes desviaciones de la normal, además, dado que se 
#dispone de más de 30 valores por grupo, el t-test tiene cierta robustez. 
#Se procede a comparar los 3 modelos.
metricas_ttest <- metricas_resamples %>%
  filter( metrica == "Accuracy") %>%
  select(-metrica)

pairwise.t.test(x = metricas_ttest$valor,
                g = metricas_ttest$modelo,
                paired = TRUE,
                # Al ser solo 3 comparaciones, no se añade ajuste de p.value
                p.adjust.method = "none")





#####################################################################
###########             Predicción de modelos             ###########
#####################################################################

predict_svmrad_target1 <- predict(object = svmrad_target1, newdata = datos_test)
predict_randfor_target1  <- predict(object = randfor_target1, newdata = datos_test)
predict_nnet_target1  <- predict(object = nnet_target1, newdata = datos_test)

predicciones <- data.frame(
  svmrad_target1 = predict_svmrad_target1,
  randfor_target1  = predict_randfor_target1,
  nnet_target1  = predict_nnet_target1,
  valor_real        = datos_test$target1
)

predicciones %>% head()


#############
#Vamos a calcular lo que han acertado las predicciones con la precisión 
#que nos ha salido en el modelo
############
calculo_accuracy <- function(x, y){
  return(mean(x == y))
}

library(purrr) # usar la función map_dbl

accuracy_test <- map_dbl(.x = predicciones[, -4],
                         .f = calculo_accuracy,
                         y = predicciones[, 4])
class(accuracy_test)
accuracy_test <- as.data.frame(accuracy_test)
class(accuracy_test)
accuracy_test <- rownames_to_column(accuracy_test, var = "modelo") 
accuracy_test <- arrange(accuracy_test, desc(accuracy_test))

metricas_resamples %>% 
  group_by(modelo, metrica) %>% 
  summarise(media = mean(valor)) %>%
  spread(key = metrica, value = media) %>%
  select(accuracy_validacion = Accuracy) %>%
  left_join(accuracy_test, by = "modelo") %>% 
  arrange(desc(accuracy_test))


# #gráfico de la comparación entre la precisión de la predicción con la precisión
# # que nos ha salido en el modelo
# metricas_resamples %>% 
#   group_by(modelo, metrica) %>% 
#   summarise(media = mean(valor)) %>%
#   spread(key = metrica, value = media) %>%
#   select(accuracy_validacion = Accuracy) %>%
#   left_join(accuracy_test, by = "modelo") %>%
#   gather(key = "datos", value = "accuracy", -modelo) %>%
#   ggplot(aes(x = reorder(modelo, accuracy), y = accuracy,
#              color = datos, label = round(accuracy, 2))) +
#   geom_point(size = 8) +
#   ylim(0, 1) +
#   scale_color_manual(values = c("orangered2", "gray50")) +
#   geom_text(color = "white", size = 3) +
#   # Accuracy basal
#   geom_hline(yintercept = 0.156, linetype = "dashed") +
#   annotate(geom = "text", y = 0.25, x = 12, label = "Accuracy basal") +
#   coord_flip() +
#   labs(title = "Accuracy promedio de validación y test", 
#        x = "modelo") +
#   theme_bw() +
#   theme(legend.position = "bottom")




#############
#Sacamos las matrices de confusión
############
confusionMatrix(data = predicciones$svmrad_target1, 
                reference = as.factor(datos_test$target1))

confusionMatrix(data = predicciones$randfor_target1, 
                reference = as.factor(datos_test$target1))

confusionMatrix(data = predicciones$nnet_target1, 
                reference = as.factor(datos_test$target1))


#############
#Ensamblar los tres modelos
############
moda <- function(x, indice_mejor_modelo){
  tabla_freq <- table(x)
  freq_maxima <- max(tabla_freq)
  if (sum(tabla_freq == freq_maxima) > 1) {
    # En caso de empate, se devuelve la predicción que
    # ocupa el índice del mejor modelo
    return(x[indice_mejor_modelo])
  }
  return(names(which.max(table(x))))
}

predicciones_ensemble <- predicciones %>% 
  select(svmrad_target1, randfor_target1, nnet_target1) %>%
  mutate(moda = apply(X = select(.data = predicciones,
                                 svmrad_target1,
                                 randfor_target1, 
                                 nnet_target1),
                      MARGIN = 1,
                      FUN = moda,
                      indice_mejor_modelo = 1))

mean(predicciones_ensemble$moda == datos_test$target1)

write.csv(predicciones_ensemble$moda, file="N:/UDMTD/UDMTD12/Enrique-EMOS/TFM/pred_ensam.csv")
write.csv(datos_test$target1, file="N:/UDMTD/UDMTD12/Enrique-EMOS/TFM/datos_test.csv")

#######################################################################
#################         RANDOM FOREST PROB         ##################
#######################################################################
predict_randfor_target1  <- predict(object = randfor_target1, 
                                    newdata = datos_test, type="prob")

predict_randfor_target1 <- predict_randfor_target1[, 2]

###########           Análisis de resultados              ###########

results_t1_randfor <- data.table(PobErr_t1_randfor = predict_randfor_target1, 
                                 target1 = datos_test$target1
)


results_t1_randfor[
  , w := datos_test$FACTORADULTO][
    , m1 := PobErr_t1_randfor * w][
      , PobErr_t1_randfor_M := ecdf(m1)(m1)][
        , target1 := as.numeric(target1) - 1][
          , IDENTHOGAR := datos_test$IDENTHOGAR][
            , CNO1_AS_raw := datos_test$CNO1_AS_raw][
              , CNO1_AS_ed := datos_test$CNO1_AS_ed][
                , CNO2_AS_raw := datos_test$CNO2_AS_raw][
                  , CNO2_AS_ed := datos_test$CNO2_AS_ed]



results_t1_randfor_Sort <- results_t1_randfor[order(-PobErr_t1_randfor)][, priority := .I]
results_t1_randfor_Sort_m1 <- results_t1_randfor[order(-PobErr_t1_randfor_M)][, priority := .I]


###########           Curva ROC         ###########
roc1 <- roc(response = results_t1_randfor$target1, 
            predictor = results_t1_randfor$PobErr_t1_randfor)
auc1 <- auc(roc1)
thrsh1 <- coords(roc1, "best", ret = "threshold")
spec1 <- coords(roc1, "best", ret = "spec")
sens1 <- coords(roc1, "best", ret = "sens")
par(mar=c(5.1, 4.1, 10, 2.1))
plot(roc1, col = 'blue', lwd = 3,
     xlim = c(1,0), ylim = c(0,1),
     print.auc = FALSE, auc.polygon = FALSE,
     main = 'RANDOM FOREST')

rocm1 <- roc(response = results_t1_randfor$target1, 
             predictor = results_t1_randfor$PobErr_t1_randfor_M, 
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




#######################################################################
#################        NEURAL NETWORK PROB         ##################
#######################################################################
predict_nnet_target1  <- predict(object = nnet_target1, 
                                 newdata = datos_test, type = "prob")

predict_nnet_target1 <- predict_nnet_target1[, 2]

###########           Análisis de resultados              ###########

results_t1_nnet <- data.table(PobErr_t1_nnet = predict_nnet_target1, 
                              target1 = datos_test$target1
)


results_t1_nnet[
  , w := datos_test$FACTORADULTO][
    , m1 := PobErr_t1_nnet * w][
      , PobErr_t1_nnet_M := ecdf(m1)(m1)][
        , target1 := as.numeric(target1) - 1][
          , IDENTHOGAR := datos_test$IDENTHOGAR][
            , CNO1_AS_raw := datos_test$CNO1_AS_raw][
              , CNO1_AS_ed := datos_test$CNO1_AS_ed][
                , CNO2_AS_raw := datos_test$CNO2_AS_raw][
                  , CNO2_AS_ed := datos_test$CNO2_AS_ed]



results_t1_nnet_Sort <- results_t1_nnet[order(-PobErr_t1_nnet)][, priority := .I]
results_t1_nnet_Sort_m1 <- results_t1_nnet[order(-PobErr_t1_nnet_M)][, priority := .I]


###########           Curva ROC         ###########
library(pROC)
roc1 <- roc(response = results_t1_nnet$target1, 
            predictor = results_t1_nnet$PobErr_t1_nnet)
auc1 <- auc(roc1)
thrsh1 <- coords(roc1, "best", ret = "threshold")
spec1 <- coords(roc1, "best", ret = "spec")
sens1 <- coords(roc1, "best", ret = "sens")
par(mar = c(5.1, 4.1, 10, 2.1))
plot(roc1, col = 'blue', lwd = 3,
     xlim = c(1,0), ylim = c(0,1),
     print.auc = FALSE, auc.polygon = FALSE,
     main = 'NEURAL NETWORK')

rocm1 <- roc(response = results_t1_nnet$target1, 
             predictor = results_t1_nnet$PobErr_t1_nnet_M, 
             col = 'red', xlim = c(0,1), ylim = c(0,1))
aucm1 <- auc(rocm1)
thrshm1 <- coords(rocm1, "best", ret = "threshold")
specm1 <- coords(rocm1, "best", ret = "spec")
sensm1 <- coords(rocm1, "best", ret = "sens")
plot(rocm1, add = TRUE, col = 'red', lty = 3, lwd = 3,
     xlim = c(1,0), ylim = c(0,1),
     print.auc = FALSE, auc.polygon = FALSE)
legend(0.5, 0.3, legend = c("Without Sampling Weight", "With Sampling Weight"),
       col = c("blue", "red"), lty = c(1, 3), cex = 0.9, bty = "n")
text(0.8, 0.95, paste0('AUC without:', round(auc1, 2)))
text(0.8, 0.9, paste0('AUC with:', round(aucm1, 2)))
#  minor.tick(nx=10, ny=10)
points(x = spec1, y = sens1, col = 'blue', pch = 19, cex = 1.5)
points(x = specm1, y = sensm1, col = 'red', pch = 19, cex = 1.5)







#######################################################################
#################      SUPPORT VECTOR MACHINE        ##################
#######################################################################
predict_svmrad_target1  <- predict(object = svmrad_target1, 
                                   newdata = datos_test, type = "prob")

predict_svmrad_target1 <- predict_svmrad_target1[, 2]

###########           Análisis de resultados              ###########

results_t1_svm <- data.table(PobErr_t1_svm = predict_svmrad_target1, 
                             target1 = datos_test$target1
)


results_t1_svm[
  , w := datos_test$FACTORADULTO][
    , m1 := PobErr_t1_svm * w][
      , PobErr_t1_svm_M := ecdf(m1)(m1)][
        , target1 := as.numeric(target1) - 1][
          , IDENTHOGAR := datos_test$IDENTHOGAR][
            , CNO1_AS_raw := datos_test$CNO1_AS_raw][
              , CNO1_AS_ed := datos_test$CNO1_AS_ed][
                , CNO2_AS_raw := datos_test$CNO2_AS_raw][
                  , CNO2_AS_ed := datos_test$CNO2_AS_ed]


results_t1_svm_Sort <- results_t1_svm[order(-PobErr_t1_svm)][, priority := .I]
results_t1_svm_Sort_m1 <- results_t1_svm[order(-PobErr_t1_svm_M)][, priority := .I]

###########           Curva ROC         ###########
library(pROC)
roc1 <- roc(response = results_t1_svm$target1, 
            predictor = results_t1_svm$PobErr_t1_svm)
auc1 <- auc(roc1)
thrsh1 <- coords(roc1, "best", ret = "threshold")
spec1 <- coords(roc1, "best", ret = "spec")
sens1 <- coords(roc1, "best", ret = "sens")
par(mar = c(5.1, 4.1, 10, 2.1))
plot(roc1, col = 'blue', lwd = 3,
     xlim = c(1,0), ylim = c(0,1),
     print.auc = FALSE, auc.polygon = FALSE,
     main = 'SVM')

rocm1 <- roc(response = results_t1_svm$target1, 
             predictor = results_t1_svm$PobErr_t1_svm_M, 
             col = 'red', xlim = c(0,1), ylim = c(0,1))
aucm1 <- auc(rocm1)
thrshm1 <- coords(rocm1, "best", ret = "threshold")
specm1 <- coords(rocm1, "best", ret = "spec")
sensm1 <- coords(rocm1, "best", ret = "sens")
plot(rocm1, add = TRUE, col = 'red', lty = 3, lwd = 3,
     xlim = c(1,0), ylim = c(0,1),
     print.auc = FALSE, auc.polygon = FALSE)
legend(0.5, 0.3, legend = c("Without Sampling Weight", "With Sampling Weight"),
       col = c("blue", "red"), lty = c(1, 3), cex = 0.9, bty = "n")
text(0.8, 0.95, paste0('AUC without:', round(auc1, 2)))
text(0.8, 0.9, paste0('AUC with:', round(aucm1, 2)))
#  minor.tick(nx=10, ny=10)
points(x = spec1, y = sens1, col = 'blue', pch = 19, cex = 1.5)
points(x = specm1, y = sensm1, col = 'red', pch = 19, cex = 1.5)


#######################################################################
#################        ENSAMBLAJE MODELOS          ##################
#######################################################################
predict_ensamblaje_target1 <- (predict_svmrad_target1+predict_nnet_target1+predict_randfor_target1)/3


results_t1_ensam <- data.table(PobErr_t1_ensam = predict_ensamblaje_target1, 
                             target1 = datos_test$target1
)


results_t1_ensam[
  , w := datos_test$FACTORADULTO][
    , m1 := PobErr_t1_ensam * w][
      , PobErr_t1_ensam_M := ecdf(m1)(m1)][
        , target1 := as.numeric(target1) - 1][
          , IDENTHOGAR := datos_test$IDENTHOGAR][
            , CNO1_AS_raw := datos_test$CNO1_AS_raw][
              , CNO1_AS_ed := datos_test$CNO1_AS_ed][
                , CNO2_AS_raw := datos_test$CNO2_AS_raw][
                  , CNO2_AS_ed := datos_test$CNO2_AS_ed]


results_t1_svm_Sort <- results_t1_ensam[order(-PobErr_t1_ensam)][, priority := .I]
results_t1_svm_Sort_m1 <- results_t1_ensam[order(-PobErr_t1_ensam_M)][, priority := .I]

#############################################################################
###########           Curvas ROC juntas         #############################
#############################################################################
par(mfrow = c(2, 2))
###########        ENSAMBLAJE         #######
roc1 <- roc(response = results_t1_ensam$target1, 
            predictor = results_t1_ensam$PobErr_t1_ensam)
auc1 <- auc(roc1)
thrsh1 <- coords(roc1, "best", ret = "threshold")
spec1 <- coords(roc1, "best", ret = "spec")
sens1 <- coords(roc1, "best", ret = "sens")
par(mar = c(5.1, 4.1, 10, 2.1))
plot(roc1, col = 'blue', lwd = 3,
     xlim = c(1,0), ylim = c(0,1),
     print.auc = FALSE, auc.polygon = FALSE,
     main = 'ENSAMBLAJE DE MODELOS')

rocm1 <- roc(response = results_t1_ensam$target1, 
             predictor = results_t1_ensam$PobErr_t1_ensam_M, 
             col = 'red', xlim = c(0,1), ylim = c(0,1))
aucm1 <- auc(rocm1)
thrshm1 <- coords(rocm1, "best", ret = "threshold")
specm1 <- coords(rocm1, "best", ret = "spec")
sensm1 <- coords(rocm1, "best", ret = "sens")
plot(rocm1, add = TRUE, col = 'red', lty = 3, lwd = 3,
     xlim = c(1,0), ylim = c(0,1),
     print.auc = FALSE, auc.polygon = FALSE)
legend(0.5, 0.3, legend = c("Without Sampling Weight", "With Sampling Weight"),
       col = c("blue", "red"), lty = c(1, 3), cex = 0.9, bty = "n")
text(0.8, 0.95, paste0('AUC without:', round(auc1, 2)))
text(0.8, 0.9, paste0('AUC with:', round(aucm1, 2)))
#  minor.tick(nx=10, ny=10)
points(x = spec1, y = sens1, col = 'blue', pch = 19, cex = 1.5)
points(x = specm1, y = sensm1, col = 'red', pch = 19, cex = 1.5)

###########       SVM         #######
roc1 <- roc(response = results_t1_svm$target1, 
            predictor = results_t1_svm$PobErr_t1_svm)
auc1 <- auc(roc1)
thrsh1 <- coords(roc1, "best", ret = "threshold")
spec1 <- coords(roc1, "best", ret = "spec")
sens1 <- coords(roc1, "best", ret = "sens")
par(mar = c(5.1, 4.1, 10, 2.1))
plot(roc1, col = 'blue', lwd = 3,
     xlim = c(1,0), ylim = c(0,1),
     print.auc = FALSE, auc.polygon = FALSE,
     main = 'SVM')

rocm1 <- roc(response = results_t1_svm$target1, 
             predictor = results_t1_svm$PobErr_t1_svm_M, 
             col = 'red', xlim = c(0,1), ylim = c(0,1))
aucm1 <- auc(rocm1)
thrshm1 <- coords(rocm1, "best", ret = "threshold")
specm1 <- coords(rocm1, "best", ret = "spec")
sensm1 <- coords(rocm1, "best", ret = "sens")
plot(rocm1, add = TRUE, col = 'red', lty = 3, lwd = 3,
     xlim = c(1,0), ylim = c(0,1),
     print.auc = FALSE, auc.polygon = FALSE)
legend(0.5, 0.3, legend = c("Without Sampling Weight", "With Sampling Weight"),
       col = c("blue", "red"), lty = c(1, 3), cex = 0.9, bty = "n")
text(0.8, 0.95, paste0('AUC without:', round(auc1, 2)))
text(0.8, 0.9, paste0('AUC with:', round(aucm1, 2)))
#  minor.tick(nx=10, ny=10)
points(x = spec1, y = sens1, col = 'blue', pch = 19, cex = 1.5)
points(x = specm1, y = sensm1, col = 'red', pch = 19, cex = 1.5)


###########          NNET         ###########
roc1 <- roc(response = results_t1_nnet$target1, 
            predictor = results_t1_nnet$PobErr_t1_nnet)
auc1 <- auc(roc1)
thrsh1 <- coords(roc1, "best", ret = "threshold")
spec1 <- coords(roc1, "best", ret = "spec")
sens1 <- coords(roc1, "best", ret = "sens")
par(mar = c(5.1, 4.1, 10, 2.1))
plot(roc1, col = 'blue', lwd = 3,
     xlim = c(1,0), ylim = c(0,1),
     print.auc = FALSE, auc.polygon = FALSE,
     main = 'NEURAL NETWORK')

rocm1 <- roc(response = results_t1_nnet$target1, 
             predictor = results_t1_nnet$PobErr_t1_nnet_M, 
             col = 'red', xlim = c(0,1), ylim = c(0,1))
aucm1 <- auc(rocm1)
thrshm1 <- coords(rocm1, "best", ret = "threshold")
specm1 <- coords(rocm1, "best", ret = "spec")
sensm1 <- coords(rocm1, "best", ret = "sens")
plot(rocm1, add = TRUE, col = 'red', lty = 3, lwd = 3,
     xlim = c(1,0), ylim = c(0,1),
     print.auc = FALSE, auc.polygon = FALSE)
legend(0.5, 0.3, legend = c("Without Sampling Weight", "With Sampling Weight"),
       col = c("blue", "red"), lty = c(1, 3), cex = 0.9, bty = "n")
text(0.8, 0.95, paste0('AUC without:', round(auc1, 2)))
text(0.8, 0.9, paste0('AUC with:', round(aucm1, 2)))
#  minor.tick(nx=10, ny=10)
points(x = spec1, y = sens1, col = 'blue', pch = 19, cex = 1.5)
points(x = specm1, y = sensm1, col = 'red', pch = 19, cex = 1.5)


###########      RANDOM FOREST        ###########
roc1 <- roc(response = results_t1_randfor$target1, 
            predictor = results_t1_randfor$PobErr_t1_randfor)
auc1 <- auc(roc1)
thrsh1 <- coords(roc1, "best", ret = "threshold")
spec1 <- coords(roc1, "best", ret = "spec")
sens1 <- coords(roc1, "best", ret = "sens")
par(mar=c(5.1, 4.1, 10, 2.1))
plot(roc1, col = 'blue', lwd = 3,
     xlim = c(1,0), ylim = c(0,1),
     print.auc = FALSE, auc.polygon = FALSE,
     main = 'RANDOM FOREST')

rocm1 <- roc(response = results_t1_randfor$target1, 
             predictor = results_t1_randfor$PobErr_t1_randfor_M, 
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

