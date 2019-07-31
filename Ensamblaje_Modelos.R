
#####################################################################
###########             Lectura de modelos             ###########
#####################################################################
svmrad_target1_Smote <- readRDS(file = "N:/UDMTD12/Enrique-EMOS/TFM/Modelos CARET/svmrad_target1_smote_prob.rds")
randfor_target1_Smote <- readRDS(file = "N:/UDMTD12/Enrique-EMOS/TFM/Modelos CARET/randfor_target1_smote_prob.rds")
nnet_target1_Smote <- readRDS(file = "N:/UDMTD12/Enrique-EMOS/TFM/Modelos CARET/nnet_target1_smote_prob.rds")
svmrad_target1 <- readRDS(file = "N:/UDMTD12/Enrique-EMOS/TFM/Modelos CARET/svmrad_target1_prob.rds")
randfor_target1 <- readRDS(file = "N:/UDMTD12/Enrique-EMOS/TFM/Modelos CARET/randfor_target1_prob.rds")
nnet_target1 <- readRDS(file = "N:/UDMTD12/Enrique-EMOS/TFM/Modelos CARET/nnet_target1_prob.rds")




#####################################################################
###########             Predicción de modelos             ###########
#####################################################################
predict_svmrad_target1_Smote <- predict(object = svmrad_target1_Smote, newdata = datos_test)
predict_randfor_target1_Smote  <- predict(object = randfor_target1_Smote, newdata = datos_test)
predict_nnet_target1_Smote  <- predict(object = nnet_target1_Smote, newdata = datos_test)
predict_svmrad_target1 <- predict(object = svmrad_target1, newdata = datos_test)
predict_randfor_target1  <- predict(object = randfor_target1, newdata = datos_test)
predict_nnet_target1  <- predict(object = nnet_target1, newdata = datos_test)


predicciones <- data.frame(
  svmrad_target1_Smote = predict_svmrad_target1_Smote,
  randfor_target1_Smote  = predict_randfor_target1_Smote,
  nnet_target1_Smote  = predict_nnet_target1_Smote,
  svmrad_target1 = predict_svmrad_target1,
  randfor_target1  = predict_randfor_target1,
  nnet_target1  = predict_nnet_target1,
  valor_real        = datos_test$target1
)

predicciones %>% head()

#####################################################################
###########           Predicción de modelos Prob          ###########
#####################################################################
predict_svmrad_target1_Prob_Smote <- predict(object = svmrad_target1_Smote, newdata = datos_test, type="prob")
predict_randfor_target1_Prob_Smote  <- predict(object = randfor_target1_Smote, newdata = datos_test, type="prob")
predict_nnet_target1_Prob_Smote  <- predict(object = nnet_target1_Smote, newdata = datos_test, type="prob")
predict_svmrad_target1_Prob <- predict(object = svmrad_target1, newdata = datos_test, type="prob")
predict_randfor_target1_Prob  <- predict(object = randfor_target1, newdata = datos_test, type="prob")
predict_nnet_target1_Prob  <- predict(object = nnet_target1, newdata = datos_test, type="prob")




####################################################################################
##############              ENSAMBLAJE DE MODELOS         ##########################
####################################################################################

##### CORRELACIÓN ENTRE LOS MODELOS
results_Correla <- resamples(list(SVM = svmrad_target1, RF = randfor_target1, 
                                  NNET = nnet_target1, SVM_SMOTE = svmrad_target1_Smote, 
                                  RF_SMOTE = randfor_target1_Smote, NNET_SMOTE = nnet_target1_Smote)) 
modelCor(results_Correla) 





##############              USANDO LA MODA        ##########################
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

#### PREDICCIONES
predicciones_ensemble <- predicciones %>% 
  select(svmrad_target1, randfor_target1, nnet_target1) %>%
  mutate(moda = apply(X = select(.data = predicciones,
                                 svmrad_target1,
                                 randfor_target1, 
                                 nnet_target1),
                      MARGIN = 1,
                      FUN = moda,
                      indice_mejor_modelo = 1))

predicciones_ensemble_Smote <- predicciones %>% 
  select(svmrad_target1_Smote, randfor_target1_Smote, nnet_target1_Smote) %>%
  mutate(moda = apply(X = select(.data = predicciones,
                                 svmrad_target1_Smote,
                                 randfor_target1_Smote, 
                                 nnet_target1_Smote),
                      MARGIN = 1,
                      FUN = moda,
                      indice_mejor_modelo = 1))

head(predicciones_ensemble_Smote)

#### PRECISIÓN TOTAL DE LOS ENSAMBLAJES
mean(predicciones_ensemble$moda == datos_test$target1)
mean(predicciones_ensemble_Smote$moda == datos_test$target1)


#### PREDICCIONES PROBABILIDAD
predict_svmrad_target1_Prob <- predict_svmrad_target1_Prob[, 2]
predict_nnet_target1_Prob <- predict_nnet_target1_Prob[, 2]
predict_randfor_target1_Prob <- predict_randfor_target1_Prob[, 2]
predict_svmrad_target1_Prob_Smote <- predict_svmrad_target1_Prob_Smote[, 2]
predict_randfor_target1_Prob_Smote <- predict_randfor_target1_Prob_Smote[, 2]
predict_nnet_target1_Prob_Smote <- predict_nnet_target1_Prob_Smote[, 2]

predict_ensamblaje_target1 <- (predict_svmrad_target1_Prob+predict_nnet_target1_Prob+
                                 predict_randfor_target1_Prob)/3

predict_ensamblaje_target1_Smote <- (predict_svmrad_target1_Prob_Smote+
                                     predict_nnet_target1_Prob_Smote+
                                     predict_randfor_target1_Prob_Smote)/3

results_t1_ensam <- data.table(PobErr_t1_ensam = predict_ensamblaje_target1, 
                               target1 = datos_test$target1
)

results_t1_ensam_Smote <- data.table(PobErr_t1_ensam_Smote = predict_ensamblaje_target1_Smote, 
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


results_t1_ensam_Smote[
  , w := datos_test$FACTORADULTO][
    , m1 := PobErr_t1_ensam_Smote * w][
      , PobErr_t1_ensam_Smote_M := ecdf(m1)(m1)][
        , target1 := as.numeric(target1) - 1][
          , IDENTHOGAR := datos_test$IDENTHOGAR][
            , CNO1_AS_raw := datos_test$CNO1_AS_raw][
              , CNO1_AS_ed := datos_test$CNO1_AS_ed][
                , CNO2_AS_raw := datos_test$CNO2_AS_raw][
                  , CNO2_AS_ed := datos_test$CNO2_AS_ed]


par(mfrow = c(1, 2))
#### CURVA ROC NORMAL
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
legend(0.8, 0.7, legend = c("Without Sampling Weight", "With Sampling Weight"),
       col = c("blue", "red"), lty = c(1, 3), cex = 0.9, bty = "n")
text(0.8, 0.95, paste0('AUC without:', round(auc1, 2)))
text(0.8, 0.9, paste0('AUC with:', round(aucm1, 2)))
#  minor.tick(nx=10, ny=10)
points(x = spec1, y = sens1, col = 'blue', pch = 19, cex = 1.5)
points(x = specm1, y = sensm1, col = 'red', pch = 19, cex = 1.5)


#### CURVA ROC SMOTE
roc1 <- roc(response = results_t1_ensam_Smote$target1, 
            predictor = results_t1_ensam_Smote$PobErr_t1_ensam_Smote)
auc1 <- auc(roc1)
thrsh1 <- coords(roc1, "best", ret = "threshold")
spec1 <- coords(roc1, "best", ret = "spec")
sens1 <- coords(roc1, "best", ret = "sens")
par(mar = c(5.1, 4.1, 10, 2.1))
plot(roc1, col = 'blue', lwd = 3,
     xlim = c(1,0), ylim = c(0,1),
     print.auc = FALSE, auc.polygon = FALSE,
     main = 'ENSAMBLAJE DE MODELOS - SMOTE')

rocm1 <- roc(response = results_t1_ensam_Smote$target1, 
             predictor = results_t1_ensam_Smote$PobErr_t1_ensam_Smote_M, 
             col = 'red', xlim = c(0,1), ylim = c(0,1))
aucm1 <- auc(rocm1)
thrshm1 <- coords(rocm1, "best", ret = "threshold")
specm1 <- coords(rocm1, "best", ret = "spec")
sensm1 <- coords(rocm1, "best", ret = "sens")
plot(rocm1, add = TRUE, col = 'red', lty = 3, lwd = 3,
     xlim = c(1,0), ylim = c(0,1),
     print.auc = FALSE, auc.polygon = FALSE)
legend(0.8, 0.7, legend = c("Without Sampling Weight", "With Sampling Weight"),
       col = c("blue", "red"), lty = c(1, 3), cex = 0.9, bty = "n")
text(0.8, 0.95, paste0('AUC without:', round(auc1, 2)))
text(0.8, 0.9, paste0('AUC with:', round(aucm1, 2)))
#  minor.tick(nx=10, ny=10)
points(x = spec1, y = sens1, col = 'blue', pch = 19, cex = 1.5)
points(x = specm1, y = sensm1, col = 'red', pch = 19, cex = 1.5)




