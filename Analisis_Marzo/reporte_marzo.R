require(boot)
require(MASS)
require(tidyverse)
require(magrittr)
require(nnet)
require(caret)
require(cluster)
require(glmnet)
require(clValid)
require(pROC)
require(randomForest)
require(rsample)
require(fpc)
require(vcd)

#load("BDs_var.RData")


# Clustering "Gower" ------------------------------------------------------

sum.tot <- merge(sum.ins, sum.admi) %>% merge(sum.grad)

sum.tot %<>% filter(area_de_conocimiento != "sin clasificar")

sum.tot$sector_ies %<>% factor()

sum.tot$metodologia %<>% factor()

sum.tot$area_de_conocimiento %<>% factor()

sum.tot$semestre %<>% factor()

sum.tot$sexo %<>% factor()

names(sum.tot)[2] <- "comparacion"

sum.tot$comparacion %<>% factor(., levels = c("UNAL", "otras IES"))

sum.tot %>% count(ano)


sum.tot <- sum.tot %>% mutate(nyear = 
                                case_when(ano == 2007 & semestre == 1 ~ 1,
                                     ano == 2007 & semestre == 2 ~ 2,
                                     ano == 2008 & semestre == 1 ~ 3,
                                     ano == 2008 & semestre == 2 ~ 4,
                                     ano == 2009 & semestre == 1 ~ 5,
                                     ano == 2009 & semestre == 2 ~ 6,
                                     ano == 2010 & semestre == 1 ~ 7,
                                     ano == 2010 & semestre == 2 ~ 8,
                                     ano == 2011 & semestre == 1 ~ 9,
                                     ano == 2011 & semestre == 2 ~ 10,
                                     ano == 2012 & semestre == 1 ~ 11,
                                     ano == 2012 & semestre == 2 ~ 12,
                                     ano == 2013 & semestre == 1 ~ 13,
                                     ano == 2013 & semestre == 2 ~ 14,
                                     ano == 2014 & semestre == 1 ~ 15,
                                     ano == 2014 & semestre == 2 ~ 16,
                                     ano == 2015 & semestre == 1 ~ 17,
                                     ano == 2015 & semestre == 2 ~ 18,
                                     ano == 2016 & semestre == 1 ~ 19,
                                     ano == 2016 & semestre == 2 ~ 20,
                                     ano == 2017 & semestre == 1 ~ 21,
                                     ano == 2017 & semestre == 2 ~ 22,
                                     ano == 2018 & semestre == 1 ~ 23,
                                     ano == 2018 & semestre == 2 ~ 24,
                                     ano == 2019 & semestre == 1 ~ 25,
                                     ano == 2019 & semestre == 2 ~ 26,
                                     ano == 2020 & semestre == 1 ~ 27,
                                     ano == 2020 & semestre == 2 ~ 28,
                                     ano == 2021 & semestre == 1 ~ 29,
                                     ano == 2021 & semestre == 2 ~ 30))


sum.tot$nyear_f <- sum.tot$nyear %>% factor(., ordered = T)

sum.tot %>% group_by(nyear_f) %>% summarise(n = n(), .groups = 'drop') %>%
  as.data.frame()

# calculate distance
# Clustering tomando solo unas varaibles
# [1] "sector_ies"           "comparacion"         
# [3] "metodologia"          "area_de_conocimiento"
# [5] "sexo"                 "demanda_real"        
# [7] "admitidos"            "demanda_potencial"   
# [9] "nyear

d_dist <- sum.tot %>% dplyr::select(.,c(1:4,7:11)) %>%  daisy(., metric = "gower",
                type = list(logratio = 2))

clust <- agnes(d_dist, method = "ward")

sum.tot$cluster <- cutree(clust, k = 4)

# Aplicar el clustering PAM
pam_results <- pam(d_dist, k = 4, diss = TRUE, cluster.only = TRUE)

sum.tot$pam_results <- pam_results

# Evaluación del Cluster --------------------------------------------------

#### Índice de Silhouette: ####

# Un valor cercano a 1 indica una buena separación de los clusters,
# mientras que un valor cercano a -1
# indica que las observaciones
# se agrupan en el cluster equivocado

sil <- silhouette(sum.tot$cluster, dist(d_dist))
summary(sil)$avg.width

# Coeficiente de Silhouette para el método PAM
sil2 <- silhouette(sum.tot$pam_results, d_dist)
summary(sil2)$avg.width

# Coeficiente de correlación de Cramer para el clustering aglomerativo
assocstats(table(sum.tot$cluster, sum.tot$area_de_conocimiento))

# Coeficiente de correlación de Cramer para el método PAM
assocstats(table(pam_results, sum.tot$area_de_conocimiento))

# Dividir los datos en entrenamiento y prueba -----------------------------
sum.tot0 <- sum.tot
sum.tot <- sum.tot %>% dplyr::select(c(1:4,7:11))

set.seed(123)

id_train <- sample(1:nrow(sum.tot),0.7*nrow(sum.tot))

data.train <- sum.tot[id_train,] %>% as.data.frame()

data.test <- sum.tot[-id_train,] %>% as.data.frame()


# Ajuste del modelo multinomial con regularización L1 -------------------

x <- model.matrix(area_de_conocimiento~., data = data.train)[,-1]

y <- data.train$area_de_conocimiento

fit <- cv.glmnet(x,y, family = "multinomial", alpha = 1)

x.test <- model.matrix(area_de_conocimiento ~.,
                       data = data.test)[,-1]

predicciones <- predict(fit, s = "lambda.min", newx = x.test,
                       type = "class")

# Obtener la matriz de confusión
m1 <- predicciones %>% factor()

m2 <- data.test$area_de_conocimiento %>% as.matrix() %>% factor()

matriz_confusion <- confusionMatrix(m1,m2)

mc <- matriz_confusion$table
mc

matriz_confusion$overall
# Accuracy = 0.325723

matriz_confusion$byClass[1:8] %>% mean()
matriz_confusion$byClass[9:16] %>% mean()


### Curvas ROC ###

y_pred <- predict(fit, newx = x.test, type = "response")

data.test %>% count(area_de_conocimiento)

# Convertir etiquetas categóricas en valores binarios
y_test <- ifelse(data.test$area_de_conocimiento ==
                   "ciencias educacion", 1, 0)

# Calcular curva ROC y el área bajo la curva (AUC)
roc_obj <- roc(y_test, y_pred[1:657])
auc_val <- auc(roc_obj)
plot(roc_obj, main = paste0("Curva ROC\nAUC = ", round(auc_val, 3)))

# Selección de Variables predictoras relevantes ---------------------------

model <- multinom(area_de_conocimiento ~ ., data = data.train)
model_stepAIC <- stepAIC(model, direction = "both", trace = FALSE)
model_stepAIC

x.test <- model.matrix(area_de_conocimiento ~.,
                       data = data.test)[,-1]

predicciones <- predict(model_stepAIC,
                        newdata = data.test[,-4], type = "class")

m1 <- predicciones %>% factor()

matriz_confusion <- confusionMatrix(m1,m2)

mc <- matriz_confusion$table
mc

matriz_confusion$overall
# Accuracy = 0.3074581

matriz_confusion$byClass[1:8] %>% mean()
matriz_confusion$byClass[9:16] %>% mean()

### Curvas ROC ###
y_pred <- predict(model_stepAIC,
                  newdata = data.test[,-4], type = "prob")

# Convertir etiquetas categóricas en valores binarios
y_test <- ifelse(data.test$area_de_conocimiento ==
                   "agronomia veterinaria afines", 1, 0)

# Calcular curva ROC y el área bajo la curva (AUC)
roc_obj <- roc(y_test, y_pred[,1])
auc_val <- auc(roc_obj)
plot(roc_obj, main = paste0("Curva ROC\nAUC = ", round(auc_val, 3)))


# ajustar los hiperparámetros del modelo ----------------------------------------------------
model <- multinom(area_de_conocimiento ~ ., data = data.train,
                  type = "class")

# Define la tabla de control de train
ctrl <- trainControl(method = "cv", number = 10, verboseIter = TRUE)

# Realiza la validación cruzada
cv.fit <- train(area_de_conocimiento ~ ., data = sum.tot, method = "multinom",
                trControl = ctrl, tuneLength = 10)

fit2 <- cv.fit$finalModel

fit2

### Predicciones ###
x.test <- model.matrix(area_de_conocimiento ~.,
                       data = data.test)[,-1]

predicciones <- predict(fit2,newdata = x.test, type = "class")

m1 <- predicciones %>% factor()

matriz_confusion <- confusionMatrix(m1,m2)

mc <- matriz_confusion$table
mc

matriz_confusion$overall
# Accuracy = 0.3652968

matriz_confusion$byClass[1:8] %>% mean()
matriz_confusion$byClass[9:16] %>% mean()


### Curvas ROC ###

y_pred <- predict(fit2,newdata = x.test, type = "prob")

# Convertir etiquetas categóricas en valores binarios
y_test <- ifelse(data.test$area_de_conocimiento ==
                   "agronomia veterinaria afines", 1, 0)

# Calcular curva ROC y el área bajo la curva (AUC)
roc_obj <- roc(y_test, y_pred[,1])
auc_val <- auc(roc_obj)
plot(roc_obj, main = paste0("Curva ROC\nAUC = ", round(auc_val, 3)))
 
# Capacidad Predictiva y Clasificadora de las Covariables ----------------

arbol.de <- randomForest(area_de_conocimiento~sector_ies+
                           demanda_real+admitidos+
                           demanda_potencial+nyear+sexo, data = sum.tot, importance = T)

varImpPlot(arbol.de, main = "Capacidad Predictiva y Clasificadora de las Covariables")


# Elastic net -------------------------------------------------------------
# método "elastic net" que combina la penalización de Lasso y Ridge.
# Este método es útil para seleccionar un
# subconjunto de características importantes
# y reducir la multicolinealidad en los datos.

# Dividir los datos en entrenamiento y prueba
set.seed(123)
train_index <- createDataPartition(sum.tot$area_de_conocimiento, p = 0.8, list = FALSE)
train_data <- sum.tot[train_index, ]
test_data <- sum.tot[-train_index, ]

# Definir el control de entrenamiento
ctrl <- trainControl(method = "cv", number = 10)

# Definir la búsqueda automática de hiperparámetros
tune_grid <- expand.grid(alpha = seq(0, 1, length = 11), lambda = seq(0, 1, length = 11))
glmnet_model <- train(area_de_conocimiento ~ ., data = train_data, method = "glmnet",
                      trControl = ctrl, tuneGrid = tune_grid)



# Imprimir el modelo
print(glmnet_model)

# Accuracy was used to select the optimal model using the largest value.
# The final values used for the model were alpha = 0.2 and lambda = 0.

m2 <- train_data$area_de_conocimiento %>% factor() %>% as.matrix()

predicciones <- predict(glmnet_model, test_data)
confusion <- confusionMatrix(predicciones, test_data$area_de_conocimiento)



precision <- confusion$overall['Accuracy']
sensibilidad <- confusion$byClass[1:8] %>% mean()
especificidad <- confusion$byClass[9:16] %>% mean()

# Imprimir las medidas de evaluación
print(precision)
print(sensibilidad)
print(especificidad)

### Predicciones ###
x.test <- model.matrix(area_de_conocimiento ~.,
                       data = test_data)[,-1]

### Curvas ROC ###

y_pred <- predict(glmnet_model,newdata = test_data, type = "prob")

# Convertir etiquetas categóricas en valores binarios
y_test <- ifelse(test_data$area_de_conocimiento ==
                   "agronomia veterinaria afines", 1, 0)

# Calcular curva ROC y el área bajo la curva (AUC)
roc_obj <- roc(y_test, y_pred[,1])
auc_val <- auc(roc_obj)
plot(roc_obj, main = paste0("Curva ROC\nAUC = ", round(auc_val, 3)))


