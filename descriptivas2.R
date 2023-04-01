library(tidyverse)

load("base_total.RData")

copy <- base_total[, -c(24, 25, 2, 7, 9, 11, 18, 2)]

## valores lógicos si tienen na 
aux <- complete.cases(copy)

## base sin na
copy_sin_na <- copy[aux, ]

names(copy_sin_na)

copy_sin_na$principal_o_seccional %>% unique()

copy_sin_na$caracter_ies  <- str_replace_all(copy_sin_na$caracter_ies,
                                             c("UNIVERSIDAD"  = "Universidad", 
                                               "INSTITUCION UNIVERSITARIA/ESCUELA TECNOLOGICA"="Institución Universitaria/Escuela Tecnológica",
                                               "INSTITUCIÓN UNIVERSITARIA/ESCUELA TECNOLÓGICA" = "Institución Universitaria/Escuela Tecnológica",
                                               "INSTITUCION TECNOLOGICA" = "Institución Tecnológica",
                                               "INSTITUCIÓN TECNOLÓGICA" = "Institución Tecnológica",
                                               "INSTITUCION TECNICA PROFESIONAL" = "Institución Técnica Profesional",
                                               "INSTITUCIÓN TÉCNICA PROFESIONAL" = "Institución Técnica Profesional"))


copy_sin_na$nivel_de_formacion %>% unique()

copy_sin_na$nivel_de_formacion <-  str_replace_all(copy_sin_na$nivel_de_formacion,
                                                   c("Universitaria" = "Universitario",
                                                   "Tecnológica" = "Tecnólogo",
                                                   "Tecnológico"  = "Tecnólogo"))

names(copy_sin_na)

copy_sin_na$metodologia %>% unique()

copy_sin_na$metodologia <-  str_replace_all(copy_sin_na$metodologia,
                                                   c("A distancia" = "Distancia",
                                                     "Distancia (virtual)" = "Distancia_virtual",
                                                     "Distancia (tradicional)"  = "Distancia"
                                                     ))



copy_sin_na$departamento_de_oferta_del_programa <- tolower(copy_sin_na$departamento_de_oferta_del_programa)

copy_sin_na$departamento_de_oferta_del_programa <- str_replace_all(copy_sin_na$departamento_de_oferta_del_programa, 
                                  c("á" = "a", "é" = "e", "í" = "i", "ó" = "o", "ú" = "u"))

copy_sin_na$departamento_de_oferta_del_programa %>% unique()



copy_sin_na$departamento_de_oferta_del_programa <-  str_replace_all(copy_sin_na$departamento_de_oferta_del_programa,
                                            c("bogota d.c" = "bogota",
                                              "bogota d.c." = "bogota",
                                              "bogota, d.c." = "bogota",
                                              "bogota." = "bogota"
                                              ))

names(copy_sin_na)

copy_sin_na$sexo %>% unique()


###  serie de tiempo


serie <-  base_total %>%  
  dplyr::select(ano, admitidos, graduados)
  

serie$ano <- as.factor(serie$ano)

serie <- serie %>% 
  mutate_at(vars(admitidos, graduados), as.numeric)

 

serie <- serie %>%
  group_by(ano) %>%
  summarize(admitidos_total_ano = sum(admitidos, na.rm = TRUE),
            graduados_total_ano = sum(graduados, na.rm = TRUE))


serie <- serie %>% slice(-1)

serie$ano <- as.character(serie$ano)

serie$ano <- as.numeric(serie$ano)






# Create a line plot of the data

ggplot(data = serie, aes(x = ano, y = admitidos_total_ano, color = "Admissions")) +
  geom_line() +
  geom_line(aes(y = graduados_total_ano, color = "Graduates")) +
  scale_color_manual(name = "Total", labels = c("Admissions", "Graduates"), values = c("red", "blue")) +
  xlab("Year") +
  ylab("Total Admissions") +
  scale_y_continuous(sec.axis = sec_axis(~., name = "Total Graduates"))


###### modelo de regresión 





mydata <- copy_sin_na %>% dplyr::select(principal_o_seccional, sector_ies, caracter_ies,
                            nivel_de_formacion,
                            metodologia,
                            area_de_conocimiento, nucleo_basico_del_conocimiento_nbc,
                            departamento_de_oferta_del_programa, sexo, ano,
                            semestre, inscritos, admitidos, graduados)


############## trasformacion de los datos a la variables necesarias 



##### númericas


mydata <- mydata %>%
  mutate_at(vars(graduados, inscritos, admitidos, ano), as.numeric) %>% 
  mutate_at(vars(sexo, semestre, metodologia, nivel_de_formacion, caracter_ies,
                 nucleo_basico_del_conocimiento_nbc, principal_o_seccional,
                 sector_ies,
                 area_de_conocimiento,
                 departamento_de_oferta_del_programa), as.factor)

##### variables tipo factor 



# Fit the linear regression model por metodo tradicional
#model <- lm(graduados ~ ., data = mydata)


#summary(mydata)




# modelo por metodo de aprendizaje de maquina 

# Load the caret package

library(caret)

# Split the data into training and validation sets

set.seed(123)

split <- createDataPartition(mydata$graduados, p = 0.7, list = FALSE)
train <- mydata[split, ]
validate <- mydata[-split, ]

# Fit the linear regression model
model <- train(graduados ~ ., data = train, method = "lm")

# Make predictions on the validation set
predictions <- predict(model, newdata = validate)
  

########### calculo de metricas de redimiento

# Calculate mean absolute error
mae <- mean(abs(predictions - validate$graduados))

# Calculate root mean squared error
rmse <- sqrt(mean((predictions - validate$graduados)^2))

# Calculate R-squared
r2 <- 1 - (sum((predictions - validate$graduados)^2) / sum((validate$graduados - mean(validate$graduados))^2))

# Print the performance metrics
print(paste("MAE:", mae))
print(paste("RMSE:", rmse))
print(paste("R-squared:", r2))

######################### evaluando el  sobreajuste  usando el MAE

train_predictions <- predict(model, newdata = train)
validate_predictions <- predict(model, newdata = validate)

# Calculate the MAE on the training and validation sets
train_mae <- mean(abs(train_predictions - train$graduados))
validate_mae <- mean(abs(validate_predictions - validate$graduados))

# Calculate the percentage difference between the MAE on the training and validation sets
difference <- 100 * (validate_mae - train_mae) / train_mae

# Print the percentage difference
print(paste("Percentage difference:", difference, "%"))



 


######################## clusterización 




# Load the klaR package
install.packages("klaR")
library(klaR)

# Set the number of clusters
k <- 5

# Build the k-modes clustering model
model <- kmodes(mydata, modes = k, iter.max = 20)

# Print the cluster assignments
print(paste("Cluster assignments:", model$cluster))



mydata_clustered <- cbind(mydata, cluster = model$cluster)

view(mydata_clustered)



### caraterización de los cluster

# Set the layout of the plot
par(mfrow = c(1, k))

# Create a bar chart of the frequency of each level of the 'sexo' variable for each cluster
by(mydata_clustered, mydata_clustered$cluster, function(x) {
  barplot(table(x$sex), beside = TRUE)
})

## podemos notar que en el cluster 1 y el cluster 4 es donde la mujeres son las que priman es dichas 
# categorias, mientras que para el cluster 2, 3 y 5 los hombres son la mayoria.


####3 para la variable sexo

ggplot(mydata_clustered, aes(x = sexo)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  facet_wrap(~ cluster)

#### resumen  númerico para las varibles continuas


library(psych)

by(mydata_clustered[, c("inscritos", "admitidos", "graduados")],
   mydata_clustered$cluster, describe)





####3 para la variable area del conocimiento 

ggplot(mydata_clustered, aes(x =area_de_conocimiento, fill = area_de_conocimiento)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  facet_wrap(~ cluster)+
  theme(axis.text.x = element_blank())




####3 para la variable area del metodologia

ggplot(mydata_clustered, aes(x =metodologia, fill = metodologia)) +
  geom_bar(aes(y = (..count..)/sum(..count..)))+theme_bw()+
  facet_wrap(~ cluster)+
  theme(legend.position = "none",axis.text.x = element_blank())



