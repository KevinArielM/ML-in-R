library(data.table)
library(dplyr)
library(vtable)
library(stringr)

##PREPROCESAMIENTO
data <- read.csv("C:/Users/kevin/Downloads/Austin_Animal_Center_Outcomes.csv")
# Revisar si hay cadenas vacías o espacios
sapply(data, function(x) sum(x == "", na.rm = TRUE))


#Eliminar registros duplicados
data <- data %>%
  distinct()   #Son solo 7
# Revisar si hay valores "Unknown" o similares
sapply(data, function(x) sum(x == "Unknown", na.rm = TRUE))

#Para posibles vacíos que no son NA directamente

data[data==""] <- NA 
summary(data)
data %>% naniar::vis_miss(., warn_large_data = F)
data$Sex.upon.Outcome %>% table() 
data$Sex.upon.Outcome %>% table() %>% prop.table()

#Imputación por muestreo aleatorios
clases_existentes <- data$Sex.upon.Outcome[!data$Sex.upon.Outcome %in% c("NULL", "Unknown")]
print(clases_existentes)
# Calcular las proporciones
proporciones <- table(clases_existentes) / length(clases_existentes)
print(proporciones)
# Imputar valores
data$Sex.upon.Outcome <- sapply(data$Sex.upon.Outcome, function(x) {
  if (x == "NULL" || x == "Unknown") {
    sample(names(proporciones), 1, prob = proporciones)
  } else {
    x
  }
})


table(data$Sex.upon.Outcome)

#Gráfica del sexo
library(ggplot2)

ggplot(data, aes(x = Sex.upon.Outcome)) +
  geom_bar() +
  labs(title = "Distribución del sexo", x="Sexo", y="Registros")+scale_fill_brewer(palette = "Set2")+theme_minimal()


#Gráfica del tipo de animal
library(ggplot2)

ggplot(data, aes(x = Animal.Type)) +
  geom_bar() +
  labs(title = "Distribución por tipo de mascota", x="Tipo", y="Registros")+scale_fill_brewer(palette = "Set2")+theme_minimal()


# Contar registros por raza y filtrar el top 25
top_25_raza <- data %>%
  count(Breed.2, sort = TRUE) %>%  # Contar y ordenar por frecuencia
  top_n(25, n)  # Filtrar las 20 razas más frecuentes

# Crear el gráfico de barras
ggplot(top_25_raza, aes(x = reorder(Breed.2, n), y = n)) +
  geom_bar(stat = "identity", fill = "gray") +
  labs(title = "Distribución del top 25 'razas'", x = "'Raza'", y = "Registros") +
  coord_flip() +  # Voltea el gráfico para una mejor visualización
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") + theme(axis.text.x = element_text(angle = 15, hjust = 1))

# Calcular la proporción de adoptados por raza
proporciones_adoptados <- data %>%
  filter(Breed.2 %in% top_25_raza$Breed.2) %>%  # Filtra por el top 25 de razas
  group_by(Breed.2) %>%
  summarise(total_adoptados = sum(Total.Outcome.Type == "Adoption"),  # Total adoptados
            total = n(),                                       # Total de registros
            proporcion_adoptados = total_adoptados / total)  # Proporción

# Mostrar el resultado
print(proporciones_adoptados, n=25)

# Ordenar el data frame por la columna de proporción en orden descendente y luego mostrar 25 filas
proporciones_adoptados_ordenadas <- proporciones_adoptados %>%
  arrange(desc(proporcion_adoptados))

print(proporciones_adoptados_ordenadas, n = 25)

# Crear el gráfico de barras
ggplot(proporciones_adoptados_ordenadas, aes(x = reorder(Breed.2, proporcion_adoptados), y = proporcion_adoptados)) +
  geom_bar(stat = "identity", fill = "gray") +
  labs(title = "Proporción de adopción en el top 25", x = "Raza", y = "Proporción de adoptados") +
  coord_flip() +  
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") + 
  theme(axis.text.y = element_text(angle = 0, hjust = 1))

data %>% naniar::vis_miss(., warn_large_data = F)

#Hay faltantes en Name y en Outcome.Subtype, variables no relevantes en el análisis

data$Name %>% table() %>% prop.table()
data$Name[is.na(data$Name) | data$Name == ""] <- "Nombre por registrar..."
data %>% naniar::vis_miss(., warn_large_data = F)

#Para Outcome.Subtype
data$Outcome.Subtype %>% table() %>% prop.table()
sum(is.na(data$Outcome.Subtype))
#La variable tiene muchos nulos, por lo que no es imposible imputar y se eliminará del modelo

#Ver la variable Breed
data$Breed %>% table()
sum(is.na(data$Breed.Subtype))

#Ver la variable Animal Type
data$Animal.Type %>% table()


data_original <- data

data <- data[, !names(data) %in% "Outcome.Subtype"]  # Elimina la columna

data %>% naniar::vis_miss(.,warn_large_data = F)

#En Outcome.Type se eliminan los registros pues es la variable objetivo
sum(is.na(data$Outcome.Type)) #Notar que solo son 7 registros

data <- data[!is.na(data$Outcome.Type), ]

data %>% naniar::vis_miss(.,warn_large_data = F)

#Por lo que el preprocesamiento es correcto, pero se agregarán las variables height and weight (opcion)


#Convertir a mes todas las fechas relevantes
convertir_mes <- function(x) {
  if (grepl("month", x, ignore.case = TRUE)) {
    return(as.numeric(gsub("[^0-9]", "", x)))
  } else if (grepl("year", x, ignore.case = TRUE)) {
    return(as.numeric(gsub("[^0-9]", "", x)) * 12)
  } else if (grepl("week", x, ignore.case = TRUE)) {
    return(as.numeric(gsub("[^0-9]", "", x)) / 4)  
  } else if (grepl("day", x, ignore.case = TRUE)) {
    return(as.numeric(gsub("[^0-9]", "", x)) / 30)  
  } else {
    return(NA)  
  }
}
data$Age.upon.Outcome.Months <- sapply(data$Age.upon.Outcome, convertir_mes)

data %>% naniar::vis_miss(.,warn_large_data = F)


data <- data[!is.na(data$Age.upon.Outcome.Months), ]

data %>% naniar::vis_miss(.,warn_large_data = F)

top_breeds <- data$Breed %>% 
  table() %>% 
  sort(decreasing = TRUE) %>% 
  head(50) %>% 
  names()
print(top_breeds)



# Definir valores promedio
height <- c("Dog" = 60, "Cat" = 25, "Bird" = 15, "Livestock" = 150, "Other" = 30) #cm
weight <- c("Dog" = 25, "Cat" = 5, "Bird" = 0.5, "Livestock" = 500, "Other" = 10) #kg

# Crear columnas de altura y peso basadas en los promedios
data$Height <- height[data$Animal.Type]
data$Weight <- weight[data$Animal.Type]


data$Date.of.Birth <- as.Date(data$Date.of.Birth, format = "%m/%d/%Y")
# Definir la fecha de referencia (01/01/2019)
corte_fecha <- as.Date("2019-01-01")

# Calcular la diferencia en meses
data$Age.in.Months <- as.numeric(difftime(corte_fecha, data$Date.of.Birth, units = "days")) / 30.44  # Aproximando un mes en días
data$Age.in.Months <- floor(data$Age.in.Months) 

data %>% naniar::vis_miss(.,warn_large_data = F)



#Preprocesamiento completo

#Modelos
library(e1071)  # Para Naive Bayes y SVM
library(caret)  # Para evaluación del modelo
library(pROC) # Curva ROC

# Dividiendo el conjunto de datos en entrenamiento y prueba (70% entrenamiento, 30% prueba)
set.seed(123)  # Para reproducibilidad
ind <- sample(1:nrow(data), 0.7 * nrow(data))  # 70% de datos de entrenamiento
train_data <- data[ind, ]
test_data <- data[-ind, ]

# Seleccionar solo las columnas necesarias 
train_data_g <- train_data[, c("Animal.Type", "Sex.upon.Outcome", "Age.upon.Outcome.Months", "Age.in.Months", "Breed.2", "Total.Outcome.Type")]
test_data_g <- test_data[, c("Animal.Type", "Sex.upon.Outcome", "Age.upon.Outcome.Months", "Age.in.Months", "Breed.2", "Total.Outcome.Type")]
#'Total.Outcome.Type' sea binaria
train_data_g$Total.Outcome.Type <- factor(train_data_g$Total.Outcome.Type, levels = c("Adoption", "No adoption"))


train_data$Animal.Type <- factor(train_data$Animal.Type)
train_data$Sex.upon.Outcome <- factor(train_data$Sex.upon.Outcome)
train_data$Breed.2 <- factor(train_data$Breed.2)

# El modelo
modelo_logit <- glm(Total.Outcome.Type ~ Animal.Type + Sex.upon.Outcome + Age.upon.Outcome.Months + Age.in.Months + Breed.2, 
                    data = train_data_g, 
                    family = binomial)
#Niveles
#unique(train_data$Animal.Type)
# #unique(train_data$Sex.upon.Outcome)
# unique(train_data$Breed.2)
# unique(train_data$Total.Outcome.Type)
# sum(is.na(train_data$Total.Outcome.Type))



#Niveles
 test_data_g$Breed.2 <- factor(test_data$Breed.2, levels = levels(train_data$Breed.2))
 sum(is.na(test_data$Breed.2))


# Predicciones en el conjunto de prueba
test_data_g$logit_pred <- predict(modelo_logit, newdata = test_data_g, type = "response")

# Predicciones en el conjunto de prueba
test_data_g$logit_pred <- predict(modelo_logit, newdata = test_data_g, type = "response")  # Probabilidades de adopción

# Si la predicción es mayor a 0.5, se predice "Adoption", de lo contrario "No adoption"
test_data_g$predicted_logit <- ifelse(test_data_g$logit_pred > 0.80, "Adoption", "No adoption")

# Obtener las probabilidades predichas y etiquetas verdaderas
logit_predicciones <- test_data_g$logit_pred  # Estas son las probabilidades predichas
logit_etiquetas <- factor(test_data_g$Total.Outcome.Type, levels = c("Adoption", "No adoption"))  # Etiquetas verdaderas como factor

# Crear la curva ROC y calcular el AUC
logit_roc <- roc(logit_etiquetas, logit_predicciones, levels = c("Adoption", "No adoption"), direction = "<")

# Calcular el AUC
logit_auc <- auc(logit_roc)
plot(logit_roc, col = "blue", main = "Curva ROC- Logística")



# Imprimir el AUC
print(logit_auc)
#Factores
test_data_g$predicted_logit <- factor(test_data_g$predicted_logit, levels = c("Adoption", "No adoption"))
test_data_g$Total.Outcome.Type <- factor(test_data_g$Total.Outcome.Type, levels = c("Adoption", "No adoption"))

# Ahora crea la matriz de confusión
confusionMatrix(test_data_g$predicted_logit, test_data_g$Total.Outcome.Type)
# Graficar la curva ROC
plot(logit_roc, col = "purple", main = "Curva ROC - Regresión Logística")
text(1, 1, labels = paste("AUC:", round(logit_auc, 3)), col = "purple", cex = 1.2)



summary(modelo_logit)


#Naive
# Ajustando el modelo
modelo_nb <- naiveBayes(Total.Outcome.Type ~ ., data = train_data_g)

# Predicciones en el conjunto de prueba
test_data_g$nb_pred <- predict(modelo_nb, newdata = test_data_g)

# Obtener las probabilidades de las clases
nb_probabilidades <- predict(modelo_nb, newdata = test_data_g, type = "raw")

# Obtener las probabilidades de la clase "Adoption"
nb_prob_adoption <- nb_probabilidades[, "Adoption"]

# Obtener predicciones y etiquetas verdaderas
nb_predicciones <- test_data_g$nb_pred  # Mantener como factor
nb_etiquetas <- test_data_g$Total.Outcome.Type  # Mantener como factor

# Asegurarse de que ambas sean factores con los mismos niveles
nb_predicciones <- factor(nb_predicciones, levels = levels(nb_etiquetas))

# Crear la matriz de confusión
cm_nb <- confusionMatrix(nb_predicciones, nb_etiquetas)

# Imprimir la matriz de confusión
print(cm_nb)

#Matriz como mapa de calor
# Convertir la matriz de confusión a un formato adecuado para ggplot
cm_table <- as.data.frame(cm_nb$table)

# Reordenar los niveles de la matriz para el orden deseado en filas y columnas
cm_table$Prediction <- factor(cm_table$Prediction, levels = c("Adoption", "No adoption"))
cm_table$Reference <- factor(cm_table$Reference, levels = c("Adoption", "No adoption"))

# Crear un gráfico de la matriz de confusión como matriz de calor
ggplot(data = cm_table, aes(x = Reference, y = Prediction)) +
  geom_tile(aes(fill = Freq), color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  geom_text(aes(label = Freq), vjust = 1) +
  theme_minimal() +
  labs(title = "Matriz de Confusión", x = "Referencia", y = "Predicción") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# Crear la curva ROC y calcular el AUC
nb_roc <- roc(nb_etiquetas, nb_prob_adoption, levels = c("Adoption", "No adoption"), direction = ">")
nb_auc <- auc(nb_roc)

# Imprimir el AUC
print(nb_auc)

# Graficar la curva ROC
plot(nb_roc, col = "purple", main = "Curva ROC - Naive Bayes")
text(1, 1, labels = paste("AUC:", round(nb_auc, 3)), col = "purple", cex = 1.2)

table(train_data_g$Total.Outcome.Type)


#Policolor
library(polycor)
correlacion <- polychor(train_data_g$Animal.Type, train_data_g$Age.in.Months)
print(correlacion)

correlacion1 <- polychor(train_data_g$Animal.Type, train_data_g$Breed.2)
print(correlacion1)

#Árbol

library(rpart)

# Cargar el paquete necesario para rpart
library(rpart)

# Ajustar el árbol de decisión
arbol_decision <- rpart(Total.Outcome.Type ~ ., data = train_data_g, method = "class")

# Instalar y cargar el paquete partykit (si no lo tienes instalado, ejecuta el siguiente comando)
# install.packages("partykit")
library(partykit)

# Convertir el árbol de decisión a un objeto de clase 'party'
arbol_party <- as.party(arbol_decision)

# Mostrar el gráfico del árbol de decisión, No es opción por las bastantes clases.
plot(arbol_party)

#Importancia de variables con rf

library(randomForest)
modelo_rf <- randomForest(Total.Outcome.Type~ ., data = train_data_g, importance = TRUE)
# Obtener la importancia de las variables
importancia <- importance(modelo_rf)
print(importancia)

# Gráfico de la importancia de las variables
varImpPlot(modelo_rf, 
           main = "Importancia de las Variables", 
           col = "steelblue",  
           pch = 16,           
           cex = 1.5)        
#Indica que por sexo hay mucha importancia


adoptados <- test_data_g[train_data_g$Total.Outcome.Type == "Adoption", ]

# Calcular el número de adoptados por sexo
adoptados_por_sexo <- table(adoptados$Sex.upon.Outcome)

# Calcular el número total de animales por sexo
total_por_sexo <- table(test_data_g$Sex.upon.Outcome)

# Calcular la proporción de adoptados por sexo
proporcion_adoptados_por_sexo <- adoptados_por_sexo / total_por_sexo

# Mostrar la proporción
print(proporcion_adoptados_por_sexo)


#Turf para perros ...


# Convertir la edad de meses a años
train_data_g$Age.in.Years <- train_data_g$Age.in.Months / 12

# Convertir la edad de meses a años
train_data_g$Age.in.Years <- train_data_g$Age.in.Months / 12

# Convertir 'Total.Outcome.Type' a binaria
train_data_g$Total.Outcome.Type <- factor(train_data_g$Total.Outcome.Type, levels = c("Adoption", "No adoption"))

# Calcular las combinaciones y sus frecuencias
turf_data <- train_data_g %>%
  filter(Animal.Type == "Dog", Breed.2 != "Domestic Shorthair") %>%  
  group_by(Sex.upon.Outcome, Age.in.Years = floor(Age.in.Years), Breed.2) %>%  
  summarise(Adoption_Count = sum(Total.Outcome.Type == "Adoption"),
            No_Adoption_Count = sum(Total.Outcome.Type == "No adoption"),
            Total_Count = n(), .groups = 'drop') %>%  
  arrange(desc(Adoption_Count)) %>%
  top_n(10, Adoption_Count)  

# Visualizar el turf
ggplot(turf_data, aes(x = reorder(paste("Dog", Sex.upon.Outcome, Age.in.Years, Breed.2), -Adoption_Count), y = Adoption_Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Top 10 combinaciones de adopción", x = "Combinaciones", y = "Cantidad de adopciones") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#No adopción

turf_data_no <- train_data_g %>%
  filter(Animal.Type == "Dog", Breed.2 != "Domestic Shorthair") %>%  
  group_by(Sex.upon.Outcome, Age.in.Years = floor(Age.in.Months / 12), Breed.2) %>%  # Convertir a años
  summarise(Adoption_Count = sum(Total.Outcome.Type == "Adoption"),
            No_Adoption_Count = sum(Total.Outcome.Type == "No adoption"),
            Total_Count = n(), .groups = 'drop') %>%  
  arrange(desc(No_Adoption_Count)) %>%
  top_n(10, No_Adoption_Count)  

# Visualizar el turf
ggplot(turf_data_no, aes(x = reorder(paste(Sex.upon.Outcome, Age.in.Years, Breed.2), -No_Adoption_Count), y = No_Adoption_Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Top 10 Combinaciones para No Adopción", x = "Combinaciones", y = "Cantidad de No Adopciones") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
