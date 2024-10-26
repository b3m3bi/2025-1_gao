library(dplyr)
library(ggplot2)

#_______________Leemos los datos_________________________
Datos <-read.csv("C:/Users/greci/OneDrive/Documentos/Proyecto no github/carros densidad_flujo-table.csv", skip=6)

#Nos quedamos con lo importante
Datos <- Datos[, c(3,7,8)]

#Agrupamos los datos por cantidad de carros y filtramos para solo tener 
#la metrica del tick final
Datos %>% group_by(numero_de_carros) %>% filter(X.step. == max(Datos$X.step.))%>%
  ggplot(aes(x = numero_de_carros, y = metrica_flujo)) + 
  scale_x_continuous(breaks = seq(1,30,5)) +
  geom_point(alpha=0.3) + labs(x = "Número de carros", y = "Flujo de carros promedio") +
  theme_classic()
