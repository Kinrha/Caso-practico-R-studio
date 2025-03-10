#Para llamar el paquete deseado, en este caso tidyverse se usa library

library(tidyverse)

# Leer el archivo Titanicv2.csv

Tc <- read.csv("Titanicv2.csv")

# La función str arroja un resumen de todas las columnas, dando el tipo de dato
# de cada columna

str(Tc)
# Por ejemplo, podemos ver que las columnas PaassengerId, sex, Age, SibSp, Parch
# son enteros.

# La función summary arroja un resumen del tipo estadístico de los datos

summary(Tc)

# Por ejmplo, podemos ver que la edad mínima de los pasageros es de 0.17 y la 
# máxima de 76, que media es de 30.27 y la mediana de 27.

#La siguiente función nos dice cuántos valores nules hay en cada columna.

colSums(is.na(Tc))

# Entonces, según los resultados de R, se desconoce la edad de 86 pasajeros, 
# y del costo de un ticket

# El operador pie, permite contraer varios pasos en uno, sin necesidad de 
# definir una variable en cada caso 

# Con el operador pipe, seleccionamos la lista de pasgeros y si sobrevivieron 
# o no, en el segundo paso filtramos a los sobrevivientes.

Tc %>% select(Survived, Name)  %>%
  filter(Survived == "Yes") %>%
  head(418) 

# Según R, tenemos 152 sobrevientes.


# En el siguiente, agrupamos por lugar de embarque, después filtamos los valores
# distintos de NA en edad, para posteriormente obtener su media de edad por 
# lugar de embarque, ordenados de menor a mayor.


Tc %>% 
  group_by(Embarked) %>%
  filter (Age != "NA") %>%
  summarise(Media.edad = mean(Age)) %>%
  ungroup() %>%
  arrange(desc(Media.edad))

# Lo que se obtuvo fue:
# 1. Cherbourg         34.7
#  2 Queenstown        29.3
#  3 Southampton       28.8


# en el siguiente agrupamos por clase social, para posteriormente obtener un 
# conteo de cada unalos, ordenadas de mayor a menor.


Tc %>% 
  group_by(Pclass) %>%
  summarise(Cantidad = n()) %>%
  ungroup() %>%
  arrange(desc(Cantidad))


# En el siguiente, agrupamos por clase social, después filtamos los valores
# por sobrevientes, para posteriormente obtener un conteo de los sobreviviente
# por clase social, ordenadas de mayor a menor.

Tc %>% 
  group_by(Pclass) %>%
  filter (Survived== "Yes") %>%
  summarise(Cantidad = n()) %>%
  ungroup() %>%
  arrange(desc(Cantidad))

# Lo que se obtuvo fue que 
# Clase baja         72
# Clase media        30
# Clase alta         50

# En el siguiente, averiguamos cuantos menores abordaron el barco.

Tc %>% 
  filter (Age<15) %>%
  summarise(Cantidad = n()) %>%
  arrange(desc(Cantidad))

# Son 31 menores de 15.

# En el siguiente, averiguamos cuantos menores de 15 abordaron el barco y 
# sobrevivieron. 

Tc %>% 
  filter (Age<15, Survived == "Yes") %>%
  summarise(Cantidad = n()) %>%
  arrange(desc(Cantidad))

# Sólo 12 menores de 15 sobrevivieron.

# En el siguiente averiguamos cuántas mujeres abordaron el barco.    
Tc %>% 
  filter (Sex == "female") %>%
  summarise(Cantidad = n()) %>%
  arrange(desc(Cantidad))

# Abordaron el barco 152 mujeres    

# En el siguiente averiguamos cuántas mujeres abordaron el barco y sobrevivieron.    
Tc %>% 
  filter (Sex == "female", Survived == "Yes") %>%
  summarise(Cantidad = n()) %>%
  arrange(desc(Cantidad))

# sobrevivieron 152 mujeres, es decir todas las mujeres sobrevivieron.    

# En el siguiente averiguamos cuántos hombres abordaron el barco.    
Tc %>% 
  filter (Sex == "male") %>%
  summarise(Cantidad = n()) %>%
  arrange(desc(Cantidad)) 

# Abordaron el barco 266 hombres 

# En el siguiente averiguamos cuántas mujeres abordaron el barco y sobrevivieron.    
Tc %>% 
  filter (Sex == "male", Survived == "Yes") %>%
  summarise(Cantidad = n()) %>%
  arrange(desc(Cantidad))

# Ningun hombre sobrevió, según esta base de datos. 

#Visualización de datos

# ggplot2 gráfica a manera de capas: Data, Aesthetics,Geometries, Facets, Statiscs,
# Coordinates y Theme.

# Primera capa: Marcos y coordenadas; ggplot()

# Segunda capa: Aesthetics (datos); ggplot(aes()), x para filas y y para columnas
# y color(legendas)

#Tercera capa: Tipo de gráfico; geom_bar(), geom_histogram()

# Las capas se separan o aumentan con un más.

Tc %>% 
  ggplot(aes(x=Embarked)) +
  geom_bar()

#El geom_bar() dicta que se hará una gráfica de barras.
#La gráfica muestra el número de pasageros por lugar de embarcación. Resaltando 
# mayor número de pasageros fue en Southampton.

Tc %>% 
  ggplot(aes(x=Age,fill = Embarked)) +
  geom_histogram(alpha=0.7)

# El geom_histogram() dicta que se hará un histograma.
# El operador fill dicta cómo se va a rellenar, en este caso de acuerdo a lugar 
# de embarque.
# El parámento alpha, aplica el grado de transparencia.

# En la gráfica se observa que la persona de más edad, casi 80, embarco en 
# Southampton.

Tc %>% 
  ggplot(aes(x=Embarked, y = Age)) +
  geom_boxplot()

# El operador geom_boxplot ddicta un diagrama de cajas.
# El diagrama muestra que la media de Cherbourg es la más alta,
# alrededor de los 35, mientras que de lo dos embarque restantes, no llega 
# a 30, entre 20 y 25. sin embargo, el embarque Southampton muestra más dispersión 
# en sus datos.

Tc %>%
  ggplot(aes(x=Embarked, y = Survived)) +
  ()
  