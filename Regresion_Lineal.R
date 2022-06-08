library(ggplot2)

#importar la data con la cual trabajaremos
data("mtcars")

#pedir informacion de la base
help(mtcars)

# un vistazo a las primeras filas de una base de datos
head(mtcars)

#inspeccionar las columnas
names(mtcars)

# dimensiones (renglon columna)
dim(mtcars)

# numero de renglones
length(mtcars)

#acceder a renglones mediante índices
mtcars[1,1]

#acceder a columnas
mtcars$mpg

### LIMPIEZA DE LA BASE
mtcars$vs <- as.factor(mtcars$vs)

mtcars$am <- as.factor(mtcars$am)

# ESTRUCTURA DE LA BASE
summary(mtcars)

#### ANALISIS DESCRIPTIVO DE LA BASE
ggplot(data=mtcars) +
  geom_point(aes(x=wt,y=mpg,color=vs,size=cyl)) +
  ggtitle('Encontrando el coche más ahorrador','Un estudio por Jonatan Blank')

### ANALISIS CUANTITATIVO DE LA BASE
ggplot(data=mtcars) + 
  geom_point(aes(x=wt,y=cyl))

cor(x=mtcars$wt,y=mtcars$cyl)

ggplot(data=mtcars) + 
  geom_boxplot(aes(x=vs,y=mpg))

ggplot(data=mtcars) + 
  geom_boxplot(aes(x=am,y=mpg))

# ANALIS DE CORRELACION
table(mtcars$vs,mtcars$am)

#REGRESION LINEAL
modelo1 <- lm(data = mtcars, mpg ~ wt + cyl)

summary(modelo1)

modelo2 <- lm(data = mtcars, mpg ~ wt + cyl + vs + am)

summary(modelo2)

modelo3 <- lm(data = mtcars, mpg ~ wt + am)

summary(modelo3)

#cosas chidas
modelo.peso <- lm(data=mtcars,wt ~ cyl + vs + am)

summary(modelo.peso)
