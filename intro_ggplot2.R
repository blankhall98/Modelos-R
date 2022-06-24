library(tidyr)
library(ggplot2)
library(dplyr)

head(iris)

dim(iris)

names(iris)

#scatterplot
g1 <- ggplot(data=iris,
             aes(x=Sepal.Length, y=Sepal.Width,
                 color=Species)) +
  geom_point()

#smooth
g1 + geom_smooth()

#text
g1 + geom_smooth() +
  geom_text(aes(label=Species))

#cambiar propiedades estaticas
g1 + geom_point(size=3,color='red')

#LINEPLOT

gdp_calculator <- function(year){
  g <- 1000 + (year)^2
  return(g)
}

años <- c(1900:2020)

paises <- c('USA','MEX')

gdp <- data.frame(
  'año' = años,
  'gdp' = gdp_calculator(años),
  'pais' = factor(sample(paises,length(años),replace=TRUE))
)

g2 <- ggplot(data=gdp,aes(x=año,y=gdp)) +
  geom_line() +
  facet_wrap(~pais)
g2

#Boxplot
g3 <- ggplot(data=iris, aes(x=Species,y=Sepal.Length,fill=Species)) +
  geom_boxplot() +
  labs(x='Especia de Florecita',y='Longitud del Sépalo de la Florecita',
       fill='Especie de la Florecita')

g4 <- g3 + coord_flip()
g4

#histograma
g5 <- ggplot(data=iris)+
  geom_histogram(aes(x=Petal.Width),bins = 25,fill='orange')
g5

#facet wrap
head(iris)

fw <- ggplot(data=iris,aes(x=Sepal.Length,y=Petal.Length)) + geom_point()
fw2 <- fw + facet_wrap(~Species)

fw2 + geom_smooth() + theme_gray()

g_sepal <- ggplot(data=iris,aes(x=Sepal.Length)) +
  geom_point(aes(y=Sepal.Width),color='pink')+
  geom_point(aes(y=Petal.Length),color='blue')+
  geom_point(aes(y=Petal.Width),color='green')
g_sepal