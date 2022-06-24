library(dplyr)
library(ggplot2)

head(iris)

#averiguar especies únicas
especies <- iris$Species
especies_unicas <- levels(especies)

versi <- iris %>% filter(Species == "versicolor")

floresota <- iris %>% filter(Sepal.Length > 6.5) %>%
  filter(Sepal.Width > 3)

floresota %>% arrange(desc(Sepal.Length))

# 5 flores virginica con el largo de pétalo más pequeño
iris %>% filter(Species == 'virginica') %>% arrange(Petal.Length) %>% head(5)

#mutate
iris <- iris %>% mutate(Sepal.Area = Sepal.Length*Sepal.Width)
iris <- iris %>% mutate(Petal.Area = Petal.Length*Petal.Width)
iris <- iris %>% mutate(Prop = Petal.Area/Sepal.Area)

ggplot(data=iris,aes(x=Petal.Area,y=Sepal.Area,col=Prop)) + geom_point() + 
  facet_wrap(~Species)
