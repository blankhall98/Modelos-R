# EN ESTE ARCHIVO GUARDAMOS UNA INTRODUCCIÓN DE R 

claseR <- data.frame(
  'alumnos'= c('Juan','Pepe','Toño','Fernanda','Sofia'),
  'edades' = c(19,45,47,21,23),
  'calificaciones' = c(7,8,9,10,9),
  'sexo' = c('Hombre','Hombre','Hombre','Mujer','Mujer')
)

claseR$sexo <- as.factor(claseR$sexo)

table(claseR$sexo,claseR$edades)

genteLista <- claseR[claseR$calificaciones >= 9,]

chicasListas <- claseR[claseR$calificaciones >= 9 & claseR$sexo == "Mujer",]

chicosListos <- subset(x=claseR, subset = claseR$calificaciones >= 9 & claseR$sexo == "Hombre")
