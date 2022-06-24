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

# Clase 2
v1 <- c(1:100) # suma de 1 hasta n --> n(n+1)/2
sum(v1)

mi_primera_funcion <- function(nombre,edad,salario){
  if(edad > 18){
    print('Ya estas viejo')
  }else{
    print('Tas chavo')
  }
  print(paste('Hola',nombre,', tienes',edad,'años'))
}

#ifelse
vector_prueba <- c(10,15,20,27,28,29)

for(v in vector_prueba){
  if(v > 17){
    print('Mayor de edad')
  }else{
    print('menor de edad')
  }
}

ifelse(vector_prueba > 17,'mayor de edad','menor de edad')

#dos maneras de iterar sobre un vector
v2 <- c('Uno','Dos','Tres')

for(num in v2){
  print(num)
}

for(i in 1:length(v2)){
  print(v2[i])
}

#while

n_sim <- 1000
val <- 0
sim <- c()
while(val < 1000){
  sim_n <- rnorm(10,0,1)
  sd_n <- sqrt(var(sim_n))
  sim <- c(sim,sd_n)
  
  val <- val + 1
}
mean(sim)


numero_minimo <- 100

media_cercana <- 40
desviacion_permitida <- 2

contador <- 0

aceptados <- c()

while(length(aceptados)<100){
  candidato <- rnorm(1,30,10)
  if(candidato > media_cercana-desviacion_permitida && candidato < media_cercana+desviacion_permitida){
    aceptados <- c(aceptados,candidato)
  }
  contador <- contador + 1
}

#apply
m <- matrix(1:12,nrow = 4,ncol = 3)
apply(m,1,sum)
apply(m,2,sum)

#lapply
v3 <- data.frame(
  'salario'= c(1000,3000,2100),
  'edad'= c(30,45,67),
  'educ'= c(5,8,4)
)

for(name in v3){
  print(paste('Buenos días',name))
}

saludar <- function(nombre){
  print(paste('Buenos días',nombre))
}

desv <- function(x){
  s <- sqrt(var(x))
  return(s)
}

lapply(v3,desv)

# GRÁFICAS
x <- c(1:10)
y <- (x+1)^2

plot(x,y)

par(mfrow = c(2, 1))

plot(x,y,type='l')
plot(x,y,type = "b")

edades <- rnorm(100,mean=35,sd=12.5)
años_escolaridad <- rnorm(100, mean = 6 + edades/10, sd = edades/20)
salario <- 1000*años_escolaridad + 250*edades + rnorm(100,mean=0,sd=2500)
sexos <- c('H','M','NB','indef')
sexo <- sample(sexos,100,replace=T)

db <- data.frame(
  'age'= edades,
  'school' = años_escolaridad,
  'wage' = salario,
  'sex' = as.factor(sexo)
)

plot(db$wage)

plot(x=db$age, y = db$school)

#histograma
hist(db$age,breaks=15,main='Edad de Nuestra Muestra',freq = F)
lines(density(db$age))

#boxplot
boxplot(db$school, main = 'Boxplot de Años de educacion',
        xlab = 'Educación', ylab = 'Años')

boxplot(school ~ sex, data = db, 
        main = 'Boxplot separado por sexos',col = "lightblue")

#dotplot
dotchart(db$age,groups = db$sex)

#pairplot
sub_db <- db[,c('age','school','wage')]

pairs(sub_db , panel = panel.smooth)

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

pairs(sub_db , lower.panel = panel.cor, upper.panel = panel.smooth)

#regresion lineal
modelo_salario <- lm(wage ~ school + age, data = db)
summary(modelo_salario)

#cstomize
plot(x,y,
     xlab = 'Años',
     ylab = expression(paste("(Años"^"2",")")),
     xlim = c(-5,30), ylim = c(-10,200),
     bty = "l", pch = 11, col='dodgerblue1')
text(x = 28, y = 190, label = "Aaaaaaa", cex = 2)
