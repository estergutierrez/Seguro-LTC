library("readxl")
library("writexl")
library("dplyr")
library("tidyverse")
library("ggplot2")
library("tictoc")
# library("cowplot")


dataEdadesH <- read_excel("C:/Users/Ana/Desktop/II-2023/Contingencias de vida II/BaseDatosProyecto.xlsx", sheet = "PobHombres")
dataEdadesM <- read_excel("C:/Users/Ana/Desktop/II-2023/Contingencias de vida II/BaseDatosProyecto.xlsx", sheet = "PobMujeres")
dataProbH <- read_excel("C:/Users/Ana/Desktop/II-2023/Contingencias de vida II/BaseDatosProyecto.xlsx", sheet = "PTHombres")
dataProbM <- read_excel("C:/Users/Ana/Desktop/II-2023/Contingencias de vida II/BaseDatosProyecto.xlsx", sheet = "PTMujeres")
dataFuerzaTrans <- read_excel("C:/Users/Ana/Desktop/II-2023/Contingencias de vida II/BaseDatosProyecto.xlsx", sheet = "MatrizFT")
Tinfl <- 0.0329



## Funciones necesarias para las actuariales


# Determina el valor de fila en el cual se encuentra la edad buscada
clasificarEdad <- function(edad) {
  if (edad >= 20 && edad <= 29) {
    return(0)
  } else if (edad >= 30 && edad <= 39) {
    return(1)
  } else if (edad >= 40 && edad <= 49) {
    return(2)
  } else if (edad >= 50 && edad <= 59) {
    return(3)
  } else if (edad >= 60 && edad <= 69) {
    return(4)
  } else if (edad >= 70 && edad <= 79) {
    return(5)
  } else if (edad >= 80 && edad <= 89) {
    return(6)
  } else {
    return(-1)
  }
}

# Obtiene la probabilidad de transición
probabilidadTransicion <- function(sexo, edad, estadoInicial, estadoFinal) {
  
  if(sexo == 0){
    matrizProbabilidades <- dataProbH
  } else if(sexo == 1){
    matrizProbabilidades <- dataProbM
  }
  
  # Calcula los índices, suma 2 por el cómo está la base de datos
  fila <- 2 + (estadoInicial-1) * 7 + clasificarEdad(edad)
  columna <- 2 + estadoFinal-1
  
  # Extrae las probabilidades de transición
  proba <- as.numeric(as.vector(matrizProbabilidades[fila, columna]))
  
  return(proba)
}

# Obtiene la fuerza de transición, HOMBRE = 0, MUJER = 1
fuerzaTransicion <- function(sexo, estadoInicial, estadoFinal){
  
  # Calcula los índices, suma 1 por el cómo está la base de datos
  fila <- (sexo * 8) + estadoInicial
  columna <- 1 + estadoFinal 
  
  # Extrae las probabilidades de transición
  proba <- as.numeric(as.vector(dataFuerzaTrans[fila, columna]))
  
  return(proba)
}
#ejemplo <- fuerzaTransicion(1, 1, 2)

matrices <- function(sexo, x, estadoInicial) {
  k <- 90 -x
  memo1 <- matrix(NA, nrow = k+1, ncol = 6)  # Matriz para almacenar resultados calculados
  memo2 <- matrix(NA, nrow = k+1, ncol = 6)
  memo3 <- matrix(NA, nrow = k+1, ncol = 6)
  memo4 <- matrix(NA, nrow = k+1, ncol = 6)
  memo5 <- matrix(NA, nrow = k+1, ncol = 6)
  memo6 <- matrix(NA, nrow = k+1, ncol = 6)
  memo <- matrix(NA, nrow = k+1, ncol = 6)
  
  for (j in 1:k) {
    if (j == 1) {
      for (i in 1:6) {
        memo[1, i] <- probabilidadTransicion(sexo, x, estadoInicial, i) # Todas probs pasar de INICIAL a i
      }
    }
    if (j >= 2) {
      for (i in 1:6) {
        memo1[j, i] <- memo[j - 1, i] * probabilidadTransicion(sexo, x + j - 1, i, 1) # 
        memo2[j, i] <- memo[j - 1, i] * probabilidadTransicion(sexo, x + j - 1, i, 2) # 
        memo3[j, i] <- memo[j - 1, i] * probabilidadTransicion(sexo, x + j - 1, i, 3)
        memo4[j, i] <- memo[j - 1, i] * probabilidadTransicion(sexo, x + j - 1, i, 4)
        memo5[j, i] <- memo[j - 1, i] * probabilidadTransicion(sexo, x + j - 1, i, 5)
        memo6[j, i] <- memo[j - 1, i] * probabilidadTransicion(sexo, x + j - 1, i, 6)
      }
      memo[j,1] <- sum(memo1[j,])
      memo[j,2] <- sum(memo2[j,])
      memo[j,3] <- sum(memo3[j,])
      memo[j,4] <- sum(memo4[j,])
      memo[j,5] <- sum(memo5[j,])
      memo[j,6] <- sum(memo6[j,])
    }
  }
  
  # Normalizar las filas 
  memo <-t(apply(memo,1,function(x) x/sum(x)))
  memo[is.na(memo)] <- 0
  
  for(i in 1:5){
    memo[k,i] <- 0
  }
  memo[k,6] <- 1
  
  return(memo) # Devuelve la matriz de transición iniciando en el estado establecido y yendo a cualquier estado
}

# Creas la gran lista para hombre y mujer

granListaMujer <- vector("list", length = 70)
for (i in 20:90) {
  sublista <- list()
  for (j in 1:6) {
    submatriz <- matrices(1, i, j)  # Supongamos que matrices() devuelve la submatriz adecuada
    sublista[[j]] <- submatriz
  }
  granListaMujer[[i - 19]] <- sublista
} 

granListaMujer[[90 - 19]][[6]] <- matrix(c(0, 0, 0, 0, 0, 1), nrow = 1, ncol = 6)



## Funciones necesarias para la simulacion
# Se crear una lista de las probabilidades acumuladas para definir en que estado se encuentran
# según la edad
probs_M <- vector("list")
for (i in 20:90) {
  temp <- list()
  for (j in 1:6) {
    prueba1<-cumsum(granListaMujer[[i-19]][[j]][1,])  
    temp[[j]] <- prueba1
  }
  probs_M[[i-19]] <- temp
} 




########### Simulacion estocastica mujeres
# La simulacion estocastica se fundamenta en 3 pases:
#   1. Simular una vida
#   2. Simular una probabilidad
#   3. Simular las iteraciones 

### Demografico
#1. simular vida de mujeres 
simularvidaM<-function(x){
  estado= rep(6,100)
  estado[1]=c=1
  k=0
  while (c!=6 && x+k<=90){  # simula y asigna k+2
    vec <-runif(n=1,min=0,max=1)
    probs<-probs_M[[x+k-19]][[c]]
    if(vec<=probs[1]){
      estado[k+2]=c=1
    }else if( vec<=probs[2]){
      estado[k+2]=c=2
    }else if(vec<=probs[3]){
      estado[k+2]=c=3
    }else if(vec<=probs[4]){
      estado[k+2]=c=4
    }else if(vec<=probs[5]){
      estado[k+2]=c=5
    }else {estado[k+2]=c=6}
    k=k+1
  }
  return(estado)
}


# Se crean los vectores para generar la poblacion

edades <-seq(35,50) # vector con las edades a considerar
edad2023M <-subset(dataEdadesM, select = c(1,6)) # Seleccionar filas relevantes
edad2023M <- edad2023M[edad2023M$`Edades` %in% edades, ] # Filtrar filas por edad
edad2023M$`2023`<-edad2023M$`2023`*0.02 # calcular el % de poblacion a usar
edad2023M$`2023` <- round(edad2023M$`2023`, digits = 0) # redondear los valores

# Se cargan la cantidad de mujeres
cantidad_mujeres<- list(edad2023M$`2023`) 
cantidad_mujeres <-as.numeric(cantidad_mujeres[[1]]) 
# Este es un vector cantidad de elementos 
ciclo_mujeres<-rep(edades, times = cantidad_mujeres)# vector de poblacion femenina


# Simulacion de los universos

n<-2 # Numero de iteraciones
iteracionesM <-vector("list", length = n)
for(j in 1:length(iteracionesM)){
  tic("Cada ciclo")
  temp<-sapply(ciclo_mujeres,simularvidaM)
  print(paste0("Fin de iteracion numero ", j))
  toc()
  iteracionesM[[j]]<-t(temp)
}
toc()



# Funcion que retorna la cantidad de estados por año de cada matriz en la lista 
cantidad_Estados<-function(lista){
  total_estados <-vector("list", length = length(lista))
  k=1
  while(k<=length(lista)){
    tic("Ciclo")
    total_estados[[k]]<-t(apply(lista[[k]], MARGIN = 2, FUN = function(x) table(factor(x, levels = 1:6))))
    print(paste0("Fin de iteracion numero ", k))
    k=k+1
    toc()
  }
  return(total_estados)
}

# Lista de matrices cuyas columnas son los años y los estados, cada entrada representa
# la cantidad de personas en el estado j, en año t
total_estadosM<-cantidad_Estados(iteracionesM)

## Para calcular la esperanza, suma totas las matrices de la lista de matrices anterior
# y lo divide entre el número de iteraciones
esperanzaM <- (Reduce("+", total_estadosM))/length(iteracionesM)   



### Percentil 99.5

# Esta función convierte la lista de matrices que tiene el recuento de personas
# por estado y año en un distintas listas
vectoresM<-lapply(lapply(total_estadosM, t), function(x) as.vector(x))
# Eliminar las listas, colocarlos los elementos anteriores en un solo vector 
tempM<-unlist(vectoresM)

# Reacomoda los vectores anteriores en un listas de vectores de tal forma que la lista
# la lista 1 guarda todas las entradas (1,1) de todas las matrices en la lista total_estadosM
# en la lista 2 se guardan todas las en (1,2) de todas las matrices en la lista total_estadosM
# Y así sucesivamente
entradasM <-vector("list", length = (length(tempM)/n))
for(i in 1:(length(tempM)/n)){
  entradasM [[i]]<-tempM[cumsum(lengths(vectoresM))-(lengths(vectoresM)-c(i))]
}

#Le aplica la función percentil a cada una de las listas anteriores
percentilesM <-sapply(entradasM, FUN=function(x) quantile(x,probs=c(0.995)))
# Se acomodan los percenctiles en una matriz cuyas columnas representan los estados
# y las filas los años
percentilesM<-matrix(data=percentilesM, nrow=100,ncol=6, byrow = TRUE)
toc()


##### Proyeccion Financiera
## Se cargan las variables a utilizar en las funciones más adelante
# poblacion
poblacion<-c(1:length(ciclo_mujeres))
# Inflacion
power <- function(x, y) x^y
inflacion<-power(c(rep((Tinfl+1), times = 100)), c(0:99))


## Funciones a utilizar 
## función que devuelve una lista de matrices con los montos montos a pagar según el año
# de cada una de las simulaciones
indicadoras_estadosM<-function(estado, iteraciones, vector_beneficio, ciclo){
  indicadora<-lapply(iteraciones, FUN= function(x) matrix(data=as.numeric(x==estado), ncol = 100))
  
  beneficios <-vector("list", length = length(indicadora))
  for(k in 1:length(indicadora)){
    temp<-unlist(lapply(poblacion, function(i) {indicadora[[k]][i,]*vector_beneficio[(ciclo[i]-34):((ciclo[i]-34)+99)]}))
    beneficios[[k]]<-matrix(data=temp, ncol = 100, nrow = length(ciclo), byrow = TRUE)
  }
  return(beneficios)
  
}

# Esta función devuelve el percentil de financieron utilizando un algoritmo similar 
# al anterior
percentil_financiero<-function(suma_cols){
  vectores<-lapply(lapply(suma_cols, t), function(x) as.vector(x))
  temp<-unlist(vectores)
  entradas<-vector("list", length = (length(temp)/n))
  for(i in 1:(length(temp)/n)){
    entradas [[i]]<-temp[cumsum(lengths(vectores))-(lengths(vectores)-c(i))]
  }
  percentiles <-sapply(entradas, FUN=function(x) quantile(x,probs=c(0.995)))
  percentiles<-matrix(data=percentiles, nrow=100,ncol=1, byrow = TRUE)
  return(percentiles)
}


### Ingresos 
Prima<-6434838 
Prima_neta<-Prima*0.95
# se define un vector de primas el cual 
vector_primas<-c(rep(Prima_neta,times = 30),rep(0, times = 26),rep(0,times =60))

# Retorna una lista de matrices la cual determina los ingresos netos a pagar según 
# para cada una de las simulaciones hechas anteriormente
primas_netasM <-indicadoras_estadosM(1, iteracionesM, vector_primas, ciclo_mujeres)

# Se suman todas las columnas de cada matriz en la lista anterior e indexa los montos 
# a la inflacion
suma_colsprimaM<-lapply(primas_netasM, FUN=function(x) colSums(x)*inflacion)

# Se suman todos los valores anteriores y se divide entre el número de simulaciones
esperanza_financieraprimasM <- (Reduce("+", suma_colsprimaM))/n
# Acomodar el resultado en una matriz
esperanza_financieraprimasM<-matrix(data=esperanza_financieraprimasM, nrow=100,ncol=1, byrow = TRUE)


# Percentil
# Se calcula los percentiles los ingresos por años
percentilesMprima<-percentil_financiero(suma_colsprimaM)






### Egresos
#### Estados 2 y 3
beneficio1<- 3000000 #costo fij

# vector para realizar calcular los montos por año
vector_beneficio1<-c(rep(0,times = 30),rep(beneficio1, times = 26),rep(0,times =60))


### Estado 2
# Retorna una lista de matrices la cual determina los egresos asociados montos a pagar 
# según el estado 2 y la edad para cada una de las simulaciones hechas anteriormente
beneficiosM2<-indicadoras_estadosM(2, iteracionesM, vector_beneficio1, ciclo_mujeres)
# Calcula los egresos por año para cada una de las simulaciones realizadas
suma_cols2<-lapply(beneficiosM2, FUN=function(x) colSums(x)*inflacion)
# Esperanza la esperanza los egresos del estado 2
esperanza_financiera2 <- (Reduce("+", suma_cols2))/n
esperanza_financiera2<-matrix(data=esperanza_financiera2, nrow=100,ncol=1, byrow = TRUE)

# Percentiles los percentiles de los egresos del estado 2
percentilesM2<-percentil_financiero(suma_cols2)



### Estado 3


# Función que retorna los egresos totales relacionados al estado 3 para cada una de las simulaciones
suma_cols3<-lapply(indicadoras_estadosM(3, iteracionesM, vector_beneficio1, ciclo_mujeres),
                   FUN=function(x) colSums(x)*inflacion)
# Esperanza de los egresos del estado 3
esperanza_financiera3 <- (Reduce("+", suma_cols3))/n
esperanza_financiera3<-matrix(data=esperanza_financiera3, nrow=100,ncol=1, byrow = TRUE)
# Percentil de los egresos del estado 3
percentilesM3<-percentil_financiero(suma_cols3)



#### Estados 4 y 5 
# Se construye un nuevo vector de beneficios relacionados a estos nuevos dos estados
beneficio2<-7000000
vector_beneficio2<-c(rep(0,times=30),rep(beneficio2,times=26),rep(0,times=60))

### Estado 4

# Retorna una lista de los egresos relacionados al estado 4 de cada una de las simulaciones
suma_cols4<-lapply(indicadoras_estadosM(4, iteracionesM, vector_beneficio2, ciclo_mujeres), FUN=function(x) colSums(x)*inflacion)
# Esperanza
esperanza_financiera4 <- (Reduce("+", suma_cols4))/n
esperanza_financiera4<-matrix(data=esperanza_financiera4, nrow=100,ncol=1, byrow = TRUE)
# Percentil
percentilesM4<-percentil_financiero(suma_cols4)




### Estado 5
# Retorna los egresos relacionados al estado 5 por año en cada una de las simulaciones realizadas
suma_cols5<-lapply(indicadoras_estadosM(5, iteracionesM, vector_beneficio2, ciclo_mujeres), FUN=function(x) colSums(x)*inflacion)
# Esperanza
esperanza_financiera5 <- (Reduce("+", suma_cols5))/n
esperanza_financiera5 <-matrix(data=esperanza_financiera5 , nrow=100,ncol=1, byrow = TRUE)

# Percentil
percentilesM5<-percentil_financiero(suma_cols5)




#### Estado 6
# Se construye el vector para calcular los egresos relacionados al estado 6
Suma_aegurada<-10000000 
Gasto_financiero<-300000
vector_SA<-c(rep(Suma_aegurada,times = 30),rep(0, times = 26),rep(0,times =60))
vector_Gasto<-c(rep(Gasto_financiero,times = 30),rep(Gasto_financiero, times = 26),rep(0,times =60)) 
vector_estado6<-vector_SA+vector_Gasto



# Construcción de la indicadora especial
# Se crea una lista de matrices indicadoras que retorna 1 si en individuo se encuentra en el estado 6
# en el año t y 0 en otro caso
indicadora_temp<-lapply(iteracionesM, FUN= function(x) matrix(data=as.numeric(x==6), ncol = 100))



# Funcion para obtener los indices del primer 1 de las matrices anteriores, es decir el año en que murio
colIndexes<-vector("list", length = length(indicadora_temp))
for(i in 1:length(indicadora_temp)){
  colIndexes[[i]]<-unlist(apply(indicadora_temp[[i]],1, FUN=function(x) min(which(x==1))))
}
# Se crean una matriz de 0 para reemplazar con los indices 
matrices_ceros <-vector("list", length = length(indicadora_temp))
remove(indicadora_temp)
for(i in 1:length(matrices_ceros)){
  matrices_ceros[[i]]<-matrix(0, ncol = 100, nrow = length(ciclo_mujeres))
}
# Indices de fila
rowIndexes<-c(1:length(ciclo_mujeres))

# Se crea la indicadora que solo devuelve un 1 por persona
for(i in 1:length(matrices_ceros)){
  matrices_ceros[[i]][cbind(rowIndexes, colIndexes[[i]])] <- 1
}



# Se aplica el mismo algoritmo anterior para obtener la matriz con los egresos
# asociados al estado 6
for(k in 1:length(matrices_ceros)){
  temp<-unlist(lapply(poblacion, function(i) 
  { matrices_ceros[[k]][i,]*vector_estado6[(ciclo_mujeres[i]-34):((ciclo_mujeres[i]-34)+99)]}))
  matrices_ceros[[k]]<-matrix(data=temp, ncol = 100, nrow = length(ciclo_mujeres), byrow = TRUE)
}

suma_cols6<-lapply(matrices_ceros, FUN=function(x) colSums(x)*inflacion)
# Esperanza
esperanza_financiera6<- (Reduce("+", suma_cols6))/n
esperanza_financiera6<-matrix(data=esperanza_financiera6, nrow=100,ncol=1, byrow = TRUE)

# Percentil
percentilesM6<-percentil_financiero(suma_cols6)
