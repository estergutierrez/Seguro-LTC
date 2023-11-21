library("readxl")
library("dplyr")
library("writexl")
library("tidyverse")
library("ggplot2")
library("tictoc")




dataEdadesH <- read_excel("C:/Users/Ana/Desktop/II-2023/Contingencias de vida II/BaseDatosProyecto.xlsx", sheet = "PobHombres")
dataProbH <- read_excel("C:/Users/Ana/Desktop/II-2023/Contingencias de vida II/BaseDatosProyecto.xlsx", sheet = "PTHombres")
dataFuerzaTrans <- read_excel("C:/Users/Ana/Desktop/II-2023/Contingencias de vida II/BaseDatosProyecto.xlsx", sheet = "MatrizFT")
Tdesc <- 0.0547
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

granListaHombre <- vector("list", length = 70)
for (i in 20:90) {
  sublista <- list()
  for (j in 1:6) {
    submatriz <- matrices(0, i, j)  # Supongamos que matrices() devuelve la submatriz adecuada
    sublista[[j]] <- submatriz
  }
  granListaHombre[[i - 19]] <- sublista
} # matrices(0, edad, estadoInicial) = granListaHombre[[edad - 19]][[estadoInicial]]



granListaHombre[[90 - 19]][[6]] <- matrix(c(0, 0, 0, 0, 0, 1), nrow = 1, ncol = 6)
#Lista de probabilidades acomuladas para simular vida
probs_H <- vector("list")
for (i in 20:90) {
  temp <- list()
  for (j in 1:6) {
    prueba1<-cumsum(granListaHombre[[i-19]][[j]][1,])  
    temp[[j]] <- prueba1
  }
  probs_H[[i-19]] <- temp
} 


simularvidaH<-function(x){
  estado= rep(6,100)
  estado[1]=c=1
  k=0
  while (c!=6 && x+k<=90){# simula y asigna k+2
    vec <-runif(n=1,min=0,max=1)
    probs<-probs_H[[x+k-19]][[c]]
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

edades <-seq(35,50) 
edad2023H <-subset(dataEdadesH, select = c(1,6)) # seleccionar lo importante
edad2023H <- edad2023H[edad2023H$`Edades` %in% edades, ] # Filtrar filas por edad
edad2023H$`2023`<-edad2023H$`2023`*0.02 # calcular el % de poblacion a usar
edad2023H$`2023` <- round(edad2023H$`2023`, digits = 0) # redondear los valores

cantidad_hombres<- list(edad2023H$`2023`) # crear lista de la columna
cantidad_hombres <-as.numeric(cantidad_hombres[[1]]) # transformarla en double
ciclo_hombres<-rep(edades, times = cantidad_hombres)#


n<-5 # Cambiar si es necesario
tic("Total") 
iteracionesH <-vector("list", length = n)
for(j in 1:length(iteracionesH)){
  tic("Cada ciclo")
  temp<-sapply(ciclo_hombres,simularvidaH)
  print(paste0("Fin de iteracion numero ", j))
  toc()
  iteracionesH[[j]]<-t(temp)
}
toc()

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

total_estadosH<-cantidad_Estados(iteracionesH)

esperanzaH <- (Reduce("+", total_estadosH))/length(iteracionesH)


# ### Percentil 99.5
vectoresH<-lapply(lapply(total_estadosH, t), function(x) as.vector(x))
temp<-unlist(vectoresH)

entradasH <-vector("list", length = (length(temp)/n))
for(i in 1:(length(temp)/n)){
  entradasH [[i]]<-temp[cumsum(lengths(vectoresH))-(lengths(vectoresH)-c(i))]
}

percentilesH <-sapply(entradasH, FUN=function(x) quantile(x,probs=c(0.995)))
percentilesH<-matrix(data=percentilesH, nrow=100,ncol=6, byrow = TRUE)





##### Creacion de dataframes

df_esperanzaHombres<-as.data.frame(esperanzaH)

df_percentilHombres<-as.data.frame(percentilesH)
colnames(df_percentilHombres) <- c("1", "2","3","4","5","6") 

#Exportar como excel
write_xlsx(df_esperanzaHombres,"C:/Users/Ana/Desktop/II-2023/Contingencias de vida II/MediaHombres_demografica.xlsx")
write_xlsx(df_percentilHombres,"C:/Users/Ana/Desktop/II-2023/Contingencias de vida II/PercentilHombres_demografica.xlsx")

write.csv(df_esperanzaHombres, "C:/Users/Ana/Desktop/II-2023/Contingencias de vida II/MediaHombres_demografica.csv", row.names=FALSE)
write.csv(df_percentilHombres, "C:/Users/Ana/Desktop/II-2023/Contingencias de vida II/PercentilHombres_demografica.csv", row.names=FALSE)





##### Proyeccion Financiera
## Variables
# poblacion
poblacion<-c(1:length(ciclo_hombres))
# Inflacion
power <- function(x, y) x^y
inflacion<-power(c(rep((Tinfl+1), times = 100)), c(0:99))
## Tasa de interes
# interes<-power(c(rep((Tdesc+1), times = 115)), c(1:115))

## Funciones a utilizar 
indicadoras_estadosH<-function(estado, iteraciones, vector_beneficio, ciclo){
  indicadora<-lapply(iteraciones, FUN= function(x) matrix(data=as.numeric(x==estado), ncol = 100))
  beneficios <-vector("list", length = length(indicadora))
  for(k in 1:length(indicadora)){
    temp<-unlist(lapply(poblacion, function(i) {indicadora[[k]][i,]*vector_beneficio[(ciclo[i]-34):((ciclo[i]-34)+99)]}))
    beneficios[[k]]<-matrix(data=temp, ncol = 100, nrow = length(ciclo), byrow = TRUE)
  }
  return(beneficios)
  
}

# Percentil
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


Prima<-6434838 
Prima_neta<-Prima*0.95
#vector de primas netas
vector_primas<-c(rep(Prima_neta,times = 30),rep(0, times = 26),rep(0,times =60))


# Ingresos
#tic("Ingresos")
primas_netasH <-indicadoras_estadosH(1, iteracionesH, vector_primas, ciclo_hombres)
suma_colsprimaH<-lapply(primas_netasH, FUN=function(x) colSums(x)*inflacion)

esperanza_financieraprimasH <- (Reduce("+", suma_colsprimaH))/n
esperanza_financieraprimasH<-matrix(data=esperanza_financieraprimasH, nrow=100,ncol=1, byrow = TRUE)

percentilesHprima<-percentil_financiero(suma_colsprimaH)


# Creacion de df
df_ingresosHombresMedia<-as.data.frame(esperanza_financieraprimasH)
df_ingresosHombresPercentil<-as.data.frame(percentilesHprima)

write_xlsx(df_ingresosHombresMedia,"C:/Users/Ana/Desktop/II-2023/Contingencias de vida II/MediaHombres_ingresos.xlsx")
write_xlsx(df_ingresosHombresPercentil,"C:/Users/Ana/Desktop/II-2023/Contingencias de vida II/PercentilHombres_ingresos.xlsx")

write.csv(df_ingresosHombresMedia, "C:/Users/Ana/Desktop/II-2023/Contingencias de vida II/MediaHombres_ingresos.csv", row.names=FALSE)
write.csv(df_ingresosHombresPercentil, "C:/Users/Ana/Desktop/II-2023/Contingencias de vida II/PercentilHombres_ingresos.csv", row.names=FALSE)


## Egresos 
## Estado 1
Gasto_primas<-0.05*Prima

## Gastos de primas

vector_gasto<-c(rep(Gasto_primas,times = 30),rep(0, times = 26),rep(0,times =60))

primas_gastoH<-indicadoras_estadosH(1, iteracionesH, vector_gasto, ciclo_hombres)

# Egresos por a;o
suma_colsgastoH<-lapply(primas_gastoH, FUN=function(x) colSums(x)*inflacion)
# Esperanza
esperanza_financieragastoH <- (Reduce("+", suma_colsgastoH))/n
esperanza_financieragastoH<-matrix(data=esperanza_financieragastoH, nrow=100,ncol=1, byrow = TRUE)


# Percentil
percentilesHgasto <-percentil_financiero(suma_colsgastoH)


# Creacion de df
esperanza_egresosH<-as.data.frame(esperanza_financieragastoH)
Percentil_egresosH<-as.data.frame(percentilesHgasto)

write_xlsx(esperanza_egresosH,"C:/Users/Ana/Desktop/II-2023/Contingencias de vida II/MediaHombres_ingresos.xlsx")
write_xlsx(Percentil_egresosH,"C:/Users/Ana/Desktop/II-2023/Contingencias de vida II/PercentilHombres_ingresos.xlsx")

write.csv(esperanza_egresosH, "C:/Users/Ana/Desktop/II-2023/Contingencias de vida II/MediaHombres_ingresos.csv", row.names=FALSE)
write.csv(Percentil_egresosH, "C:/Users/Ana/Desktop/II-2023/Contingencias de vida II/PercentilHombres_ingresos.csv", row.names=FALSE)




### Estado 2
beneficio1<- 3000000 #costo fijo
vector_beneficio1<-c(rep(0,times = 30),rep(beneficio1, times = 26),rep(0,times =60))

beneficiosH2<-indicadoras_estadosH(2, iteracionesH, vector_beneficio1, ciclo_hombres)
# Egresos por a;o
suma_colsH2<-lapply(beneficiosH2, FUN=function(x) colSums(x)*inflacion)
# Esperanza
esperanza_financieraH2 <- (Reduce("+", suma_colsH2))/n
esperanza_financieraH2<-matrix(data=esperanza_financieraH2, nrow=100,ncol=1, byrow = TRUE)

# Percentiles
percentilesH2<-percentil_financiero(suma_colsH2)



### Estado 3
beneficiosH3<-indicadoras_estadosH(3, iteracionesH, vector_beneficio1, ciclo_hombres)
# Egresos por a;o
suma_colsH3<-lapply(beneficiosH3, FUN=function(x) colSums(x)*inflacion)
# Esperanza
esperanza_financieraH3 <- (Reduce("+", suma_colsH3))/n
esperanza_financieraH3<-matrix(data=esperanza_financieraH3, nrow=100,ncol=1, byrow = TRUE)
# Percentil
percentilesH3<-percentil_financiero(suma_colsH3)


#### Estados 4 y 5 
beneficio2<-7000000
vector_beneficio2<-c(rep(0,times=30),rep(beneficio2,times=26),rep(0,times=60))

### Estado 4
# Indicadora 
beneficiosH4<-indicadoras_estadosH(4, iteracionesH, vector_beneficio2, ciclo_hombres)

suma_colsH4<-lapply(beneficiosH4, FUN=function(x) colSums(x)*inflacion)
# Esperanza
esperanza_financieraH4 <- (Reduce("+", suma_colsH4))/n
esperanza_financieraH4<-matrix(data=esperanza_financieraH4, nrow=100,ncol=1, byrow = TRUE)
# Percentil
percentilesH4<-percentil_financiero(suma_colsH4)


### Estado 5
# Indicadora 
beneficiosH5<-indicadoras_estadosH(5, iteracionesH, vector_beneficio2, ciclo_hombres)
suma_colsH5<-lapply(beneficiosH5, FUN=function(x) colSums(x)*inflacion)
# Esperanza
esperanza_financieraH5 <- (Reduce("+", suma_colsH5))/n
esperanza_financieraH5 <-matrix(data=esperanza_financieraH5 , nrow=100,ncol=1, byrow = TRUE)

percentilesH5<-percentil_financiero(suma_colsH5)


#### Estado 6
Suma_aegurada<-10000000 
Gasto_finalizacion<-300000
vector_SA<-c(rep(Suma_aegurada,times = 30),rep(0, times = 26),rep(0,times =60))
vector_Gastofinalizacion<-c(rep(Gasto_finalizacion,times = 30),rep(Gasto_finalizacion, times = 26),rep(0,times =60)) 
vector_estado6<-vector_SA+vector_Gastofinalizacion


indicadora_temp<-lapply(iteracionesH, FUN= function(x) matrix(data=as.numeric(x==6), ncol = 100))
# Funcion para obtener los indices del primer 6, es decir cuando murio
colIndexes<-vector("list", length = length(indicadora_temp))
for(i in 1:length(indicadora_temp)){
  colIndexes[[i]]<-unlist(apply(indicadora_temp[[i]],1, FUN=function(x) min(which(x==1))))
  
}

# Se crean una matriz de 0 para reemplazar con los indices  
matrices_ceros <-vector("list", length = length(indicadora_temp))
for(i in 1:length(indicadora_temp)){
  matrices_ceros[[i]]<-matrix(0, ncol = 100, nrow = length(ciclo_hombres))
}
# Indices de fila
rowIndexes<-c(1:length(ciclo_hombres))
for(i in 1:length(indicadora_temp)){
  matrices_ceros[[i]][cbind(rowIndexes, colIndexes[[i]])] <- 1
}



beneficiosH6 <-vector("list", length = length(matrices_ceros))
for(k in 1:length(matrices_ceros)){
  temp<-unlist(lapply(poblacion, function(i) 
  {matrices_ceros[[k]][i,]*vector_estado6[(ciclo_hombres[i]-34):((ciclo_hombres[i]-34)+99)]}))
  beneficiosH6[[k]]<-matrix(data=temp, ncol = 100, nrow = length(ciclo_hombres), byrow = TRUE)
}

suma_colsH6<-lapply(beneficiosH6, FUN=function(x) colSums(x)*inflacion)
# Esperanza
esperanza_financieraH6<- (Reduce("+", suma_colsH6))/n
esperanza_financieraH6<-matrix(data=esperanza_financieraH6, nrow=100,ncol=1, byrow = TRUE)

# Percentil
percentilesH6<-percentil_financiero(suma_colsH6)




