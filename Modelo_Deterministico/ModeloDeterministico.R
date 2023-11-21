# Paquetes
library(extrafont)
font_import()
loadfonts(device = "win")
library(ggplot2)
library(cowplot)
library(readxl)

#
dataEdadesH <- read_excel("/Users/ester/Desktop/Instrucciones proyecto/BaseDatosProyecto.xlsx", sheet = "PobHombres")
dataEdadesM <- read_excel("/Users/ester/Desktop/Instrucciones proyecto/BaseDatosProyecto.xlsx", sheet = "PobMujeres")
dataProbH <- read_excel("/Users/ester/Desktop/Instrucciones proyecto/BaseDatosProyecto.xlsx", sheet = "PTHombres")
dataProbM <- read_excel("/Users/ester/Desktop/Instrucciones proyecto/BaseDatosProyecto.xlsx", sheet = "PTMujeres")
dataFuerzaTrans <- read_excel("/Users/ester/Desktop/Instrucciones proyecto/BaseDatosProyecto.xlsx", sheet = "MatrizFT")
Tdesc <- 0.0547
Tinfl <- 0.0329

# Hombres
poblacionH <-  as.data.frame(dataEdadesH)
row.names(poblacionH) <- poblacionH$Edades
poblacionH$Edades <- NULL
poblacionH <- round(poblacionH * 0.02)
poblacionTotalHombres <- sum(poblacionH$`2023`[37:52])

# Mujeres
poblacionM <-  as.data.frame(dataEdadesM)
row.names(poblacionM) <- poblacionM$Edades
poblacionM$Edades <- NULL
poblacionM <- round(poblacionM * 0.02)
poblacionTotalMujeres <- sum(poblacionM$`2023`[37:52])

# Función para realizar la proyección para hombres o mujeres
# Esta función tiene como parametros edad, la poblacion, las probabilidades de transición
# y el total de población 

# En estas listas se va a guardar la proyección para la edad x para cada año.
# Esto es: Para el año x+1 se calcula cuántas personas van a transicionar al estado 1,2,3,4,5,6.
# Para el año x+i se toma la población para cada estado y se calcula la probabilidad de transicionar 
# al estado 1,2,3,4,5,6.
# Finalmente se suma la población en cada estado y se guarda la distribución para ese año en la lista
# Ejemplo: La entrada deterministicoHombres[[x]][[n]][j] es la cantidad de asegurados de sexo
# masculino que adquirieron el seguro a la edad x + 34 y que se encuentran en el estado j, 
# después de n años de haber iniciado la cobertura del seguro.

proyectarGenero <- function(edad, poblacionGenero, granListaGenero, totalPoblacion) {
  proyeccion <- list()
  proyeccion[[1]] <- poblacionGenero$`2023`[edad + 2] * granListaGenero[[edad - 19]][[1]][1,]
  pobTotal <- sum(proyeccion[[1]])
  
  i <- 1
  while (i + edad - 19 <= 70 && proyeccion[[i]][6] < totalPoblacion) {
    estados <- lapply(1:6, function(j) proyeccion[[i]][j] * granListaGenero[[edad + i - 19]][[j]][1,])
    proyeccion[[i + 1]] <- Reduce(`+`, estados)
    i <- i + 1
  }

  return(proyeccion)
}

# Se crean las listas donde se va a guardar el modelo
deterministicoHombres <- list()
deterministicoMujeres <- list()

# Se inicia el modelo para cada edad según sexo
for (edad in 35:50) {
  deterministicoHombres[[paste("", edad)]] <- proyectarGenero(edad, poblacionH, granListaHombre, poblacionTotalHombres)
  deterministicoMujeres[[paste("", edad)]] <- proyectarGenero(edad, poblacionM, granListaMujer, poblacionTotalMujeres)
}

##### Modelo para los gráficos


# Ahora se separa el modelo por estado en lugar de estado y edad

# Se crea una función para calcular la población en un año específico
calcularPoblacion <- function(deterministicoHombres, indice) {
  poblacion <- rep(0, 56)
  poblacion[1] <- 0
  
  pobTemp <- rep(0, 16)
  
  for (t in 1:55) {
    for (x in 1:16) {
      if (length(deterministicoHombres[[x]]) >= t) {
        pobTemp[x] <- deterministicoHombres[[x]][[t]][indice]
      }
    }
    poblacion[t + 1] <- sum(pobTemp)
  }
  
  return(poblacion)
}

# Calcular poblaciones para cada estado del 1 al 6

poblacionE1 <- calcularPoblacion(deterministicoHombres, 1)
poblacionE1[1] <- sum(poblacionH$`2023`[37:52])
poblacionE2 <- calcularPoblacion(deterministicoHombres, 2)
poblacionE3 <- calcularPoblacion(deterministicoHombres, 3)
poblacionE4 <- calcularPoblacion(deterministicoHombres, 4)
poblacionE5 <- calcularPoblacion(deterministicoHombres, 5)
poblacionE6 <- calcularPoblacion(deterministicoHombres, 6)

poblacionE1M <- calcularPoblacion(deterministicoMujeres, 1)
poblacionE1M[1] <- sum(poblacionM$`2023`[37:52])
poblacionE2M <- calcularPoblacion(deterministicoMujeres, 2)
poblacionE3M <- calcularPoblacion(deterministicoMujeres, 3)
poblacionE4M <- calcularPoblacion(deterministicoMujeres, 4)
poblacionE5M <- calcularPoblacion(deterministicoMujeres, 5)
poblacionE6M <- calcularPoblacion(deterministicoMujeres, 6)


# GRAFICOS DE LA PROYECCION DETERMINISTICA


h<-5
w<-h*1.6

anios <- c(0:55)+2023
datos <- data.frame(anios, poblacionE1, poblacionE2, poblacionE3,
                    poblacionE4, poblacionE5, poblacionE6)

ggplot(datos, aes(x = anios)) +
  geom_line(aes(y = poblacionE1, color = "Estado 1"), size = 1) +
  geom_line(aes(y = poblacionE2, color = "Estado 2"), size = 1) +
  geom_line(aes(y = poblacionE3, color = "Estado 3"), size = 1) +
  geom_line(aes(y = poblacionE4, color = "Estado 4"), size = 1) +
  geom_line(aes(y = poblacionE5, color = "Estado 5"), size = 1) +
  geom_line(aes(y = poblacionE6, color = "Estado 6"), size = 1) +
  scale_color_manual(values = c("Estado 1" = "#DD6E42", "Estado 2" = "#AEC5EB",
                                "Estado 3" = "#EBD4AE", "Estado 4" = "#4F6D7A",
                                "Estado 5" = "#98A88B", "Estado 6" = "#011638")) +
  labs(x = "Tiempo (t)",
       y = "Cantidad de población") +
  theme_minimal() +
  theme_minimal_hgrid(font_size = 23) +
  theme(legend.position = "top", legend.justification = "center",
        axis.title.y = element_text(angle=90, size=rel(1), vjust = 1 ), 
        text = element_text(size = 15, family="serif"), axis.text = element_text(size = 15)) +
  guides(color = guide_legend(title = "Estado"))

#ggsave(filename = "Proyeccion_Deterministica1.pdf",width = w,height = h)

ggplot(datos, aes(x = anios)) +
  geom_line(aes(y = poblacionE2, color = "Estado 2"), size = 1) +
  geom_line(aes(y = poblacionE3, color = "Estado 3"), size = 1) +
  geom_line(aes(y = poblacionE4, color = "Estado 4"), size = 1) +
  geom_line(aes(y = poblacionE5, color = "Estado 5"), size = 1) +
  scale_color_manual(values = c( "Estado 2" = "#AEC5EB","Estado 3" = "#EBD4AE", 
                                 "Estado 4" = "#DD6E42", "Estado 5" = "#98A88B")) +
  labs(x = "Tiempo (t)",
       y = "Cantidad de población") +
  theme_minimal() +
  theme_minimal_hgrid(font_size = 23) +
  theme(legend.position = "top", legend.justification = "center",
        axis.title.y = element_text(angle=90, size=rel(1), vjust = 1 ), 
        text = element_text(size = 15, family="serif"), axis.text = element_text(size = 15)) +
  guides(color = guide_legend(title = "Estado"))

#ggsave(filename = "Proyeccion_Deterministica2.pdf",width = w,height = h)


datos <- data.frame(anios, poblacionE1M, poblacionE2M, poblacionE3M,
                    poblacionE4M, poblacionE5M, poblacionE6M)

ggplot(datos, aes(x = anios)) +
  geom_line(aes(y = poblacionE1M, color = "Estado 1"), size = 1) +
  geom_line(aes(y = poblacionE2M, color = "Estado 2"), size = 1) +
  geom_line(aes(y = poblacionE3M, color = "Estado 3"), size = 1) +
  geom_line(aes(y = poblacionE4M, color = "Estado 4"), size = 1) +
  geom_line(aes(y = poblacionE5M, color = "Estado 5"), size = 1) +
  geom_line(aes(y = poblacionE6M, color = "Estado 6"), size = 1) +
  scale_color_manual(values = c("Estado 1" = "#DD6E42", "Estado 2" = "#AEC5EB",
                                "Estado 3" = "#EBD4AE", "Estado 4" = "#4F6D7A",
                                "Estado 5" = "#98A88B", "Estado 6" = "#011638")) +
  labs(x = "Tiempo (t)",
       y = "Cantidad de población") +
  theme_minimal() +
  theme_minimal_hgrid(font_size = 23) +
  theme(legend.position = "top", legend.justification = "center",
        axis.title.y = element_text(angle=90, size=rel(1), vjust = 1 ), 
        text = element_text(size = 15, family="serif"), axis.text = element_text(size = 15)) +
  guides(color = guide_legend(title = "Estado"))

#ggsave(filename = "Proyeccion_Deterministica1M.pdf",width = w,height = h)

ggplot(datos, aes(x = anios)) +
  geom_line(aes(y = poblacionE2M, color = "Estado 2"), size = 1) +
  geom_line(aes(y = poblacionE3M, color = "Estado 3"), size = 1) +
  geom_line(aes(y = poblacionE4M, color = "Estado 4"), size = 1) +
  geom_line(aes(y = poblacionE5M, color = "Estado 5"), size = 1) +
  scale_color_manual(values = c( "Estado 2" = "#AEC5EB","Estado 3" = "#EBD4AE", 
                                 "Estado 4" = "#DD6E42", "Estado 5" = "#98A88B")) +
  labs(x = "Tiempo (t)",
       y = "Cantidad de población") +
  theme_minimal() +
  theme_minimal_hgrid(font_size = 23) +
  theme(legend.position = "top", legend.justification = "center",
        axis.title.y = element_text(angle=90, size=rel(1), vjust = 1 ), 
        text = element_text(size = 15, family="serif"), axis.text = element_text(size = 15)) +
  guides(color = guide_legend(title = "Estado"))

#ggsave(filename = "Proyeccion_Deterministica2M.pdf",width = w,height = h)


######### Inciso 10: Preparar un modelo determinístico de proyección anual con un 
# horizonte de proyección de 100 años que tenga como variable de salida, los montos 
# esperados de ingresos y egresos para cada uno de los estados y separado por sexo.

Beneficio1 <- 3000000
Beneficio2 <- 7000000
Beneficio3 <- 10000000 
GastoFinalizacion <- 300000
PrimaUnica <- as.numeric(primaUnica)

# Función para calcular ingresos netos para un género específico
calcularIngresosNetos <- function(poblacion, deterministico, PrimaUnica, Tinfl) {
  ingresos <- rep(0, 56)
#  ingresos[1] <- poblacion$`2023`[37:52] * PrimaUnica
  pob <- 0
  
  for (n in 1:30) {
    for (edad in 1:16) {
      if (34 + edad + n < 65) {
        pob <- pob + deterministico[[edad]][[n]][1]
      }
    }
    ingresos[n+1] <- pob * PrimaUnica * (1 + Tinfl)^n
    pob <- 0
  }
  
  ingresosNetos <- ingresos * 0.95
  ingresosNetos[1] <- sum(poblacion$`2023`[37:52]) * PrimaUnica * 0.95  # No hay descuento para el primer año
  return(ingresosNetos)
}

calcularIngresosBrutos <- function(poblacion, deterministico, PrimaUnica, Tinfl) {
  ingresos <- rep(0, 56)
  #  ingresos[1] <- poblacion$`2023`[37:52] * PrimaUnica
  pob <- 0
  
  for (n in 1:30) {
    for (edad in 1:16) {
      if (34 + edad + n < 65) {
        pob <- pob + deterministico[[edad]][[n]][1]
      }
    }
    ingresos[n+1] <- pob * PrimaUnica * (1 + Tinfl)^n
    pob <- 0
  }
  
  ingresosNetos <- ingresos
  ingresosNetos[1] <- sum(poblacion$`2023`[37:52]) * PrimaUnica  # No hay descuento para el primer año
  return(ingresosNetos)
}

# Función para calcular egresos para un género específico y un beneficio específico
calcularEgresos <- function(estado, deterministico, Beneficio, Tinfl) {
  egresos <- rep(0, 56)
  pob <- 0
  
  for (n in 15:55) {
    for (edad in 1:16) {
      if (n <= length(deterministico[[edad]]) & 34 + edad + n > 64) {
        pob <- pob + deterministico[[edad]][[n]][estado]
      }
    }
    egresos[n + 1] <- pob * Beneficio * (1 + Tinfl)^n
    pob <- 0
  }
  
  return(egresos)
}

# Calcula ingresos y egresos para hombres y mujeres
ingresos1Netos <- calcularIngresosNetos(poblacionH, deterministicoHombres, PrimaUnica, Tinfl)
ingresos1Brutos <- calcularIngresosBrutos(poblacionH, deterministicoHombres, PrimaUnica, Tinfl)
egresos2 <- calcularEgresos(2, deterministicoHombres, Beneficio1, Tinfl)
egresos3 <- calcularEgresos(3, deterministicoHombres, Beneficio1, Tinfl)
egresos4 <- calcularEgresos(4, deterministicoHombres, Beneficio2, Tinfl)
egresos5 <- calcularEgresos(5, deterministicoHombres, Beneficio2, Tinfl)

ingresos1MNetos <- calcularIngresosNetos(poblacionM, deterministicoMujeres, PrimaUnica, Tinfl)
ingresos1MBrutos <- calcularIngresosBrutos(poblacionM, deterministicoHombres, PrimaUnica, Tinfl)
egresos2M <- calcularEgresos(2, deterministicoMujeres, Beneficio1, Tinfl)
egresos3M <- calcularEgresos(3, deterministicoMujeres, Beneficio1, Tinfl)
egresos4M <- calcularEgresos(4, deterministicoMujeres, Beneficio2, Tinfl)
egresos5M <- calcularEgresos(5, deterministicoMujeres, Beneficio2, Tinfl)

# Estado 6
egresos6 <- rep(0, 56)
pob1 <- pob2 <- 0

for (n in 2:55) {
  for (edad in 1:16) {
    if (n <= length(deterministicoHombres[[edad]]) & 34 + edad + n > 64) {
      pob1 <- pob1 + abs(deterministicoHombres[[edad]][[n]][6] - deterministicoHombres[[edad]][[n - 1]][6])
    }
    if (n <= length(deterministicoHombres[[edad]]) & 34 + edad + n <= 64) {
      pob2 <- pob2 + abs(deterministicoHombres[[edad]][[n]][6] - deterministicoHombres[[edad]][[n - 1]][6])
    }
  }
  egresos6[n + 1] <- (GastoFinalizacion * pob1 + pob2 * (Beneficio3 + GastoFinalizacion)) * (1 + Tinfl)^n
  pob1 <- pob2 <- 0
}

egresos6M <- rep(0, 56)
pob1M <- pob2M <- 0

for (n in 2:55) {
  for (edad in 1:16) {
    if (n <= length(deterministicoMujeres[[edad]]) & 34 + edad + n > 64) {
      pob1M <- pob1M + abs(deterministicoMujeres[[edad]][[n]][6] - deterministicoMujeres[[edad]][[n - 1]][6])
    }
    if (n <= length(deterministicoMujeres[[edad]]) & 34 + edad + n <= 64) {
      pob2M <- pob2M + abs(deterministicoMujeres[[edad]][[n]][6] - deterministicoMujeres[[edad]][[n - 1]][6])
    }
  }
  egresos6M[n + 1] <- (1 + Tinfl)^n * (GastoFinalizacion * pob1M + pob2M * (Beneficio3 + GastoFinalizacion))
  pob1M <- pob2M <- 0
}

# GRAFICOS DEL FINANCIERO

# Estado 1
datos <- data.frame(anios, ingresos1Brutos, ingresos1MBrutos)

ggplot(datos, aes(x = anios)) +
  geom_line(aes(y = ingresos1Brutos/10000000, color = "Hombres"), linewidth = 1) +
  geom_line(aes(y = ingresos1MBrutos/10000000, color = "Mujeres"), linewidth = 1) +
  scale_color_manual(values = c("Hombres" = "#DD6E42", "Mujeres" = "#AEC5EB")) +
  geom_vline(xintercept = 30+2023, linetype="dashed", 
             color = "lightgrey", size=1) +
  geom_vline(xintercept = 15+2023, linetype="dashed", 
             color = "lightgrey", size=1) +
  scale_color_manual(values = c("Hombres" = "#DD6E42", "Mujeres" = "#AEC5EB")) +
  labs(x = "Año",
       y = "Monto en decenas \n de millones de colones") +
  theme_minimal() +
  theme_minimal_hgrid(font_size = 23) +
  theme(legend.position = "top", legend.justification = "center",
        axis.title.y = element_text(angle=90, size=rel(1), vjust = 1 ), 
        text = element_text(size = 15, family = "serif"), axis.text = element_text(size = 15)) +
  guides(color = guide_legend(title = "Género"))

#ggsave(filename = "Det_IngresosBrutosEstado1.pdf",width = w,height = h)

datos <- data.frame(anios, ingresos1Netos, ingresos1MNetos)

ggplot(datos, aes(x = anios)) +
  geom_line(aes(y = ingresos1Netos/10000000, color = "Hombres"), linewidth = 1) +
  geom_line(aes(y = ingresos1MNetos/10000000, color = "Mujeres"), linewidth = 1) +
  scale_color_manual(values = c("Hombres" = "#DD6E42", "Mujeres" = "#AEC5EB")) +
  geom_vline(xintercept = 30+2023, linetype="dashed", 
             color = "lightgrey", size=1) +
  geom_vline(xintercept = 14+2023, linetype="dashed", 
             color = "lightgrey", size=1) +
  scale_color_manual(values = c("Hombres" = "#DD6E42", "Mujeres" = "#AEC5EB")) +
  labs(x = "Año",
       y = "Monto en decenas \n de millones de colones") +
  theme_minimal() +
  theme_minimal_hgrid(font_size = 23) +
  theme(legend.position = "top", legend.justification = "center",
        axis.title.y = element_text(angle=90, size=rel(1), vjust = 1 ), 
        text = element_text(size = 15, family = "serif"), axis.text = element_text(size = 15)) +
  guides(color = guide_legend(title = "Género"))

#ggsave(filename = "Det_IngresosNetosEstado1.pdf",width = w,height = h)

# Estado 2
datos <- data.frame(anios, egresos2, egresos2M)

ggplot(datos, aes(x = anios)) +
  geom_line(aes(y = egresos2/10000000, color = "Hombres"), linewidth = 1) +
  geom_line(aes(y = egresos2M/10000000, color = "Mujeres"), linewidth = 1) +
  scale_color_manual(values = c("Hombres" = "#DD6E42", "Mujeres" = "#AEC5EB")) +
  geom_vline(xintercept = 30+2023, linetype="dashed", 
             color = "lightgrey", size=1) +
  geom_vline(xintercept = 15+2023, linetype="dashed", 
             color = "lightgrey", size=1) +
  scale_color_manual(values = c("Hombres" = "#DD6E42", "Mujeres" = "#AEC5EB")) +
  labs(x = "Año",
       y = "Monto en decenas \n de millones de colones") +
  theme_minimal() +
  theme_minimal_hgrid(font_size = 23) +
  theme(legend.position = "top", legend.justification = "center",
        axis.title.y = element_text(angle=90, size=rel(1), vjust = 1 ), 
        text = element_text(size = 15, family = "serif"), axis.text = element_text(size = 15)) +
  guides(color = guide_legend(title = "Género"))

#ggsave(filename = "Det_EgresosEstado2.pdf",width = w,height = h)

# Estado 3

datos <- data.frame(anios, egresos3, egresos3M)

ggplot(datos, aes(x = anios)) +
  geom_line(aes(y = egresos3/1000000, color = "Hombres"), size = 1) +
  geom_line(aes(y = egresos3M/1000000, color = "Mujeres"), size = 1) +
  scale_color_manual(values = c("Hombres" = "#DD6E42", "Mujeres" = "#AEC5EB")) +
  geom_vline(xintercept = 30+2023, linetype="dashed", 
             color = "lightgrey", size=1) +
  geom_vline(xintercept = 15+2023, linetype="dashed", 
             color = "lightgrey", size=1) +
  scale_color_manual(values = c("Hombres" = "#DD6E42", "Mujeres" = "#AEC5EB")) +
  labs(x = "Año",
       y = "Monto en decenas \n de millones de colones") +
  theme_minimal() +
  theme_minimal_hgrid(font_size = 23) +
  theme(legend.position = "top", legend.justification = "center",
        axis.title.y = element_text(angle=90, size=rel(1), vjust = 1 ), 
        text = element_text(size = 15, family = "serif"), axis.text = element_text(size = 15)) +
  guides(color = guide_legend(title = "Género"))

#ggsave(filename = "Det_EgresosEstado3.pdf",width = w,height = h)

# Estado 4

datos <- data.frame(anios, egresos4, egresos4M)

ggplot(datos, aes(x = anios)) +
  geom_line(aes(y = egresos4/1000000, color = "Hombres"), size = 1) +
  geom_line(aes(y = egresos4M/1000000, color = "Mujeres"), size = 1) +
  scale_color_manual(values = c("Hombres" = "#DD6E42", "Mujeres" = "#AEC5EB")) +
  geom_vline(xintercept = 30+2023, linetype="dashed", 
             color = "lightgrey", size=1) +
  geom_vline(xintercept = 15+2023, linetype="dashed", 
             color = "lightgrey", size=1) +
  scale_color_manual(values = c("Hombres" = "#DD6E42", "Mujeres" = "#AEC5EB")) +
  labs(x = "Año",
       y = "Monto en decenas \n de millones de colones") +
  theme_minimal() +
  theme_minimal_hgrid(font_size = 23) +
  theme(legend.position = "top", legend.justification = "center",
        axis.title.y = element_text(angle=90, size=rel(1), vjust = 1 ), 
        text = element_text(size = 15, family = "serif"), axis.text = element_text(size = 15)) +
  guides(color = guide_legend(title = "Género"))

#ggsave(filename = "Det_EgresosEstado4.pdf",width = w,height = h)

# Estado 5

datos <- data.frame(anios, egresos5, egresos5M)

ggplot(datos, aes(x = anios)) +
  geom_line(aes(y = egresos5/1000000, color = "Hombres"), size = 1) +
  geom_line(aes(y = egresos5M/1000000, color = "Mujeres"), size = 1) +
  scale_color_manual(values = c("Hombres" = "#DD6E42", "Mujeres" = "#AEC5EB")) +
  geom_vline(xintercept = 30+2023, linetype="dashed", 
             color = "lightgrey", size=1) +
  geom_vline(xintercept = 15+2023, linetype="dashed", 
             color = "lightgrey", size=1) +
  scale_color_manual(values = c("Hombres" = "#DD6E42", "Mujeres" = "#AEC5EB")) +
  labs(x = "Año",
       y = "Monto en decenas \n de millones de colones") +
  theme_minimal() +
  theme_minimal_hgrid(font_size = 23) +
  theme(legend.position = "top", legend.justification = "center",
        axis.title.y = element_text(angle=90, size=rel(1), vjust = 1 ), 
        text = element_text(size = 15, family = "serif"), axis.text = element_text(size = 15)) +
  guides(color = guide_legend(title = "Género"))

#ggsave(filename = "Det_EgresosEstado5.pdf",width = w,height = h)

# Estado 6

datos <- data.frame(anios, egresos6, egresos6M)

ggplot(datos, aes(x = anios)) +
  geom_line(aes(y = egresos6/1000000, color = "Hombres"), linewidth = 1) +
  geom_line(aes(y = egresos6M/1000000, color = "Mujeres"), linewidth = 1) +
  scale_color_manual(values = c("Hombres" = "#DD6E42", "Mujeres" = "#AEC5EB")) +
  geom_vline(xintercept = 30+2023, linetype="dashed", 
             color = "lightgrey", size=1) +
  geom_vline(xintercept = 15+2023, linetype="dashed", 
             color = "lightgrey", size=1) +
  geom_vline(xintercept = 40+2023, linetype="dashed", 
             color = "lightgrey", size=1) +
  scale_color_manual(values = c("Hombres" = "#DD6E42", "Mujeres" = "#AEC5EB")) +
  labs(x = "Año",
       y = "Monto en decenas \n de millones de colones") +
  theme_minimal() +
  theme_minimal_hgrid(font_size = 23) +
  theme(legend.position = "top", legend.justification = "center",
        axis.title.y = element_text(angle=90, size=rel(1), vjust = 1 ), 
        text = element_text(size = 15, family = "serif"), axis.text = element_text(size = 15)) +
  guides(color = guide_legend(title = "Género"))

#ggsave(filename = "Det_EgresosEstado6.pdf",width = w,height = h)
