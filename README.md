# 📈 Seguro LTC (Proyecto CA0309) 
En este repositorio se encuentran las bases de datos necesarias para la formulación de un seguro de cuidados a largo plazo (LTC). 
Además contiene el código para el cálculo de las funciones actuariales, el modelo determinístico y el modelo estocástico. 
Así como los resultados financieros. 
El código se encuentra segmentado en tres partes:

# 📂 Cálculo actuarial: Esta carpeta contiene los siguientes elementos

  📊 BaseDatosProyecto.xlsx : este es el archivo que contiene los datos sobre probabilidades de transición, población y tasas.
  
  📂 Figuras: Contiene todos los gráficos generados referentes al cálculo de primas
  
  💾 Proyecto.Rmd : Es el código con las funciones actuariales necesarias
  
# 📂 Modelo_deterministico: Esta carpeta contiene los siguientes elementos

  📂 Figuras: Contiene todos los gráficos generados referentes a la proyección demográfica y financiera deterministica 
  
  💾 ModeloDeterministico.R: Es el código que genera la proyección demográfica y financiera para ambos sexos.
  
# 📂 Modelo estocástico: Esta carpeta contiene los siguientes elementos

  📂 Figuras: Contiene todos los gráficos generados referentes a la proyección demográfica y financiera estocástica
  
  💾 Proyecciones_hombres.R: Es el código que genera la proyección demográfica y financiera estocástica para hombres.
  
  💾 Proyecciones_mujeres.R: Es el código que genera la proyección demográfica y financiera estocástica para mujeres.

## 💻 Instrucciones para correr el código 
Debe descargar el código y correrlo en el siguiente orden:
1. Proyecto.Rmd
2. ModeloDeterministico.R
3. Proyecciones_hombres.R
4. Proyecciones_mujeres.R

⚠️ Recuerde que debe cambiar las rutas para importar la base de datos en el código Proyecto.Rmd
