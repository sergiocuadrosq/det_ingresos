library(readr) 
library(dplyr)  
library(ggplot2)  
library(readxl)
library(car)

datos <- read_csv("./data/interm/lambayeque_sin_outliers.csv")

#Esta base de datos se ha procesado en Python. Se han corregido outliers, missing values y se ha
#restringido el departamento de estudio a Lambayeque. En total, tenemos 2411 observaciones.


################# Primer modelo ##########################
datos$ln_ing_total = log(datos$ing_total)
primer_modelo = lm(ln_ing_total ~ educ, data=datos)
summary(primer_modelo)

#Verificando homocedasticidad:
plot(primer_modelo, which = 1)
# La varianza de los errores se mueve alrededor de cero, con una tendencia a este (linea roja), siendo un
# indicador de homocedasticidad.

#Verificando normalidad de los errores
hist(residuals(primer_modelo), main = "Histograma de Residuos", xlab = "Residuos")
# El gráfico muestra la campana de Gauss propia de la dist. normal, aunque a la derecha el cambio es brusco.
shapiro.test(residuals(primer_modelo))
# p-value para el test de normalidad de los errores es cercano a 0, verificando la normalidad 
# de los errores.

#Verificando multicolinealidad
vif(primer_modelo)
# VIF no existe porque solo es un regresor.

#Ver coeficientes
summary(primer_modelo)$coefficients

# Interpretación 
# Un nivel más de educación (del 1 al 5, como variable categorica) aumenta
# los ingresos totales en un 12.37%

################# Segundo modelo ##########################
datos$ln_ing_total = log(datos$ing_total)
datos$age_2 = datos$age^2

segundo_modelo = lm(ln_ing_total ~ educ + age_2 + age, data=datos)
summary(segundo_modelo)

#Verificando heterocedastidad:
plot(segundo_modelo, which = 1)
# La varianza de los errores se mueve alrededor de cero, con una tendencia a este (linea roja),
# aunque a la derecha los valores se disparan hacia 1

#Verificando normalidad de los errores
hist(residuals(segundo_modelo), main = "Histograma de Residuos", xlab = "Residuos")
shapiro.test(residuals(segundo_modelo))
#El p-value 2.2e-16 indica una normalidad fuerte.

#Verificando multicolinealidad
vif(segundo_modelo)
# El VIF de educ es cercano a 1, por lo que no hay problema. El VIF de age_2 y age es alto, pero es porque se
# construyeron uno a partir del otro. 

#Ver coeficientes
summary(segundo_modelo)$coefficients

# Interpretación 
# Un nivel más de educación (del 1 al 5, como variable categorica) aumenta
# los ingresos totales en un 10.89%, así como un año más de edad aumenta los
# ingresos totales en un 3.2%.


################# Tercer modelo ##########################

tercer_modelo = lm(ln_ing_total ~ educ + male ,data=datos)
summary(tercer_modelo)


#Verificando heterocedastidad:
plot(tercer_modelo, which = 1)
# Muy cercanos a cero, y casi uniformes.

#Verificando normalidad de los errores
hist(residuals(tercer_modelo), main = "Histograma de Residuos", xlab = "Residuos")
shapiro.test(residuals(tercer_modelo))
# p-value cercano a cero, confirmando una normalidad en los residuos.

#Verificando multicolinealidad
vif(tercer_modelo)
# No se puede encontrar un VIF porque solo tenemos un regresor.

#Ver coeficientes
summary(tercer_modelo)$coefficients

# Interpretación 
# Un nivel más de educación (del 1 al 5, como variable categorica) aumenta
# los ingresos totales en un 14.64%, así como ser hombre (controlando por educación)
# aumenta los ingresos totales en un 42% en comparación con las mujeres.


#CREACION MATRIZ RESUMEN
# Crear un dataframe para la matriz resumen
matriz_resumen <- data.frame(
  Modelo = c("Modelo 1", "Modelo 2", "Modelo 3"),
  Coeficientes = "",
  p_value = "",
  F_statistic = "",
  F_p_value = ""
)

# Resultados del primer modelo
matriz_resumen[1, "Coeficientes"] <- paste(
  paste(round(coef(primer_modelo)[1], 4), "(Intercepto)"), 
  paste(round(coef(primer_modelo)[2], 4), "(Educación)"), 
  sep = ", "
)
matriz_resumen[1, "p_value"] <- summary(primer_modelo)$coefficients[,"Pr(>|t|)"][2]
matriz_resumen[1, "F_statistic"] <- summary(primer_modelo)$fstatistic[1]
matriz_resumen[1, "F_p_value"] <- summary(primer_modelo)$fstatistic[3]

# Resultados del segundo modelo
matriz_resumen[2, "Coeficientes"] <- paste(
  paste(round(coef(segundo_modelo)[1], 4), "(Intercepto)"), 
  paste(round(coef(segundo_modelo)[2], 4), "(Educación)"), 
  paste(round(coef(segundo_modelo)[3], 4), "(Edad^2)"), 
  paste(round(coef(segundo_modelo)[4], 4), "(Edad)"), 
  sep = ", "
)
matriz_resumen[2, "p_value"] <- summary(segundo_modelo)$coefficients[,"Pr(>|t|)"][2]
matriz_resumen[2, "F_statistic"] <- summary(segundo_modelo)$fstatistic[1]
matriz_resumen[2, "F_p_value"] <- summary(segundo_modelo)$fstatistic[3]

# Resultados del tercer modelo
matriz_resumen[3, "Coeficientes"] <- paste(
  paste(round(coef(tercer_modelo)[1], 4), "(Intercepto)"), 
  paste(round(coef(tercer_modelo)[2], 4), "(Masculino)"), 
  sep = ", "
)
matriz_resumen[3, "p_value"] <- summary(tercer_modelo)$coefficients[,"Pr(>|t|)"][2]
matriz_resumen[3, "F_statistic"] <- summary(tercer_modelo)$fstatistic[1]
matriz_resumen[3, "F_p_value"] <- summary(tercer_modelo)$fstatistic[3]

# Mostrar la matriz resumen
print("//Modelos que explican el ingreso total//")
print("/Recordar: Se está usando al logaritmo del ingreso total en las regresiones/")
print("----MATRIZ RESUMEN:----")
print(matriz_resumen)
