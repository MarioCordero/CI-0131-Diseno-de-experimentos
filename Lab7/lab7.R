setwd("~/Desktop/UCR/CI-0131-Experimentos/Lab7")
beis = (read.csv(file.choose(), header=T, encoding = "UTF-8"))
attach(beis)

# -----------------------------------------------
# --------------------------------- PRIMERA PARTE
# -----------------------------------------------

# Grafico de dispersion
plot(beis$at_bats, beis$runs)

# Coeficiente de correlacion
cor(beis$runs, beis$at_bats)

# suma de residuos cuadrados
if(!require('statsr')) {
  install.packages('statsr')
  library('statsr')
}
plot_ss(x = at_bats, y = runs, data = beis)

# Lo mismo pero visualizando residuos al cuadrado
plot_ss(x = at_bats, y = runs, data = beis, showSquares = TRUE) # min posible Sum of Squares:  123721.9

# Ajuste del modelo lineal
m1 <- lm(runs ~ at_bats, data = beis)
summary(m1)

# Ahora se creará un diagrama de dispersión agregando la línea de mínimos cuadrados.
plot(beis$runs ~ beis$at_bats)
abline(m1)

# verificación de supuestos de modelo de regresión
# Normalidad
shapiro.test(m1$residuals)
# Homocedasticidad
library(car)
ncvTest(m1)
# Independencia
plot(m1$residuals)
abline(h = 0, col = "red", lwd = 1)

# -----------------------------------------------
# --------------------------------- SEGUNDA PARTE
# -----------------------------------------------

# Ajuste del nuevo modelo lineal usando homeruns como predictor
m2 <- lm(runs ~ homeruns, data = beis)

# Mostrar el resumen estadistico del modelo
summary(m2)

# Pregunta #9
# Lista de las variables pendientes
variables <- c("hits", "bat_avg", "strikeouts", "stolen_bases", "wins")

# Bucle para extraer los datos de cada modelo
for (var in variables) {
  # Crear la fórmula y el modelo
  formula_modelo <- as.formula(paste("runs ~", var))
  modelo <- lm(formula_modelo, data = beis)
  resumen <- summary(modelo)
  
  # Cálculos
  correlacion <- cor(beis$runs, beis[[var]])
  r_cuadrado <- resumen$r.squared
  
  # Extraer el p-value del estadístico F
  f_stat <- resumen$fstatistic
  p_value <- pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)
  
  # Imprimir resultados
  cat(sprintf("--- %s ---\n", toupper(var)))
  cat(sprintf("Correlación: %.4f\n", correlacion))
  cat(sprintf("p-value:     %.4e\n", p_value))
  cat(sprintf("R^2:         %.4f\n\n", r_cuadrado))
}

# -----------------------------------------------
# --------------------------------- TERCERA PARTE
# -----------------------------------------------
# Modelo de regresión lineal con cinco de las variables originales: at_bats, hits, homeruns, bat_avg,y wins
mul <- lm(runs ~ at_bats + hits + homeruns + bat_avg + wins, data = beis)
summary(mul)

# Pregunta 14
# Cargar libreria car para el calculo de VIF
library(car)
# Calcular el VIF del modelo multiple
vif(mul)