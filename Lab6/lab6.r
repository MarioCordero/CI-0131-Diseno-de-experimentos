# cargar los datos
library(ggplot2)
library(dplyr)
library(car)
library(lsr)
library(effectsize)

# 1. Cargar los datos
datos <- read.csv("falsospositivos_2026.csv")


# PARTE 2.1

# a) Boxplot por Herramienta
ggplot(datos, aes(x = Herramienta, y = FalsosPositivos, fill = Herramienta)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Falsos Positivos según la Herramienta",
       x = "Herramienta",
       y = "Cantidad de Falsos Positivos") +
  theme(legend.position = "none") # Ocultamos la leyenda porque el eje X ya lo dice

# b) Boxplot por Acoplamiento
ggplot(datos, aes(x = Acoplamiento, y = FalsosPositivos, fill = Acoplamiento)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Falsos Positivos según el Nivel de Acoplamiento",
       x = "Nivel de Acoplamiento",
       y = "Cantidad de Falsos Positivos") +
  theme(legend.position = "none")

# c) Boxplot combinado
ggplot(datos, aes(x = Herramienta, y = FalsosPositivos, fill = Acoplamiento)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Falsos Positivos por Herramienta y Acoplamiento",
       x = "Herramienta",
       y = "Cantidad de Falsos Positivos",
       fill = "Acoplamiento")

# d.1) Interacción (Eje X: Herramienta, Líneas: Acoplamiento)
with(datos, interaction.plot(
  x.factor = Herramienta, 
  trace.factor = Acoplamiento, 
  response = FalsosPositivos, 
  type = "b",             # "b" significa puntos (both) y líneas
  pch = 19,               # Círculos sólidos
  col = c("blue", "red"), # Colores para las líneas
  main = "Gráfico de Interacción: Herramienta vs Acoplamiento",
  xlab = "Herramienta", 
  ylab = "Media de Falsos Positivos",
  trace.label = "Acoplamiento"
))

# d.2) Interacción (Eje X: Acoplamiento, Líneas: Herramienta)
with(datos, interaction.plot(
  x.factor = Acoplamiento, 
  trace.factor = Herramienta, 
  response = FalsosPositivos, 
  type = "b", 
  pch = 19, 
  col = c("blue", "red", "darkgreen"), 
  main = "Gráfico de Interacción: Acoplamiento vs Herramienta",
  xlab = "Nivel de Acoplamiento", 
  ylab = "Media de Falsos Positivos",
  trace.label = "Herramienta"
))

# PARTE 2.2

# Es vital asegurar que las variables independientes sean tratadas como factores
datos$Herramienta <- as.factor(datos$Herramienta)
datos$Acoplamiento <- as.factor(datos$Acoplamiento)

# Ajuste del ANOVA sin considerar el factor de bloqueo
anova_sin_bloques <- aov(FalsosPositivos ~ Herramienta * Acoplamiento, data = datos)

# Mostrar la tabla del ANOVA
summary(anova_sin_bloques)

# PARTE 3.1

# Asegurarnos de que el bloque también sea tratado como categoría/factor
# (Revisa que la columna en tu CSV se llame exactamente 'ServidorCI' o 'Bloque')
datos$ServidorCI <- as.factor(datos$ServidorCI)

# Ajuste del ANOVA con el bloque ServidorCI de forma aditiva
anova_con_bloque <- aov(FalsosPositivos ~ Herramienta * Acoplamiento + ServidorCI, data = datos)

# Mostrar la tabla del ANOVA
summary(anova_con_bloque)

# PARTE 3.2

# 1. Prueba de Normalidad (Shapiro-Wilk)
shapiro.test(anova_con_bloque$residuals)

# 2. Prueba de Homocedasticidad (Bartlett)
bartlett.test(anova_con_bloque$residuals ~ interaction(Herramienta, Acoplamiento), data = datos)

# 3. Prueba de Independencia (Grafico solo con puntos)
plot(anova_con_bloque$residuals, type = "p", 
     main = "Residuales en orden de recoleccion", 
     ylab = "Residuales", xlab = "Orden de observacion", 
     pch = 16, col = "blue")
abline(h = 0, lty = 2, col = "red", lwd = 2)