setwd("~/Desktop/UCR/CI-0131-Experimentos/Lab7")
beis = (read.csv(file.choose(), header=T, encoding = "UTF-8"))
attach(beis)

# --------------------------------- PRIMERA PARTE

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