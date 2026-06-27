setwd("~/Desktop/UCR/CI-0131-Experimentos/Tarea.Detalle de interacciones en diseños factoriales")

# Cargar el archivo IDE2025.csv
IDE= (read.csv(file.choose(), header=T, encoding = "UTF-8"))
attach(IDE)

# Renombrar categorías de desarrolladores
IDE$Experiencia <- factor (IDE$Experiencia,
                           levels = c(0.5, 1, 2),
                           labels = c("Nov", "Int", "Ava"))

# Ajustar ANOVA con interacción
res.aov_inter <- aov(Duracion ~ Herramienta * Experiencia, data = IDE)
summary(res.aov_inter)

#Para determinar qué niveles de Experiencia son los que presentan diferencias se realiza prueba de múltiples comparaciones (Tukey)
TukeyHSD(res.aov_inter, which = "Experiencia")

# Ahora bien, ¿qué se puede hacer para determinar dónde se da la interacción? ¿Entre cuáles niveles? Para darnos una idea creamos los gráficos de interacciones:
library ( tidyverse )

# Herramienta en el eje x
IDE %>%
  ggplot() +
  aes(x = Herramienta, color = Experiencia, group = Experiencia, y = Duracion) +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line")

# Experiencia en el eje x
IDE %>%
  ggplot() +
  aes(x = Experiencia, color = Herramienta, group = Herramienta, y = Duracion) +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line")

# Sin embargo, los gráficos sólo sirven para un análisis preliminar, pero no determinan dónde está la interacción, como sí lo haría un estadístico. Para eso vamos a realizar unos contrastes. Se carga la biblioteca \texttt{emmeans} y se calculan las medias marginales estimadas a partir del modelo ANOVA.
install.packages(“emmeans”)
library(emmeans)
means_aov <- emmeans(res.aov_inter, ~ Herramienta * Experiencia)
print(means_aov)