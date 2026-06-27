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
# install.packages("emmeans")
library(emmeans)
means_aov <- emmeans(res.aov_inter, ~ Herramienta * Experiencia)
print(means_aov)

# Por ejemplo, se pueden comparar VSCode-Nov vs IntelliJ-Ava (los grupos 2 y 4 de la lista anterior):
contrast(means_aov, method = list("VSCode_Nov vs IntelliJ_Ava" = c(0, 1, 0, 0, -1, 0)))

# Sin embargo, también se pueden crear contrastes para identificar interacciones, marcando 4 de las 6 posiciones del vector de coeficientes.
contrast(means_aov,
         method = list("Interacción Novato - Intermedio" = c(1, -1, -1, 1, 0, 0 )))

# El contraste de medias entre Intermedio y Avanzado sería:
contrast(means_aov,
         method = list("Interacción Intermedio - Avanzado" = c(0, 0, 1, -1, -1, 1)))

# Finalmente, el contraste entre Novato y Avanzado se define como:
contrast(means_aov,
         method = list("Interacción Novato - Avanzado" = c(1, -1, 0, 0, -1, 1)))

# Ahora bien, dado que se están realizando 3 comparaciones, es conveniente realizar un ajuste, similar al que realiza el Tukey, para reducir la propagación del error de tipo I. Existen varios ajustes posibles.
# Para ello se pueden realizar todas las comparaciones a la vez.
# 1. Ajustar el modelo y calcular las medias marginales
model <- aov(Duracion ~ Herramienta * Experiencia, data = IDE)
means <- emmeans(model, ~ Herramienta * Experiencia)

# 2. Definir manualmente los contrastes de interacción
# (Comparar cómo cambia el efecto de Herramienta entre pares de niveles de Experiencia)
interaction_contrasts <- list(
  "Interacción Nov vs Int" = c(1, -1, -1, 1, 0, 0), # Efecto en Nov vs Int
  "Interacción Nov vs Ava" = c(1, -1, 0, 0, -1, 1), # Efecto en Nov vs Ava
  "Interacción Int vs Ava" = c(0, 0, 1, -1, -1, 1) # Efecto en Int vs Ava)
)

# 3. Calcular los contrastes y mostrar resultados
interaction_results <- contrast(means, method = interaction_contrasts,
                                adjust = "none") # En este caso inicial no se hace ajuste.
print(interaction_results)

# Puede ver que estos resultados coinciden con los contraste individuales. Ahora realicemos los contrastes pero agregando la corrección de Holm-Bonferroni.
interaction_results <- contrast(means, method = interaction_contrasts,
                                adjust = "holm")
print(interaction_results)