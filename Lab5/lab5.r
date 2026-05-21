library(ggplot2)
library(dplyr)
library(car)
library(lsr)
library(effectsize)

IDE= (read.csv(file.choose(), header=T, encoding = "UTF-8"))
attach(IDE)

IDE$Experiencia <- factor (IDE$Experiencia,
                           levels = c(0.5, 1, 2),
                           labels = c("Nov", " Int", " Ava"))
IDE$Herramienta <- as.factor(IDE$Herramienta)

str(IDE)

table(IDE$Herramienta, IDE$Experiencia)

#--------------------Usando tidyverse
#Calcule la media, la varianza y la desviación estándar por grupos
group_by(IDE, Experiencia) %>%
  summarise(
    count = n(),
    mean = mean(Duracion, na.rm = TRUE),
    var = var(Duracion, na.rm = TRUE),
    sd = sd(Duracion, na.rm = TRUE)
  )

#-----------------------Usando data.table

# Convertir IDE a data.table
library(data.table)
setDT(IDE)

# Descriptivas por Experiencia
IDE[, .(count = .N, 
        mean = mean(Duracion, na.rm = TRUE),
        var = var(Duracion, na.rm = TRUE),
        sd = sd(Duracion, na.rm = TRUE)), 
    by = Experiencia]

# Descriptivas por Herramienta
IDE[, .(count = .N, 
        mean = mean(Duracion, na.rm = TRUE),
        var = var(Duracion, na.rm = TRUE),
        sd = sd(Duracion, na.rm = TRUE)), 
    by = Herramienta]

# Ahora veamos el detalle agrupando tanto “Herramienta” como “Experiencia”:
IDE[, .(count = .N, 
        mean = mean(Duracion), 
        var = var(Duracion), 
        sd = sd(Duracion)), 
    by = .(Herramienta, Experiencia)]

# -----------------------------

boxplot(Duracion ~ Herramienta, data=IDE, frame = FALSE,
        col = c("#00AFBB", "#E7B800"), ylab=" Duracion")

boxplot(Duracion ~ Experiencia, data = IDE, frame = FALSE,
        col = c("#00AFBB", "#E7B800", "#FC4E07"), ylab = "Duración (horas)")

boxplot(Duracion ~ Herramienta * Experiencia, data=IDE, frame = FALSE,
        col = c("#00AFBB", "#E7B800"), ylab="Duracion")

boxplot(Duracion ~ Experiencia * Herramienta, data=IDE, frame = FALSE,
        col = c("#00AFBB", "#E7B800"), ylab="Duracion")

IDE %>%
  ggplot() +
  aes(x = Herramienta, color = Experiencia, group = Experiencia, y = Duracion) +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line")

IDE %>% 
  ggplot() + 
  aes(x = Experiencia, color = Herramienta, group = Herramienta, y = Duracion) + 
  stat_summary(fun = mean, geom = "point") + 
  stat_summary(fun = mean, geom = "line")

interaction.plot(x.factor = IDE$Experiencia, trace.factor = IDE$Herramienta,
                 response = IDE$Duracion, fun = mean,
                 type = "b", legend = TRUE,
                 xlab = "Experiencia", ylab="Duración en horas",
                 pch=c(1,19), col = c("#00AFBB", "#E7B800"))

interaction.plot(x.factor = IDE$Herramienta, trace.factor = IDE$Experiencia,
                 response = IDE$Duracion, fun = mean,
                 type = "b", legend = TRUE,
                 xlab = "Herramienta", ylab = "Duración en horas",
                 pch = c(1,19), col = c("#00AFBB", "#E7B800", "#FC4E07"))