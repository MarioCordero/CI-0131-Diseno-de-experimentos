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

res.aov <- aov(Duracion ~ Herramienta + Experiencia, data = IDE)
summary (res.aov)
res.aov_inter <- aov(Duracion ~ Herramienta * Experiencia, data = IDE)
summary (res.aov_inter)

res.aov_inter2 <- aov(Duracion ~ Herramienta + Experiencia + Herramienta:Experiencia, data = IDE)

hist(res.aov_inter$residuals)
qqnorm(res.aov_inter$residuals)
qqline(res.aov_inter$residuals)

shapiro.test(res.aov_inter$residuals)

plot(res.aov_inter$residuals)

plot(res.aov_inter, 1)

inter <- interaction(IDE$Experiencia, IDE$Herramienta)
bartlett.test(Duracion ~ inter, data = IDE)

summary(res.aov)

summary(res.aov_inter)

TukeyHSD(res.aov_inter , which = "Experiencia")

par(mar = c(2, 6, 2, 2))
plot(TukeyHSD(res.aov, conf.level=.95, which = "Experiencia"), las = 1)

TukeyHSD(res.aov_inter, which = "Herramienta")

TukeyHSD(res.aov_inter)

library(lsr)
etaSquared(res.aov_inter, anova = TRUE)

library(effectsize)
effectsize::eta_squared(res.aov_inter)

#############################
library(pwr2)
efectoHerr = 0.15 # se toma del eta.squared.part que usted obtuvo. 0.15 no es el real
efectoExp = 0.35 # se toma del eta.squared.part que usted obtuvo. 0.35 no es el real
fA = sqrt(efectoHerr / (1 - efectoHerr))
fB = sqrt(efectoExp / (1 - efectoExp))
print(fA)
print(fB)

effectsize::cohens_f(res.aov_inter)

pwr.2way(a=2, b=3, alpha=0.05, size.A=30, size.B=20, f.A=fA, f.B=fB)

############################
potencia_buscada <- 0.80
resultados <- data.frame()
for(nA in 5:120){
  for(nB in 5:100){
    p <- pwr.2way(a = 2, b = 3, alpha = 0.05,
                  size.A = nA,size.B = nB, f.A = fA, f.B = fB )
    resultados <- rbind(
      resultados,
      data.frame( nA = nA, nB = nB, power = p$power)
    )
  }
}
subset(resultados, power >= potencia_buscada)[1, ]