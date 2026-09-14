library(tidyverse)
library(data.table)
library(ggcorrplot)
library(GGally)
library(knitr)
library(kableExtra)
library(ROCR)

library(jtools)
library(ggstance)
library(broom)
library(interactions)
library(GLMsData)

library(readxl)
data <- read_excel('MArket Discipline Data untuk Regresi.xlsx')
str(data)

glimpse(data)

data <- data %>% mutate(PG = round(PG))

data %>% ggplot(
  aes(PG)
) +
  geom_histogram(position = "dodge", bins = 6, fill = "Tomato") +
  theme_bw()

c(Mean = mean(data$PG), Variance = var(data$PG))

PG_model <- glm(PG~PCR+TG, family = poisson(link = "log"), data = data)

summary(PG_model)

anova(PG_model, test = "Chisq")

PG_model_2 <- glm(PG~PCR+TG, family = quasipoisson(link = "log"), data = data)
summary(PG_model_2)

library(arm)
coef1 <- coef(PG_model)
coef2 <- coef(PG_model_2)
se_coef1 <- se.coef(PG_model)
se_coef2 <- se.coef(PG_model_2)
exponent <- exp(coef1)
both <- cbind(coef1, se_coef1, coef2, se_coef2, exponent)
both

newdata <- data.frame(TG = c(0.5, 0.7), PCR = c(1.5,1.7))
predict(PG_model_2, newdata, type = "response")

plot_summs(PG_model, scale = T, exp = T)

plot_summs(PG_model, PG_model_2, scale = T, exp = T)


#-----------------------------------------------------------#
# Hanya 1 Variabel

PG_TG_model <- glm(PG~TG, family = poisson(link = "log"), data = data)

summary(PG_TG_model)

anova(PG_TG_model, test = "Chisq")

PG_TG_model_2 <- glm(PG~TG, family = quasipoisson(link = "log"), data = data)
summary(PG_TG_model_2)

library(arm)
coef1 <- coef(PG_TG_model)
coef2 <- coef(PG_TG_model_2)
se_coef1 <- se.coef(PG_TG_model)
se_coef2 <- se.coef(PG_TG_model_2)
exponent <- exp(coef1)
both <- cbind(coef1, se_coef1, coef2, se_coef2, exponent)
both

newdata <- data.frame(TG = c(0.5, 0.7))
predict(PG_TG_model_2, newdata, type = "response")

plot_summs(PG_TG_model, scale = T, exp = T)

plot_summs(PG_TG_model, PG_TG_model_2, scale = T, exp = T)

#-----------------------------------------------------------#
# Menggunakan gaussian

PG_TG_model_gaussian <- glm(PG~TG, family = gaussian(link = "identity"), data = data)

summary(PG_TG_model_gaussian)

anova(PG_TG_model_gaussian, test = "Chisq")

library(arm)
coef1 <- coef(PG_TG_model_gaussian)
se_coef1 <- se.coef(PG_TG_model_gaussian)
exponent <- exp(coef1)
both <- cbind(coef1, se_coef1, exponent)
both

newdata <- data.frame(TG = c(0.5, 0.7))
predict(PG_TG_model_gaussian, newdata, type = "response")

plot_summs(PG_TG_model_gaussian, scale = T, exp = T)
