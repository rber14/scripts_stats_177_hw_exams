library(car)
library(dplyr)
library(readxl)
library(MASS)
library(ggpubr)
library(multcompView)


edu = read_excel('/Users/robertocampos/Desktop/education.xlsx')
str(edu)

edu$City = as.factor(edu$City)
edu$Education_Level = as.factor(edu$Education_Level)

plt = ggline(edu, x = 'City', y = 'Size', color = 'City', add = c('mean_se', 'dotplot'))
plt

plt2 = ggline(edu, x = 'Education_Level', y = 'Size', color = 'Education_Level', add = c('mean_se', 'dotplot'))
plt2

#a Clase size is the response variable

#b The factors in this problems are Education Level and Location

#c Education Level -  Elementary, Intermediate, High School
#  Location -  City A, City B, City C

#d 

mdl = aov(Size ~ Education_Level + City , data = edu)
summary(mdl)
#
#interaction.plot(edu$Education_Level, edu$City, edu$Size)


#2
health = read_excel('/Users/robertocampos/Downloads/Health.xlsx')
health = health[-c(4)]
str(health)

health$body_fat = as.factor(health$body_fat)
health$smoking = as.factor(health$smoking)
health
#renaming col
names(health)[2] = 'body_fat'


plt_h = ggline(health, x = 'body_fat', y = 'minutes', color = 'body_fat', add = c('mean_se', 'dotplot'))
plt_h

plt_h2 = ggline(health, x = 'smoking', y = 'minutes', color = 'smoking', add = c('mean_se', 'dotplot'))
plt_h2

interaction.plot(health$body_fat, health$smoking, health$minutes)
interaction.plot(health$smoking, health$body_fat, health$minutes)
mdl2 = aov(minutes ~ body_fat + smoking +body_fat:smoking,data = health)
summary(mdl2)
library(interplot)
interplot(m = mdl2, var1 = 'body_fat', var2 = 'smoking')

#l_mdl = lm(minutes ~ body_fat + smoking,data = health)
#summary(l_mdl)

par(mfrow = c(2,2))
plot(mdl2)

p = TukeyHSD(mdl2,conf.level=0.95, groups = TRUE)
p
plot(p)


