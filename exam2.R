# Roberto Campos
#Exam 2
library(car)
library(dplyr)
library(readxl)
library(MASS)
H <- read_excel("Desktop/Main_Folder/CSUS_classes/stat103/HARRIS.xlsx")
View(H)

#1. The file HARRIS shows the values of the following variables for 93 employees of Harris Bank Chicago in 1977:
#  •Salary= Annual beginning (or starting) salary,in dollars
#  •Educate= Years of schooling at the time of hire
#  •Exper= Number of months of previous work experienceat time of hire
#  •Months= Number of months after January 1, 1969, that the individual was hired 
#  Construct an initial regression model for the starting salaries of employees of Harris Bank Chicago 
#  using only the variables above. Base all of your responses below on the initial model only!


  #a. Write down the regression equation for the initial model.
model = lm(H$Salary ~ H$Educate + H$Exper + H$Months)
model

summary(model)
# salary = 3179.74 + 139.618 * Educate + 1.481 * Exper + 20.633 * Months

  #b. Test the overall utility of the model. Explain.
aov_harris = aov(H$Salary ~ H$Educate + H$Exper + H$Months)
summary(aov_harris)
plot(aov_harris)

aov.1 = aov(H$Salary ~ H$Educate)
summary(aov.1)

?aov()


print(model.tables(aov_harris,"means"),digits=3)    

# plotting residuals
model_resid = resid(model)
plot(H$Months, model_resid)
abline(0, 0)

up = predict(model)

standRes = (up - mean(model_resid)) / sd(model_resid)
hist(model_resid, freq = FALSE)
curve(dnorm, add = TRUE)

ress = stdres(aov_harris)
hist(ress, freq = FALSE)

model1 = lm(H$Salary ~ H$Educate + H$Exper + H$Months + H$Male, data = H)
model1
x = H$Salary
a = H$Educate
b = H$Exper 
c = H$Months
d = H$Male
model.1 = lm(x ~ a+ b + c + d)

model_best = lm(x~a + I(a^2) + b + I(b^2) + c + I(c^2) + d + I(d^2))
model_best_2 = lm(x~a + I(a^2) + I(a^3) + b + I(b^2) + I(b^3) + c + I(c^2) + I(c^3) + d + I(c^2) + I(d^3))
model_best_3 = lm(x ~ poly(a,4,raw = TRUE) + poly(b, 4, raw = TRUE) + poly(c, 4, raw = TRUE) + poly(d, 4, raw =TRUE)  )
model_best_4 = lm(x ~ poly(a,5,raw = TRUE) + poly(b, 5, raw = TRUE) + poly(c, 5, raw = TRUE) + poly(d, 5, raw =TRUE)    )

remove_exper_model = lm(x ~ a + c + d)
summary(model.1)
summary(model_best)
summary(model_best_2)
summary(model_best_3)
summary(model_best_4)
summary(remove_exper_model)

AIC(model.1,remove_exper_model)
AIC(model.1, model_best)
AIC(model_best, model_best_4)

AIC(model_best_2, model_best_4)
anova(model.1, model_best)
anova(model.1, model_best_2)
anova(model.1, model_best_3)
anova(model.1, model_best_4)

?anova()

View(test_data_m)
male_pred <- predict(model.1, data.frame(a = 12, b = 120, c = 36, d = 1), interval = "predict", level = .95)
male_pred

p = predict(model.1, test_data_m)
p

library(olsrr)
ols_mallows_cp()


