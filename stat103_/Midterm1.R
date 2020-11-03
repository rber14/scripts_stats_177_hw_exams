library(rcompanion)
library(readxl)
bs <- read_excel("/Users/robertocampos/Downloads/Baseball.xlsx")
sc <- read_excel("/Users/robertocampos/Downloads/SolarCookers.xlsx")
View(bs)
View(sc)

model <- lm(formula = Wins~E.R.A., data =bs)
summary(model)

eq = paste0("y = ", round(coeff[2],1), "*x + ", round(coeff[1],1))
plot(bs$E.R.A., bs$Wins, main = eq)
abline(model)

coeff = coefficients(model)
coeff


summary(model)

84.85 - qnorm(.95)

t.test(bs$Wins, bs$E.R.A., conf.level = .95)

E.R.A. <-  4
test_data <- data.frame(E.R.A.)
#View(E.R.A)

aa <- predict(model, newdata = test_data, interval = "confidence", level = .95)
aa


bb <-  predict(model, newdata = test_data, interval = "confidence", level = .9)
bb

cc <- predict(model, newdata = test_data, interval = "prediction", level = .95)
cc

yankee <- predict(model,newdata = test_data)
yankee

yankeeRes = rstandard(model)
View(yankeeRes)

model1 = lm(sc$`Exergy Output`~sc$`Temperature Difference`)
plot(sc$`Temperature Difference`, sc$`Exergy Output`)
abline(model1)
summary(model1)


model22 = lm(sc$`Exergy Output`~poly(sc$`Temperature Difference`,2,raw=TRUE))

coeff22 = coefficients(model22)
coeff22

eq22 = paste0("y = ",  round(coeff22[2],1), "*x + ", round(coeff2[1],1))

plot(sc$`Temperature Difference`, sc$`Exergy Output`, main = eq22)
lines(spline(sc$`Temperature Difference`, fitted(model22)), col="red")
summary(model22)



model2 = lm(sc$`Exergy Output`~poly(sc$`Temperature Difference`,3,raw=TRUE))

coeff2 = coefficients(model2)
coeff2

eq2 = paste0("y = ", round(coeff2[3],1), "*x + ", round(coeff2[2],1), "*x + ", round(coeff2[1],1))
plot(sc$`Temperature Difference`, sc$`Exergy Output`, main = eq2)
lines(spline(sc$`Temperature Difference`, fitted(model2)), col="red")
summary(model2)


model3 = lm(sc$`Exergy Output`~poly(sc$`Temperature Difference`,4,raw=TRUE))

coeff3 = coefficients(model3)
coeff3

eq4 = paste0("y = ", round(coeff3[4],1), "*x + ", round(coeff3[3],1), "*x + ", round(coeff3[2],1), "*x + ", round(coeff3[1], 1))

plot(sc$`Temperature Difference`, sc$`Exergy Output`, main = eq4)
lines(spline(sc$`Temperature Difference`, fitted(model3)), col="red")
summary(model3)

model4 = lm(sc$`Exergy Output`~poly(sc$`Temperature Difference`,5,raw=TRUE))
plot(sc$`Temperature Difference`, sc$`Exergy Output`)
lines(spline(sc$`Temperature Difference`, fitted(model4)), col="red")
summary(model4)

# The Studentized residuals. Like standardized residuals, 
# these are normalized to unit variance, but the Studentized version 
# is fitted ignoring the current data point

studentized = studres(model2)
plot(sc$`Temperature Difference`, studentized, main = eq2)
lines(spline(sc$`Temperature Difference`, fitted(model2)), col="red")

cold <-  30
test_data1 <- data.frame(cold)
View(test_data1)

cc <- predict(model3, newdata = test_data1, interval = "confidence", level = .95)
cc
xxx = -.3 * (30)^2 + 10.6 * (30)+ -11.7
xxx

