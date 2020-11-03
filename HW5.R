library(readxl)
X93cars <- read_excel("Downloads/93cars.xlsx")
View(X93cars)        

model = lm(X93cars$Horsepower~X93cars$`MPG Highway`, data = X93cars)
anova_one_way = aov(X93cars$Horsepower~X93cars$`MPG Highway`, data = X93cars)
coeff = coefficients(model)

summary(model)
summary(anova_one_way)


#a
#p-value is 3.744e-11

#b
# R^2 = .3832

#c
eq = paste0("y = ", round(coeff[2],1), "*x + ", round(coeff[1],1))
plot(X93cars$`MPG Highway`, X93cars$Horsepower, main = eq, xlab = ' x', ylab = 'y', pch = 19)
abline(model, col = 'red')

#2 Quadratic model
#a The be value is significant it's less then .05
#b p-value = 1.3676-10
#c R^2 is .3963 and R^2 adjusted is .3829, R squared is more appropriate since there is only on predictor value
fit2<-lm(X93cars$Horsepower~poly(X93cars$`MPG Highway`,2,raw=TRUE))
#model2 = lm(X93cars$Horsepower~X93cars$`MPG Highway`+ I(X93cars$`MPG Highway`^2), data = X93cars)
summary(fit2)
fit2$coefficient[1]
fit2$coefficient[2]
fit2$coefficient[3]

quadratic = fit2$coefficient[3]*X93cars$`MPG Highway`^2 + fit2$coefficient[2]*X93cars$`MPG Highway` + fit2$coefficient[1]
quadratic

plot(X93cars$`MPG Highway`, X93cars$Horsepower)
par(new = TRUE)
lines(spline(X93cars$`MPG Highway`, fitted(fit2)), col="red")


#3
fit3<-lm(X93cars$Horsepower~poly(X93cars$`MPG Highway`,10,raw=TRUE))
#model2 = lm(X93cars$Horsepower~X93cars$`MPG Highway`+ I(X93cars$`MPG Highway`^2), data = X93cars)
summary(fit3)

plot(X93cars$`MPG Highway`, X93cars$Horsepower)
par(new = TRUE)
lines(spline(X93cars$`MPG Highway`, fitted(fit3)), col="red")

model.fit3 = resid(fit3)
plot(X93cars$`MPG Highway`, model.fit3)
abline(0,0)

temp = data.frame(Horsepower = 150)
predict(fit3, newdata = temp, interval = 'confidence')


