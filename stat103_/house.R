library(readxl)

HP<- read_excel("Downloads/House Price.xlsx")
View(HP)   
colnames(HP)[3] = 'H_size'

#liner regression model and summary
model = lm(formula = HP$Price~HP$H_size)
anova_one_way <- aov(HP$Price~HP$H_size, data = HP)
summary(model)
summary(anova_one_way)

#finding the coefs of the regression line
coeff = coefficients(model)
coeff

# saving the regression line information
eq = paste0("y = ", round(coeff[2],1), "*x + ", round(coeff[1],1))

#plotting the data with regression line
plot(HP$H_size, HP$Price, main = eq, xlab = ' x', ylab = 'y', pch = 19) 
abline(lm(HP$Price~HP$H_size), col = 'red')
abline(v = mean(HP$H_size), col = 'blue')
abline(h = mean(HP$Price), col = 'yellow')

#box plot for outliers
boxplot(HP$H_size, main="Size", sub=paste("Outlier rows: ", boxplot.stats(HP$H_size)$out))

#density plot - distribution of the predictor variable
plot(density(HP$H_size), main="Density Plot: Size", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(HP$H_size), 2)))
polygon(density(HP$H_size), col="red")

#correlation 
cor(HP$Price,HP$H_size)


# compare diff models with this function and find the lowest
# number. AIC and BIC measures goodness of fit
AIC(model)
BIC(model)

#model of the residuals with horizontal line at 0
model.res = resid(model)
plot(HP$H_size, model.res)
abline(0,0)

View(model.res)

#min max residuals
max(model.res)
min(model.res)

model.matrix(model)





