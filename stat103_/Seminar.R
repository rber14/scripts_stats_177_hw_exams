smn<- read_excel("Desktop/Main_Folder/CSUS_classes/stat103/Seminar.xlsx")
View(smn)

#normal lm
model = lm(formula = smn$Enrollment~smn$`Lead Time`, data = smn)
anova_one_way <- aov(smn$`Lead Time`~smn$Enrollment, data = smn)
plot(smn$`Lead Time`,smn$Enrollment)
abline(model, col= 'red')
summary(model)
summary(anova_one_way)

#finding the coefs of the regression line
coeff = coefficients(model)
coeff

#polynominal regression - higher degree
model2 = lm(formula = smn$Enrollment~poly(smn$`Lead Time`, 2, raw = TRUE), data = smn)
abline(model2, col = 'green')
model3 = lm(formula = smn$Enrollment~smn$`Lead Time` + I(smn$`Lead Time`^2) + I(smn$`Lead Time`^3) + I(smn$`Lead Time`^4), data = smn)
abline(model3, col = 'yellow')

summary(model)

leadtimevalue = seq(0, 30, .1)
predcount = predict(model2, list(smn$`Lead Time` = leadtimevalue, smn$`Lead Time`^2 = leadtimevalue^2) )
