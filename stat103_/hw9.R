library(car)
library(dplyr)
library(readxl)
library(MASS)
library(ggpubr)
library(multcompView)


btl = read_excel('/Users/robertocampos/Downloads/Bottle Return.xlsx')
str(btl)
View(btl)

mylog = glm( btl$Sold ~ btl$Redeemed + btl$Deposit, data =  btl)
mylog2 = glm(Redeemed ~ 1, data = btl)
summary(mylog)
944/125743
confint(mylog)
t(residuals(mylog))%*%residuals(mylog)


# b0 = 35.77
# b1 = 13.92

#b
# the deposit variable seems to not be significant at the alpha level .05 

#c
# 0.007506257 or .7 percent

#d RSS

#e e^13.92

#f 244.69



temp_m = lm(Redeemed ~ Deposit, data =  btl)
summary(temp_m)

par(mfrow = c(2,2))
plot(mylog)



model2 = lm(btl$Deposit~poly( btl$Redeemed,3,raw=TRUE))
plot(normalize(btl$Redeemed), btl$Deposit)
lines(spline(btl$Redeemed, fitted(model2)), col="red")
summary(model2)

cc = predict(mylog, data.frame(c(15)), interval = 'condifence', level = .95)
cc                


car = read_excel('/Users/robertocampos/Downloads/Car Purchase.xlsx')
View(car)




#a 
#b0 = -4.7
#b1 = .06773
#b2 = .5986
# E(y) = .598^Age + .0677^Income - 4.739 

#b
# only income is significant at a p value of .015

#c
#e^b1 = e^.0677 means 1.06 more likely to buy a car
#e^b2 = e^.598 means 1.8 more likely to buy a car

#d
#2.5
i = car$Income
a = car$Age

mdl_lg = glm(car$Purchase ~ i + a , binomial)
summary(mdl_lg)
#d 
pred <- predict(mdl_lg, data.frame(i = 55, a = 6))
pred

pred_c <- predict.glm(mdl_lg, data.frame(i = 55, a = 6), interval = 'confidence', level = .95)
pred_c
log(.598*6 + .0677*55 - 4.739) / 1 + log(.598^6 + .0677^55 - 4.739)

