library(readxl)
VACATION <- read_excel("Desktop/Main_Folder/CSUS_classes/stat103/VACATION.xlsx")
View(VACATION)  

model = lm(formula = VACATION$Price~VACATION$`Lot size`+VACATION$Trees+VACATION$Distance, data = VACATION)
summary(model)

model.res = resid(model)
plot(VACATION$`Lot size`+VACATION$Trees+VACATION$Distance, model.res)
abline(0,0)

model2 = lm(formula = VACATION$Price~VACATION$Trees+VACATION$Distance, data = VACATION)
summary(model2)

plot(VACATION$Trees+VACATION$Distance, VACATION$Price, main = eq, xlab = ' x', ylab = 'y', pch = 19) 
abline(model2, col = 'red')
abline(v = mean(VACATION$Trees+VACATION$Distance), col = 'blue')
abline(h = mean(VACATION$Price), col = 'yellow')

new_data <- data.frame(size=40, Trees=50, Distance=75)

View(new_data)
a = predict.lm(model2, newdata=new_data, interval = 'confidence', level = .95)
a

b = predict.lm(model2, new_data, interval = 'predict', level = .95)
b


amus <- read_excel("/Users/robertocampos/Desktop/Main_Folder/CSUS_classes/stat103/AMUSEMENT.xlsx")
View(amus)  

amus_model = lm(amus$Attendance ~ amus$Yesterday + amus$Weekend + amus$Sunny + amus$Rain)
summary(amus_model)


amus_model2 = lm(amus$Attendance ~ amus$Yesterday + amus$Weekend + amus$Sunny)
summary(amus_model2)

AIC(amus_model, amus_model2)

ols_mallows_cp(amus_model2, amus_model)
ols_mallows_cp(amus_model, amus_model2)

cata =  read_excel('/Users/robertocampos/Desktop/Main_Folder/CSUS_classes/stat103/CATALOG.xlsx')
View(cata)

cata_model = lm(cata$Time ~ cata$Boxes + cata$Weight, data = cata)
summary(cata_model)

VIF(cata_model)

cata_model = lm(cata$Time ~ cata$Boxes + cata$Weight + cata$Codes, data = cata)
summary(cata_model)


one <- subset(cata, Codes == '1')
two <- subset(cata, Codes == '2')
three <- subset(cata, Codes == '3')

mean(one$Time)
mean(two$Time)
mean(three$Time)

              