library(ggplot2)

x = c(22,26,27,33,29,29,34,30,40)
y = c(497,541,556,576,578,607,662,739,805)

# cor = .82
cor(x, y) 

q1  = plot(x,y, main = 'Quiz 2', xlab = ' x', ylab = 'y', pch = 19) 
abline(lm(y~x), col = 'red')

# The estimated regression line equation is:
# y = 141.08 + 15.89x

model = lm(formula = y ~ x)
model

mean(x)
mean(y)
sum(x)
sum(y)

#When x = 31 then y = 636.46
y2 = 141.08 + 15.98 * (31)
y2

summary(model)
