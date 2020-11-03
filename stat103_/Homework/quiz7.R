library(car)
library(dplyr)
library(readxl)
library(dummies)
library(MASS)

lb<- read_excel("/Users/robertocampos/Desktop/Main_Folder/CSUS_classes/stat103/Lightbulbs.xlsx")
View(lb)
lb2 <- read_excel("/Users/robertocampos/Desktop/Main_Folder/CSUS_classes/stat103/Lightbulbs.xlsx")
dummy_ecoded = data.frame(dummy(lb$Brand))
lb = cbind(lb, dummy_ecoded)
lb = lb[-c(2)]
View(lb2)

#put the names into diff variables to later predict
y = lb$Hours
a = lb$Brand.Dot
b = lb$Brand.GE
#generic dropped
c = lb$Brand.generic
d = lb$Brand.West

#Regression model 
# c = Brand.generic which was dropped 
# only a = Brand.dot, b = Brand.GE, and d = Brand.West are used in the model
full_model = lm(lb$Hours ~ lb$Brand.Dot + lb$Brand.GE + lb$Brand.West)
summary(full_model)
 
#one way anova, based on the regression model? 
#null hypothesis: means of different brands are the same
#alternative hypothesis: atleast one of the means differs
aov_model = aov(y ~ lb$Brand.Dot + lb$Brand.GE + lb$Brand.West)
aov_model
summary(aov_model)

summary.aov(full_model)
#1.
#a.
# With the help of Dummy Variables,
# we can turn the categorical data into numeric variables/indicator variables

#b 
# The test for this model can be a one way analysis of variance test to check if there is
# any statistical significance. This will allow us to check if the lightbulb performance
# was different between brands
View(lb2)
ge = subset(lb2, lb2$Brand == 'GE')
dot = subset(lb2, lb2$Brand == 'Dot')
west = subset(lb2, lb2$Brand == 'West')
generic = subset(lb2, lb2$Brand == 'generic')

g.m = mean(ge$Hours)
dot.m =mean(dot$Hours)
west.m = mean(west$Hours)
gen.m = mean(generic$Hours)
g.m
dot.m
west.m
gen.m

print("Mean for GE", g.m)

print("Mean for Dot")
print("Mean for West")
print("Mean for generic")

#quiz 8 
#1
#a.   What are the hypotheses of the test of the model for the lightbulbs data?
#H0: the mean is the same for all brands
#HA: the mean is different for atleast two of the brands

#b 
summary(full_model)
anova(full_model)
oneway.test(lb$Hours ~ lb$Brand.Dot + lb$Brand.GE + lb$Brand.West +lb$Brand.generic, data = lb)

m = aov(lb$Hours ~ lb$Brand.Dot + lb$Brand.GE + lb$Brand.West + lb$Brand.generic, data = lb)
anova(aov(lb$Hours ~ lb$Brand.Dot + lb$Brand.GE + lb$Brand.West + lb$Brand.generic, data = lb))
summary(m)
as.factor(lb2$Brand)

