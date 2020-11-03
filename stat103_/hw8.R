library(car)
library(dplyr)
library(readxl)
library(MASS)
library(ggpubr)
library(lsmeans)
library(multcompView)
coli = read_excel("/Users/robertocampos/Desktop/coliform_samples.xlsx")
View(coli)

str(coli)
coli$Beach = as.factor(coli$Beach)
coli$Location = as.factor(coli$Location)
str(coli)

plot = ggline(coli, x = 'Location', y = 'Counts', color = 'Location', add = c('mean_se', 'dotplot'))
plot

plot2 = ggline(coli, x = 'Beach', y = 'Counts', color = 'Beach', add = c('mean_se', 'dotplot'))
plot2
model1 = lm(Counts ~ Beach * Location, data = coli)
Anova(model1, type = 'III')

#check assumtions
#normal dist of model resid
#there is deviation, may not be normally dist
plot(model1, 2)
plot(model1, 1)

#testing normality of residuals
aov_resid = residuals(object = model1)
shapiro.test(aov_resid)

posthoc = lsmeans(model1,
                  pairwise ~ Beach * Location,
                  adjust = 'tukey'
                  )

posthoc

# we only want to look at significant
CLD(posthoc,
    alpha = .05,
    Letters = letters)

anova1 = aov(Counts ~ Beach + Location, data = coli)
summary(anova1)
tukey1 = TukeyHSD(anova1)
tukey1

p = lsmeans(anova1, pairwise ~ Beach * Location, adjust = 'tukey')
p

CLD(p, alpha = .05, Letters = letters)

#2

vcr = read_excel("/Users/robertocampos/Desktop/VCR_repair.xlsx")
View(vcr)
str(vcr)
#turning into factors
vcr$Centers = as.factor(vcr$Centers)
vcr$Brands = as.factor(vcr$Brands)
str(vcr)
#aov with interaction on center and brands
aov2 = aov(Repair_time ~  Brands + Centers +Centers:Brands, data = vcr)
summary(aov2)

#info plots
plot3 = ggline(vcr, x = 'Brands', y = 'Repair_time', color = 'Brands', add = c('mean_se', 'dotplot'))
plot3

plot3 = ggline(vcr, x = 'Centers', y = 'Repair_time', color = 'Centers', add = c('mean_se', 'dotplot'))
plot3

#tukey  test 
TukeyHSD(aov2)

#tukey but filtering at .05 alpha significance level 
p2 = lsmeans(aov2, pairwise ~ Brands*Centers, adjust = 'tukey')
#adding letters for interaction
#cld() = Set up a compact letter display of all pair-wise comparisons
CLD(p2, alpha = .05, Letters = letters)
