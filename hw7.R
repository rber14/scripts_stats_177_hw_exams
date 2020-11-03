library(car)
library(dplyr)
library(readxl)
library(dummies)
library(MASS)

store_df = read_excel("/Users/robertocampos/Desktop/store_price.xlsx")
brd = read_excel("/Users/robertocampos/Desktop/brands.xlsx")
View(brd)

dencoded = data.frame(dummy(brd$brands))
View(dencoded)

bat = cbind(brd, dencoded)
View(bat)

bat = bat[-c(2)]
View(bat)

oneway.test(bat$lifetime ~ bat$brands.brand1 + bat$brands.brand2 + bat$brands.brand3 + bat$brands.brand4, data = bat)

m1 = subset(brd, brd$brands == 'brand1')
m2 = subset(brd, brd$brands == 'brand2')
m3 = subset(brd, brd$brands == 'brand3')
m4 = subset(brd, brd$brands == 'brand4')

m1 = mean(m1$lifetime)
m2 = mean(m2$lifetime)
m3 = mean(m3$lifetime)
m4 = mean(m4$lifetime)


m1
m2
m3
m4

#prob 2
View(store_df)
s_encode = data.frame(dummy(store_df$store))
View(s_encode)

ps = cbind(price, s_encode)
ps = ps[-c(2)]

aov_price = aov(ps$price ~ ps$store.discount + ps$store.speciality + ps$store.variety, data = ps)
summary(aov_price)
plot(aov_price)

oneway.test(ps$price ~ ps$store.discount + ps$store.speciality + ps$store.variety)

s1 = subset(store_df, store_df$store == 'discount')
s2 = subset(store_df, store_df$store == 'speciality')
s3 = subset(store_df, store_df$store == 'variety')

store1_mean = mean(s1$price)
store2_mean = mean(s2$price)
store3_mean = mean(s3$price)

store1_mean
store2_mean  
store3_mean


