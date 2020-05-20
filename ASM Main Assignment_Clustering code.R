library(tidyverse)
library(mclust)

df = read_csv("winemag-data-130k-v2.csv")

df1 <- df[ which(df$country== "US"),]

df1 <- na.omit((df1))

df2 <- subset(df1, select = c(price,points))

plot(df1$points,df1$price)
boxplot(df2)$out
boxplot(df2$price)

df2 <- subset(df2, price < 250)  #outliers removal
head(df2)

plot(df2)

fit <- Mclust(df2)

print(fit)
summary(fit)

fit$parameters$pro
fit$parameters$mean

plot(fit, what = "classification")
plot(fit, what = "uncertainty")
table(fit$classification,fit$uncertainty)
plot(fit, what = "BIC")
plot(fit, what = "density")

fit$BIC
#####################################
Bfit1 <- Mclust(df2, G= 9, modelNames= "VVE")


plot(Bfit1, what = "classification")
plot(Bfit1,what = "uncertainty")
plot(Bfit1, what = "BIC")
plot(Bfit1, what = "density")

table(Bfit1$classification,Bfit1$uncertainty)
summary(Bfit1)

########################################
# Bfit2 <- Mclust(df2, G= 16:20)
# 
# 
# plot(Bfit2, what = "classification")
# plot(Bfit2,what = "uncertainty")
# 
# 
# table(Bfit2$classification,Bfit1$uncertainty)
# summary(Bfit2)
