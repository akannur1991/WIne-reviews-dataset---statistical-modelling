library(sqldf)
library(readxl)
dfa <- read_excel("winemag-data-130k-v2.xlsx")
head(dfa)



dfa <- dfa[ which(dfa$price=='15'),]
dfa<-sqldf("select country, price, points,variety from dfa")
df <- dfa

df$country <- factor(df$country)
df$variety <- factor(df$variety)

df1 <- df[ which(df$country== "Chile"),]
df1 <- df1[ which(df1$variety== "Chardonnay"),]

df2 <- df[ which(df$country=='South Africa'),]
df2 <- df2[ which(df2$variety== "Sauvignon Blanc"),]

head(df1)

df_final = rbind(df1,df2)
dim(df_final)
head(df1)
dim(df1)
dim(df2)
View(df_final)
df_final <- na.omit(df_final)
#df_final <- unique(df_final)

#test of conditions for performing T test
#1. Normal distributed or not
hist(df1$points)
hist(df2$points)
hist(df_final$points)
qqnorm(df1$points)
qqnorm(df2$points)
qqnorm(df_final$points)
#qqline(df1$points)
#qqplot(df1$points,df2$points)
#2. test for normality- if p value less than 0.05 then null hypo. rejected i.e not normally distributed
shapiro.test(df1$points)
shapiro.test(df2$points)
shapiro.test(df_final$points)
# requirement of same variance(null hypo- var is same)
var.test(points ~ variety, data=df_final)
#students t test(two tailed t test) to check if means are different(null hypo- means are same)
#t-test sample:
t.test(points ~ variety, data=df_final, var.equal = TRUE)
#one tailed t test
t.test(points ~ variety, data=df_final, alternative = "less" , var.equal = TRUE)



library(ggplot2)
ggplot(df_final) + geom_boxplot(aes(country, points, fill = country)) + geom_jitter(aes(country, points, shape = df_final$country))
ggplot(df_final) + geom_boxplot(aes(variety, points, fill = variety)) + geom_jitter(aes(variety, points, shape = df_final$variety))

quantile(df1$points)# quartiles of Chardonnay
quantile(df2$points)#quartiles of sauvignon blanc
IQR(df1$points)
IQR(df2$points)                                                                            
# tapply(df_final$points, df_final$country, mean)
# tapply(df_final$points, df_final$country, median)
# tapply(df_final$points, df_final$country, sd)

tapply(df_final$points, df_final$variety, mean)
tapply(df_final$points, df_final$variety, median)
tapply(df_final$points, df_final$variety, sd)

# c = tapply(df_final$points, df_final$country, mean)
c = tapply(df_final$points, df_final$variety, mean)

#Difference in mean in observed data between South Africa and Chile
diffMean = c['Sauvignon Blanc'] - c['Chardonnay']
print('Difference in mean in observed data between South Africa and Chile:')
print(diffMean)



#Bayesian Model
compare_points_gibbs <- function(y, ind, mu0 = 50, tau0 = 1/400, del0 = 0, gamma0 = 1/400,
                            a0 = 1, b0 = 50, maxiter = 5000)
{
  print('Value of IND')
  print(ind)
  y1 <- y[ind == 'Chardonnay']
  y2 <- y[ind == 'Sauvignon Blanc']
  n1 <- length(y1)
  n2 <- length(y2)
  ##### starting values
  mu <- (mean(y1) + mean(y2)) / 2
  del <- (mean(y1) - mean(y2)) / 2
  mat_store <- matrix(0, nrow = maxiter, ncol = 3)
  #####
  ##### Gibbs sampler
  an <- a0 + (n1 + n2)/2
  for(s in 1 : maxiter)
  {
    ##update tau
    bn <- b0 + 0.5 * (sum((y1 - mu - del) ^ 2) + sum((y2 - mu + del) ^ 2))
    tau <- rgamma(1, an, bn)
    ##
    ##update mu
    taun <- tau0 + tau * (n1 + n2)
    mun <- (tau0 * mu0 + tau * (sum(y1 - del) + sum(y2 + del))) / taun
    print('MUN')
    print(mun)
    mu <- rnorm(1, mun, sqrt(1/taun))
    ##
    ##update del
    gamman <- tau0 + tau*(n1 + n2)
    deln <- ( del0 * tau0 + tau * (sum(y1 - mu) - sum(y2 - mu))) / gamman
    del<-rnorm(1, deln, sqrt(1/gamman))
    ##
    ## store parameter values
    mat_store[s, ] <- c(mu, del, tau)
    #print(mat_store[s, ])
  }
  colnames(mat_store) <- c("mu", "del", "tau")
  #print(mat_store)
  return(mat_store)
}


library(MCMCpack)

##Calling function
fit <- compare_points_gibbs(df_final$points, as.factor(df_final$variety))
graphics.on()
par("mar")
par(mar=c(1,1,1,1))
plot(as.mcmc(fit))

raftery.diag(as.mcmc(fit))

apply(fit, 2, mean)

apply(fit, 2, sd)


mean(1/sqrt(fit[, 3]))

sd(1/sqrt(fit[, 3]))

y1_sim <- rnorm(5000, fit[, 1] + fit[, 2], sd = 1/sqrt(fit[, 3]))
y2_sim <- rnorm(5000, fit[, 1] - fit[, 2], sd = 1/sqrt(fit[, 3]))

ggplot(data.frame(y_sim_diff = y1_sim - y2_sim)) + stat_bin(aes(y_sim_diff))

#How much better is the Wine:
difference  = mean(y2_sim - y1_sim)
#difference  = mean(y2_sim) - mean(y1_sim)

print('How much better is the Wine')
print(difference)
mean(y2_sim > y1_sim) ##probability

ggplot(data.frame(y1_sim, y2_sim)) + geom_point(aes(y1_sim, y2_sim), alpha = 0.3) +
  geom_abline(slope = 1, intercept = 0)




