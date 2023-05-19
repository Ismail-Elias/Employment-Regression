df = read.csv(file.choose(), header = T)

#Check the regression assumption
lm.1 = lm(Employed ~ GNP.deflator + GNP + Unemployed + Armed.Forces + Population, data = df)
#plot the regression information
par(mfrow = c(2,2))
plot(lm.1)
#multicollinearity
car::vif(lm.1)
#the original regression contains lots of multicollinearity, hence some of the variable should be removed
#correlation matrix
cor(df[complete.cases(df), ])

#based on the correlation matrix, GNP.deflator, Population and year have been removed due to high multicollinearity.

#Regression 2. 
lm.2 = lm(Employed ~ GNP + Armed.Forces + Unemployed, data = df)

#linearity
par(mfrow = c(2,2))
plot(lm.2)

attach(df)
#normality assumption
shapiro.test(Employed)
shapiro.test(GNP)
shapiro.test(Unemployed)
shapiro.test(Armed.Forces)
#based on the shapiro test, Employed is the one that is not normal. hence, new data (random generate) should be done

#independent row
#since, each row contains the information yearly, hence there is no interruption from previous row

#multicollinearity
car::vif(lm.2)
#since the VIF value is not more than 10, there is no collinearity between the independent variables

#Homoscedacity
lmtest::bptest(lm.1)
#Since the p-value is 0.06 > 0.05. we have sufficient evidence to conclude that there is no heteroscedacity in the
#regression model.

#######################################
df.2 = df[order(df$Employed), ]
set.seed(000)
df.2$Employed_r = sort(rnorm(17, mean = 68.73, sd = sd(df.2$Employed)))

#Check for Regression assumption

#normality
shapiro.test(df.2$Employed_r)
#since the p-value is 0.57 > 0.05. we conclude that the Employed is normally distributed.

#linearity
lm.3 = lm(Employed_r ~ GNP + Unemployed + Armed.Forces, data = df.2)

par(mfrow = c(2,2))
plot(lm.3)
#Based on normal QQ plot, the data point distributed nearby the regression line. hence, we assumed it to have linear relationship

#homoscedacity
lmtest::bptest(lm.3)
#the p-value is 0.8038 > 0.05. Hence, this conclude that there is no heteroscedacity.

#multicollinearity
car::vif(lm.3)
#since the VIF value is not more than 10, there is no collinearity between the independent variables

#######################################

#Data summarization
library(psych)
describe(df.2)

#boxplot
par(mfrow = c(2,2))
boxplot(df.2$GNP, xlab = 'GNP', col = 'aquamarine2')
boxplot(df.2$Armed.Forces, xlab = 'Armed Forces', col = 'aquamarine2')
boxplot(df.2$Unemployed, xlab = 'Unemployed', col = 'aquamarine2')
boxplot(df.2$Employed_r, xlab = 'Generated Employed', col = 'aquamarine2')


#histogram
par(mfrow = c(2,2))
hist(df.2$GNP, xlab = 'GNP', prob = T)
lines(density(df.2$GNP))
hist(df.2$Armed.Forces, xlab = 'Armed Forces', prob = T)
lines(density(df.2$Armed.Forces, na.rm = T))
hist(df.2$Unemployed, xlab = 'Unemployed', prob = T)
lines(density(df.2$Unemployed, na.rm = T))
hist(df.2$Employed_r, xlab = 'Generated Employed', prob = T)
lines(density(df.2$Employed_r))

#scatter plot
plot(df.2$GNP, df.2$Employed_r)
plot(df.2$Armed.Forces, df.2$Employed_r)
plot(df.2$Unemployed, df.2$Employed_r)

#check missing value
summary(df.2)

#Missing value treatment
df.2$Armed.Forces[is.na(df.2$Armed.Forces)] = mean(df.2$Armed.Forces, na.rm = T)
df.2$Unemployed[is.na(df.2$Unemployed)] = mean(df.2$Unemployed, na.rm = T)

#Check missing value
sum(is.na(df.2))

#build the regression model
lm.4 = lm(Employed_r ~ GNP + Unemployed + Armed.Forces, data = df.2)

#summarize the regression model
summary(lm.4)

#scatter plot
par(mfrow = c(2,2))
plot(lm.4)

#exclude the unused data
df.3 = df.2[, -c(1,2,3,7,8,9)]

#bootstrap
library(bootstrap)

boot.lm = function(dtx, b){
  dt = data.frame(dtx)
  dt_gnp = dt[,1]
  dt_unemp = dt[,2]
  dt_armf = dt[,3]
  dt_emp = dt[,4]
  
  model = lm(dt_emp ~ dt_gnp + dt_unemp + dt_armf, data = dtx)
  gnp.hat = coef(model)[2]
  unemp.hat = coef(model)[3]
  armf.hat = coef(model)[4]
  
  n=nrow(dtx)
  gnp.b=numeric(b)
  unemp.b=numeric(b)
  armf.b=numeric(b)
  
  set.seed(000)
  for(i in 1:b){
    k = sample(1:n, size = n, replace = T)
    dt_gnp.1 = dt_gnp[k]
    dt_unemp.1 = dt_unemp[k]
    dt_armf.1 = dt_armf[k]
    dt_emp.1 = dt_emp[k]
    model.1 = lm(dt_emp.1 ~ dt_gnp.1 + dt_unemp.1 + dt_armf.1)
    
    gnp.b[i] = coef(model.1)[2]
    unemp.b[i] = coef(model.1)[3]
    armf.b[i] = coef(model.1)[4]
  }
  bias.gnp = mean(gnp.b - gnp.hat)
  bias.unemp = mean(unemp.b - unemp.hat)
  bias.armf = mean(armf.b - armf.hat)
  
  se.gnp = sd(gnp.b)
  se.unemp = sd(unemp.b)
  se.armf = sd(armf.b)
  
  par(mfrow = c(1,3))
  hist(gnp.b, prob = T, col = 'cyan', xlab = 'GNP')
  lines(density(gnp.b), lwd = 2, lty = 2, col = 'aquamarine3')

  hist(unemp.b, prob = T, col = 'cyan', xlab = 'Unemployed')
  lines(density(unemp.b), lwd = 2, lty = 2, col = 'aquamarine3')
  
  hist(armf.b, prob = T, col = 'cyan', xlab = 'Armed Force')
  lines(density(armf.b), lwd = 2, lty = 2, col = 'aquamarine3')
  
  mtext(paste(b, "Bootstrap Sampling", sep = " "), side = 3, line = -2, outer = TRUE)
  
  print(list(sample.gnp = gnp.b[1:10], sample.unemp = unemp.b[1:10], sample.armf = armf.b[1:10], 
             est.gnp = gnp.hat, est.unemp = unemp.hat, est.armf = armf.hat, 
             bias.gnp = bias.gnp, bias.unemp = bias.unemp, bias.armf = bias.armf,
             se.gnp = se.gnp, se.unemp = se.unemp, se.armf = se.armf, lm_summary = summary(model.1)))
}

boot.lm(df.3, 100) #100 bootstrap
boot.lm(df.3, 1000) #1000 bootstrap
boot.lm(df.3, 5000) #5000 bootstrap
boot.lm(df.3, 6000) #6000 bootstrap

reg.boot = function(df, index){
  gnp = df$GNP[index]
  unemp = df$Unemployed[index]
  armf = df$Armed.Forces[index]
  emp = df$Employed[index]
  
  model = lm(emp~gnp + unemp + armf)
  coef(model)
}

library(boot)
reg.model = boot(df.3, reg.boot, R = 1000)
reg.model

boot.ci(reg.model, index = 2)
