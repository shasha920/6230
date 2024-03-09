# Simulation

# Regression with one dummy variable

e = rnorm(500,0,1)
x = rnorm(500,0,1)
d = rbinom(500,1,0.5)
beta0 = 1
delta0 = 5
beta1 = 1
y = beta0 + delta0*d + beta1*x + e
plot(x,y)
abline(lm(y[d==0]~x[d==0]),col="red")
abline(lm(y[d==1]~x[d==1]),col="blue")
summary(lm(y~d+x))

# Regression with 2 categories
attach(wage1)

summary(lm(wage~female+educ+exper+tenure))
# beta0hat = -1.56794
# delta0hat = -1.81085

male = 1-female

summary(lm(wage~female+male+educ+exper+tenure))

summary(lm(wage~male+educ+exper+tenure))

# Regression with 3 categories
attach(gpa1)

summary(lm(colGPA~business+engineer))
# 0.05149
# -0.14083

rest = 1-business-engineer

summary(lm(colGPA~business+rest))

# Regression with interaction of dummy variables
attach(wage1)

summary(lm(wage~female+married+I(female*married)))

# our base group here is single male
# average wage for this group is 5.168
# average wage for single female is 5.168-0.5564=4.6116
# average wage for married male is 5.168+2.8150=7.983
# average wage for married female is 5.1680-0.5564+2.8150-2.8607=4.5659

marrmale = (1-female)*married
marrfemale = female*married
singfemale = female*(1-married)

summary(lm(wage~marrmale+marrfemale+singfemale))

# our base group here is single male
# average wage for this group is 5.168
# average wage for single female is 5.168-0.5564=4.6116
# average wage for married male is 5.168+2.8150=7.983
# avefage wage for married female is 5.1680-0.6021=4.5659

# Question 1
attach(beauty)
# (a)
sum(abvavg*female)/sum(female)

sum(abvavg*(1-female))/sum((1-female))
# (b)
summary(lm(abvavg~female))

# (c) and (d)
summary(lm(lwage~belavg+abvavg,data=subset(beauty,female==1))) # only female observation

summary(lm(lwage~belavg+abvavg,data=subset(beauty,female==0)))

# (e)

summary(a<- lm(lwage~belavg+abvavg+educ+exper+expersq+union+goodhlth+black+married+south+bigcity+smllcity+service,data=subset(beauty,female==1)))

summary(a<- lm(lwage~belavg+abvavg+educ+exper+expersq+union+goodhlth+black+married+south+bigcity+smllcity+service,data=subset(beauty,female==0)))

# Question 2

# For loanapp data, it contains lots of missing value. 
# Usually people use . in excel to represent missing value
# However, R read . as one type of input.
# In order to tell R that . is missing value, we need to specify it as NA
# This can be done when we import data
attach(loanapp)

#(b)

summary(lm(approve~white))

# (c)

summary(lm(approve~white+hrat+obrat+loanprc+unem+male+married+dep+sch+cosign+chist+pubrec+mortlat1+mortlat2+vr, data = loanapp))

# (d)

summary(lm(approve~white+hrat+obrat+I(white*obrat)+loanprc+unem+male+married+dep+sch+cosign+chist+pubrec+mortlat1+mortlat2+vr))

# Question 3

# (a)

attach(jtrain2)

sum(train)/length(train)

attach(jtrain3)

sum(train)/length(train)

# (b)

attach(jtrain2)

summary(lm(re78~train))

# (c)

summary(lm(re78~train+re74+re75+educ+age+black+hisp))

# (d)

attach(jtrain3)

summary(lm(re78~train))

summary(lm(re78~train+re74+re75+educ+age+black+hisp))

# Linear Probability vs. Logistic regression

attach(mroz)
lpm <- lm(inlf~nwifeinc+educ+exper+expersq+age+kidslt6+kidsge6)
summary(lpm)

# percentage of correct prediction
# when yhat > 0.5 => we predict y = 1
# when yhat <= 0.5 => we predict y = 0

yhat <- lpm$fitted.values
ytilde = matrix(0,length(yhat),1)
for(i in 1:length(yhat)){
  ytilde[i] <- if(yhat[i]>0.5) 1 else 0
}
pcp = matrix(0,length(yhat),1)
for(i in 1:length(yhat)){
  pcp[i] <- if(ytilde[i]==inlf[i]) 1 else 0
}

sum(pcp)/length(yhat)

# Logit Model
logitm <-glm(inlf~nwifeinc+educ+exper+expersq+age+kidslt6+kidsge6,family = "binomial")
summary(logitm)

yhat <- logitm$fitted.values
ytilde = matrix(0,length(yhat),1)
for(i in 1:length(yhat)){
  ytilde[i] <- if(yhat[i]>0.5) 1 else 0
}
pcp = matrix(0,length(yhat),1)
for(i in 1:length(yhat)){
  pcp[i] <- if(ytilde[i]==inlf[i]) 1 else 0
}

sum(pcp)/length(yhat)
