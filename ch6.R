# Simulation for quadratic model
set.seed(111)
N = 1000
x = rnorm(N,0,1)
e = rnorm(N,0,1)
beta0 = 1
beta1 = 1
beta2 = -5
y = beta0 + beta1*x + beta2*x^2 + e
plot(x,y)
abline(lm(y~x),col="red")
summary(lm(y~x))

x2 = x^2

summary(lm(y~x+x2))

attach(wage1)

summary(lm(lwage~educ+exper+I(exper^2)))

# Simulation for model with interaction term

set.seed(111)
N = 1000
x1 = rnorm(N,0,1)
x2 = rnorm(N,0,1) + x1
e = rnorm(N,0,1)
beta0 = 1
beta1 = 2
beta2 = 3
y = beta0 + beta1*x1 + beta2*x1*x2 + e
plot(x1,y)
summary(lm(y~x1))

attach(vote1)

summary(lm(voteA~expendA+expendB+prtystrA+I(prtystrA*expendA)))

pe = prtystrA*expendA

summary(lm(voteA~expendA+expendB+prtystrA+pe))

# Practice question 1

attach(hprice2)

# Consider the model, log(price)=\beta_0+\beta_1 log(nox)+\beta_2 log(dist)+\beta_3 rooms+\beta_4 rooms^2+\beta_5 stratio + u 

summary(lm(lprice~lnox+I(log(dist))+rooms+I(rooms^2)+stratio))

# Q1 what is the turning point of the effect of number of rooms on log(price)

# Q2 Does this turning point makes sense? Why do we get this estimation result?

sum(rooms<4.38)

# Practice question 2

attach(attend)

# Consider the model, stndfnl = \beta_0 + \beta_1 atndrte + \beta_2 priGPA + \beta_3 ACT + \beta_4 priGPA^2 + \beta_5 ACT^2 + \beta_6 priGPA*atndrte + u

summary(a<-lm(stndfnl~atndrte+priGPA+ACT+I(priGPA^2)+I(ACT^2)+I(priGPA*atndrte)))

pa = priGPA*atndrte

summary(a<-lm(stndfnl~atndrte+priGPA+ACT+I(priGPA^2)+I(ACT^2)+pa))


# Q1 What is the estimated marginal effect of atndrte on stndfnl?

# Q2 Does atndrte have significant effect on stndfnl?

linearHypothesis(a,c("atndrte=0","pa=0"))

# Q3 How can we summarize the marginal effect of atndrte on stndfnl?

# Simulation studies: residual analysis (residual against included x)
set.seed(100)
x = rnorm(500,0,1)
beta0 = 1
beta1 = 2
beta2 = 3
y = beta0 + beta1*x + beta2*x^2 + rnorm(500,0,1)
a <- lm(y~x+I(x^2))
summary(a)
plot(x,y)
abline(lm(y~x))
plot(x,a$residuals)
cor(x,a$residuals)

# Simulation studies: residual analysis (residual against not included x)
set.seed(100)
x1 = rnorm(500,0,1)
x2 = rnorm(500,0,1)
beta0 = 1
beta1 = 2
beta2 = 3
y = beta0 + beta1*x1 + beta2*x2 + rnorm(500,0,1)
a <- lm(y~x1+x2)
summary(a)
plot(x2,a$residuals)

# Simulation studies: residual analysis (residual against fitted values)
set.seed(100)
x1 = rnorm(500,0,1)
beta0 = 1
beta1 = 2
y = matrix(500,0,1)
for(i in 1:500){
  y[i] = beta0 + beta1*x1[i] + rnorm(1,0,1)
}
a <- lm(y~x1)
summary(a)
plot(a$fitted.values,a$residuals)
