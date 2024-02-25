# Chapter 4 hypothesis testing

# simulation

# t test

set.seed(100)
II = 10000
N = 300
beta0 = 1
beta1 = 2
beta0mat = matrix(0,II,1)
beta1mat = matrix(0,II,1)
varbeta0mat = matrix(0,II,1)
varbeta1mat = matrix(0,II,1)
for(i in 1:II){
  x = rnorm(N,0,2)
  e = rnorm(N,0,1)
  y = beta0 + beta1*x + e
  beta0mat[i]=summary(lm(y~x))$coefficients[1,1]
  beta1mat[i]=summary(lm(y~x))$coefficients[2,1]
  varbeta0mat[i]=summary(lm(y~x))$coefficients[1,2]
  varbeta1mat[i]=summary(lm(y~x))$coefficients[2,2]
}

tstat = rt(10000,300-1-1)

# Null hypothesis is beta0=1, which is correct

hbeta0 <- hist((beta0mat-1)/varbeta0mat, plot = FALSE)
ht <- hist(tstat, plot = FALSE)

plot(hbeta0, col=rgb(1,0,0,0.5)) # red one
plot(ht, col=rgb(0,0,1,0.5), add = TRUE) # blue one
abline(v=c(-qt(0.975,300-1-1),qt(0.975,300-1-1)), col=c("yellow","yellow"))

# we see that the two histograms overlap each other, this suggests that the distribution we discussed in class is correct.

# We can play with the null hypothesis, suppose I change it to beta0=0.8

hbeta0 <- hist((beta0mat-0.6)/varbeta0mat, plot = FALSE)
ht <- hist(tstat, plot = FALSE)

plot(hbeta0, col=rgb(1,0,0,0.5), xlim = c(-4,8)) # red one
plot(ht, col=rgb(0,0,1,0.5), add = TRUE) # blue one
abline(v=c(-qt(0.975,300-1-1),qt(0.975,300-1-1)), col=c("yellow","yellow"))
# We can play with the null hypothesis, suppose I change it to beta0=1.2

hbeta0 <- hist((beta0mat-1.2)/varbeta0mat, plot = FALSE)
ht <- hist(tstat, plot = FALSE)

plot(hbeta0, col=rgb(1,0,0,0.5), xlim = c(-8,4)) # red one
plot(ht, col=rgb(0,0,1,0.5), add = TRUE) # blue one
abline(v=c(-qt(0.975,300-1-1),qt(0.975,300-1-1)), col=c("yellow","yellow"))

# Same logic with beta1
# Null hypothesis is beta1=2, which is correct

hbeta1 <- hist((beta1mat-2)/varbeta1mat, plot = FALSE)
ht <- hist(tstat, plot = FALSE)

plot(hbeta1, col=rgb(1,0,0,0.5)) # red one
plot(ht, col=rgb(0,0,1,0.5), add = TRUE) # blue one
abline(v=c(-qt(0.975,300-1-1),qt(0.975,300-1-1)), col=c("yellow","yellow"))

# we see that the two histograms overlap each other, this suggests that the distribution we discussed in class is correct.

# We can play with the null hypothesis, suppose I change it to beta1=1.8

hbeta1 <- hist((beta1mat-1.8)/varbeta1mat, plot = FALSE)
ht <- hist(tstat, plot = FALSE)

plot(hbeta1, col=rgb(1,0,0,0.5), xlim = c(-4,10)) # red one
plot(ht, col=rgb(0,0,1,0.5), add = TRUE) # blue one
abline(v=c(-qt(0.975,300-1-1),qt(0.975,300-1-1)), col=c("yellow","yellow"))

# We can play with the null hypothesis, suppose I change it to beta1=2.2

hbeta1 <- hist((beta1mat-2.2)/varbeta1mat, plot = FALSE)
ht <- hist(tstat, plot = FALSE)

plot(hbeta1, col=rgb(1,0,0,0.5), xlim = c(-10,4)) # red one
plot(ht, col=rgb(0,0,1,0.5), add = TRUE) # blue one
abline(v=c(-qt(0.975,300-1-1),qt(0.975,300-1-1)), col=c("yellow","yellow"))

install.packages("AER")
library(AER)

# Example question 1

attach(gpa1)
# part a:

summary(a<-lm(colGPA~hsGPA+ACT+skipped+alcohol))

# part b: testing beta_2 = 0 vs beta_2 =/= 0
# We can answer this question using either one of the three methods: t statistics, confidence interval, and p-value
# We can calculate t stat as,

# we find that the absolute value of test statistics is smaller than critical value, so we fail to reject the null hypothesis.

qt(0.975,141-4-1)

# Confidence interval
CIbeta2 = c(summary(a)$coefficients[3,1]-qt(0.975,141-4-1)*summary(a)$coefficients[3,2],summary(a)$coefficients[3,1]+qt(0.975,141-4-1)*summary(a)$coefficients[3,2])
# since 0 is contained in confidence interval, we fail to reject the null hypothesis

# part c: testing beta_4=1 vs beta_4 =/= 1

# We can calculate t stat as,
tstatbeta4 = (summary(a)$coefficients[5,1]-1)/summary(a)$coefficients[5,2]
# We can use t-table to find critical value or ask R to find for us
qt(0.975,141-4-1) # finding critical value for t statistics when alpha=0.05 and two-sided test
# This is a two-sided test, so we can test it by comparing the absolute value of t statistics with critical value
(abs(tstatbeta4)>qt(0.975,141-4-1))
# we find that the absolute value of test statistics is larger than critical value, so we reject the null hypothesis.

# Example question 2
attach(vote1)

summary(a<-lm(voteA~lexpendA+lexpendB))

linearHypothesis(a,c("lexpendA=-lexpendB"))

# Example question 3
attach(wage2)

summary(a<-lm(wage~educ+exper+meduc+feduc))

linearHypothesis(a,c("educ=exper"))

linearHypothesis(a,c("meduc=feduc"))

# Example question 4
attach(hprice1)

summary(a <- lm(price~assess))
tstatbeta1 = (summary(a)$coefficients[2,1]-1)/summary(a)$coefficients[2,2]
qt(0.95,88-1-1)

abs(tstatbeta1)>qt(0.95,88-1-1)

linearHypothesis(a,c("(Intercept) = 0","assess=1"))
  
# Example question 5
attach(ceosal2)

summary(a <- lm(lsalary~lsales+lmktval+profmarg+ceoten+comten))

linearHypothesis(a,c("ceoten=0","comten=0"))

# Example question 6
attach(return1)

summary(lm(return~dkr+eps+netinc+salary))
