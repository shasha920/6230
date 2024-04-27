install.packages("plm")

# pooled cross section

summary(lm(lwage~y85+educ+exper+expersq+union+female, data = kielmc))

summary(lm(rprice~nearinc,data=subset(kielmc,y81==1)))

summary(lm(rprice~nearinc,data=subset(kielmc,y81==0)))

summary(lm(rprice~nearinc+y81+y81nrinc,data=kielmc))

summary(lm(rprice~nearinc+y81+y81nrinc+age+agesq,data=kielmc))

summary(lm(rprice~nearinc+y81+y81nrinc+age+agesq+intst+land+area+rooms+baths,data=kielmc))

# Examples for Difference-in-Difference-in-Differences

# Suppose Big House and Small House Have different trend

attach(kielmc)

big = (area>2000)

big = as.numeric(big)

summary(lm(rprice~nearinc+y81+y81nrinc+age+agesq+big+I(big*y81)+I(big*nearinc)+I(big*y81nrinc),data=kielmc))

# panel data part
# pooled cross section

a = plm(lfare~y98+y99+y00+passen+bmktshr+ldist+ldistsq, model = c("pooling"), data = airfare, index = c("id","year"))

summary(a)

# First Difference

b = plm(lfare~y98+y99+y00+passen+bmktshr+ldist+ldistsq, model = c("fd"), data = airfare, index = c("id","year"))

summary(b)

# Fixed Effect Estimation (demeaned estimation)

c = plm(lfare~y98+y99+y00+passen+bmktshr+ldist+ldistsq, model = c("within"), data = airfare, index = c("id","year"))

summary(c)

# Random Effect

d = plm(lfare~y98+y99+y00+passen+bmktshr+ldist+ldistsq, model = c("random"), data = airfare, index = c("id","year"))

summary(d)

# Hausman test
phtest(c,d)

