library(survival)
library(splines)
data(colon)

colon.2 <- subset(colon, etype == 2)

# p1
fitcox.p1 <- coxph(Surv(time, status)~rx, data = colon.2)
# p2
exp(confint(fitcox.p1))
# p4
fitcox.p4 <- coxph(Surv(time, status)~rx+age+sex, data = colon.2)
