library(survival)
library(splines)
data(colon)

colon.2 <- subset(colon, etype == 2)

fit.kaplan <- survfit(Surv(time, status)~rx, data=colon.2,
                      type = "kaplan-meier")
fit.felming <- survfit(Surv(time, status)~rx, data=colon.2,
                       type = "fleming-harrington")
# problem 1
summary(fit.kaplan)
summary(fit.felming)

# problem 2
fit.kaplan
fit.felming

