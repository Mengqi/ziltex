# Problem 1
library(MASS)
attach(birthwt)

x.p1 <- cbind(low, age, lwt, race, smoke, ptl, ht, ui, ftv)
ols.p1 <- lm(bwt~x.p1)
print(summary(ols.p1))
lms.p1 <- lmsreg(x.p1, bwt)
print(coef(lms.p1))

# Problem 2
# i)
library(car)
attach(stackloss)

reg.p2 <- lm(stack.loss~Air.Flow + Water.Temp + Acid.Conc.)
reg.vif.p2 <- vif(reg.p2)
print(reg.vif.p2)

# ii)
# model before data modification
ols.p2 <- lm(stack.loss~Air.Flow + Water.Temp + Acid.Conc.)
lms.p2 <- lmsreg(stack.loss~Air.Flow + Water.Temp + Acid.Conc.)
lts.p2 <- ltsreg(stack.loss~Air.Flow + Water.Temp + Acid.Conc.)
rlm.p2 <- rlm(stack.loss ~ ., stackloss, psi = psi.huber)

# data modification
stack.loss[20] <- 1450
Water.Temp[13] <- 180
Acid.Conc.[13] <- 1

# a)
ols.outlier.p2 <- lm(stack.loss~Air.Flow + Water.Temp + Acid.Conc.)
print(summary(ols.outlier.p2))

# b)
lmi <- lm.influence(ols.outlier.p2)
lms <- summary(ols.outlier.p2)
e <- resid(ols.outlier.p2)
s <- lms$sigma
si <- lmi$sigma
xxi <- diag(lms$cov.unscaled)
h <- lmi$hat
bi <- coef(ols.outlier.p2) - t(coef(lmi))

DFBETAS <- bi/t(si%o%xxi^0.5)
index.DFBETAS1 <- abs(DFBETAS[1,]) > 1
index.DFBETAS2 <- abs(DFBETAS[2,]) > 1
index.DFBETAS3 <- abs(DFBETAS[3,]) > 1
index.DFBETAS4 <- abs(DFBETAS[4,]) > 1
inf.DFBETAS1 <- DFBETAS[1,index.DFBETAS1]
inf.DFBETAS2 <- DFBETAS[2,index.DFBETAS2]
inf.DFBETAS3 <- DFBETAS[3,index.DFBETAS3]
inf.DFBETAS4 <- DFBETAS[4,index.DFBETAS4]

student.resid <- e/(si*(1-h)^0.5)
t.975.16 <- 2.5101
index.student <- abs(student.resid) > t.975.16
inf.student <- student.resid[index.student]
print(inf.student)

cooks.p2 <- cooks.distance(ols.outlier.p2)
f.975.4.17 <- 0.1161
index.cooks <- abs(cooks.p2) > f.975.4.17
inf.cooks <- cooks.p2[index.cooks]
print(inf.cooks)

DFFITS <- h^0.5*e/(si*(1-h))
index.DFFITS <- abs(DFFITS) > 1
inf.DFFITS <- DFFITS[index.DFFITS]
print(inf.DFFITS)

# c)
lms.outlier.p2 <- lmsreg(stack.loss~Air.Flow + Water.Temp + Acid.Conc.)
lts.outlier.p2 <- ltsreg(stack.loss~Air.Flow + Water.Temp + Acid.Conc.)
rlm.outlier.p2 <- rlm(stack.loss ~ ., stackloss, psi = psi.huber)
