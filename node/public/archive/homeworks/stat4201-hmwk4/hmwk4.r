# problem1
cesium <- read.table("Chernobyl_Fallout", header=TRUE)
#(a)
postscript(file="~/Documents/LaTeX/stat4201-hmwk4/sactterplot.eps",
           onefile=FALSE, horizontal=FALSE) 
plot(cesium$Mushrooms~cesium$Soils)
dev.off()

#(b)
cesium.reg <- lm(cesium$Mushrooms~cesium$Soils)
cesium.reg.summary <- summary(cesium.reg)
print(cesium.reg.summary)
#(c)
cesium.trim <- cesium[c(1:16),]
cesium.trim.reg <- lm(cesium.trim$Mushrooms~cesium.trim$Soils)
cesium.trim.reg.summary <- summary(cesium.trim.reg)
print(cesium.trim.reg.summary)
# additional
lmi.p1 <- lm.influence(cesium.reg)
res.p1 <- resid(cesium.reg)
nt.p1 <- shapiro.test(res.p1)
print(nt.p1)

cor.p1 <- cor.test(cesium$Mushrooms, cesium$Soils)
print(cor.p1)

# problem2

attach(stackloss)
#a)
fit1 <- lm(stack.loss~stack.x[,1] + stack.x[,2] + stack.x[,3])

lmi <- lm.influence(fit1)
lms <- summary(fit1)
print(lms)

#b)
e <- resid(fit1)
nt.p2 <- shapiro.test(e)
print(nt.p2)

cor1.p2 <- cor.test(stack.loss, stack.x[,1])
print(cor1.p2)
cor2.p2 <- cor.test(stack.loss, stack.x[,2])
print(cor2.p2)
cor3.p2 <- cor.test(stack.loss, stack.x[,3])
print(cor3.p2)

s <- lms$sigma
si <- lmi$sigma
xxi <- diag(lms$cov.unscaled)
h <- lmi$hat

bi <- coef(fit1) - t(coef(lmi))
stand.resid <- e/(s*(1-h)^0.5)
student.resid <- e/(si*(1-h)^0.5)
DFFITS <- h^0.5*e/(si*(1-h))
