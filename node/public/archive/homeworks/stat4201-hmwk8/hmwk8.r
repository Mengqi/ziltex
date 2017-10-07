# Problem 1
data.p1 <- read.csv("ex2012.csv", header=TRUE)
attach(data.p1)

fit.p1 <- glm(Y~CK+H, family=binomial)
confint(fit.p1)

# Problem 2
CK.log <- log(CK)

postscript(file="~/Documents/LaTeX/stat4201-hmwk8/scatter.eps",
           onefile=FALSE, horizontal=FALSE)
plot(H, CK.log, pch=c(21, 22)[GROUP])
dev.off()

Y <- 1*(GROUP=="Case")
CK.square <- CK^2
fit.b1 <- glm(Y~CK+CK.square, family=binomial)

CK.log.square <- CK.log^2
fit.b2 <- glm(Y~CK.log+CK.log.square, family=binomial)

fit.c <- glm(Y~CK.log+H, family=binomial)

drop1(fit.c, test="Chisq")

odd1 <- exp(-28.9 + 4.02 * log(100) + 0.14 * 100)
odd2 <- exp(-28.9 + 4.02 * log(80) + 0.14 * 85)
ratio <- odd1 / odd2
