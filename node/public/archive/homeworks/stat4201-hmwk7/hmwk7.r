# Problem 1
data.p1 <- read.table("ex0525", header=TRUE)

aov.p1 <- aov(CCK~Disease, data = data.p1)
summary(aov.p1)

postscript(file="~/Documents/LaTeX/stat4201-hmwk7/qq.eps",
           onefile=FALSE, horizontal=FALSE)
qqnorm(resid(aov.p1)); qqline(resid(aov.p1), color = 2)
dev.off()

bartlett.test(data.p1$CCK, data.p1$Disease)

kruskal.test <- kruskal.test(data.p1$CCK, data.p1$Disease)

# Problem 2
data.p2 <- read.csv("ex1319.csv", header=TRUE)

aov.p2 <- aov(IQ~ADOPTIVE*BIOLOGIC, data = data.p2)
summary(aov.p2)

postscript(file="~/Documents/LaTeX/stat4201-hmwk7/qq2.eps",
           onefile=FALSE, horizontal=FALSE)
qqnorm(resid(aov.p2)); qqline(resid(aov.p2), color = 2)
dev.off()

bartlett.test(data.p1$CCK, data.p1$Disease)
