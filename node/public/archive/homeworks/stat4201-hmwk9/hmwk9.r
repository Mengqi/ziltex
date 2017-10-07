data <- read.csv("ex2225.csv", header=TRUE)
attach(data)

fit.p1 <- glm(mates~bodysize, family = poisson)
confint.p1 <- confint(fit.p1)
anova.p1 <- anova(fit.p1, test = "Chi")
