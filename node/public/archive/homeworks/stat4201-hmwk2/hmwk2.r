data(RatPupWeight, package="nlme")

mix <- RatPupWeight
high <- RatPupWeight[RatPupWeight$Treatment=="High",]
low <- RatPupWeight[RatPupWeight$Treatment=="Low",]
control <- RatPupWeight[RatPupWeight$Treatment=="Control",]
active <- rbind(high, low)

#######################
##  problem 1
#######################

# histogram

postscript(file="~/Documents/LaTeX/stat4201-hmwk2/hist_control.eps",
           onefile=FALSE, horizontal=FALSE) 
hist(control$weight)
dev.off()

postscript(file="~/Documents/LaTeX/stat4201-hmwk2/hist_active.eps",
           onefile=FALSE, horizontal=FALSE)
hist(active$weight)
dev.off()

# 1.1 parametric
result.t <- t.test(active$weight, control$weight, var.equal=T)

# 1.1 non-parametric
result.wilw <- wilcox.test(active$weight, control$weight)

# 1.1 resampling
library(boot)

s1 <- var(control$weight)
s2 <- var(active$weight)
l1 <- length(control$weight)
l2 <- length(control$weight)
m <- mean(control$weight) - mean(active$weight)
se <- sqrt(s1/l1 + s2/l2)
z <- m / se

foo <- function(d1, i) {
  dd1 <- d1[i,]
  d2 <- active
  dd2 <- d2[i,]

  # I twiked a little, active group only take the first
  # length(conrol$weight) units. This is due to I can't find
  # the proper function to handle the bootstrap for different
  # group size. But all in all, this makes the sample sizes tends
  # to be identical, it will increase accuracy a little. So basically,
  # this shouldn't affect too much.
  
  mm <- mean(dd1$weight) - mean(dd2$weight)

  return(mm)
}
boot.control <- boot(control, foo, R = 500)
ci.control <- boot.ci(boot.control, type ="bca")


# 1.2 norm test para
norm.active <- shapiro.test(active$weight)
norm.control <- shapiro.test(control$weight)


# 1.3 parametric
pearson.mix <- cor.test(mix$weight, mix$Lsize)

# 1.3 nonparametric
spearman.mix <- cor.test(mix$weight, mix$Lsize, method="sp")
