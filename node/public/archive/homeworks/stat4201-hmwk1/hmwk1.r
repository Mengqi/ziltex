# Advanced Data Analysis Homework 1
#
# Name Mengqi Zong
# UNI: mz2326
#

library(boot)
library(bootstrap)

male <- read.table("male.data", header=TRUE)
female <- read.table("female.data", header=TRUE)
# "mix.data" is the combination of "male.data" and "female.data"
mix <- read.table("mix.data", header=TRUE)

#######################
##  problem 1    
#######################

postscript(file="~/Documents/LaTeX/stat4201-hmwk1/boxplot_mix.eps", onefile=FALSE, horizontal=FALSE)
boxplot(mix$SALARY)
dev.off()

#######################
##  problem 2    
#######################

# male - EPA
postscript(file="~/Documents/LaTeX/stat4201-hmwk1/boxplot_male.eps", onefile=FALSE, horizontal=FALSE)
boxplot(male$SALARY)
dev.off()

stem(male$SALARY)

postscript(file="~/Documents/LaTeX/stat4201-hmwk1/hist_male.eps", onefile=FALSE, horizontal=FALSE)
hist(male$SALARY)
dev.off()

# male - Standard Deviation
sd.male <- sd(male$SALARY)
cat("Male Salary Standard Deviation = ")
print(sd.male)

# male - IQR
iqr.male <- IQR(male$SALARY)
cat("Male IQR = ")
print(iqr.male)

# female - EPA
postscript(file="~/Documents/LaTeX/stat4201-hmwk1/boxplot_female.eps", onefile=FALSE, horizontal=FALSE)
boxplot(female$SALARY)
dev.off()

stem(female$SALARY)

postscript(file="~/Documents/LaTeX/stat4201-hmwk1/hist_female.eps", onefile=FALSE, horizontal=FALSE)
hist(female$SALARY)
dev.off()

# female - Standard Deviation
sd.female <- sd(female$SALARY)
cat("Female Salary Standard Deviation = ")
print(sd.female)

# female - IQR
iqr.female <- IQR(female$SALARY)
cat("Female IQR = ")
print(iqr.female)

#######################
##  problem 3
#######################

# male - Jackknife Standard Deviation
jacksd.male <- jackknife(male$SALARY, sd)
cat("Male Salary Standard Deviation Using Jackknife = ")
print(jacksd.male)

# male - Bootstrap Standard Deviation
foosd <- function(d, i) {
  d2 <- d[i,]
  return(sd(d2$SALARY))
}
bootsd.male <- boot(male, foosd, R = 500)
cat("Male Salary Standard Deviation Using Bootstrap = ")
print(bootsd.male)

# male - Jackknife IQR
jackiqr.male <- jackknife(male$SALARY, IQR)
cat("Male Salary IQR Using Jackknife = ")
print(jackiqr.male)

# male - Bootstrap IQR
fooiqr <- function(d, i) {
  d2 <- d[i,]
  return(IQR(d2$SALARY))
}
bootiqr.male <- boot(male, fooiqr, R = 500)
cat("Female Salary IQR Using Bootstrap = ")
print(bootiqr.male)

# female - Jackknife Standard Deviation
jacksd.female <- jackknife(female$SALARY, sd)
cat("Female Salary Standard Deviation Using Jackknife = ")
print(jacksd.female)

# female - Bootstrap Standard Deviation
bootsd.female <- boot(female, foosd, R = 500)
cat("Female Salary Standard Deviation Using Bootstrap = ")
print(bootsd.female)

# female - Jackknife IQR
jackiqr.female <- jackknife(female$SALARY, IQR)
cat("FEMale Salary IQR Using Jackknife = ")
print(jackiqr.female)

# female - Bootstrap IQR
fooiqr <- function(d, i) {
  d2 <- d[i,]
  return(IQR(d2$SALARY))
}
bootiqr.female <- boot(female, fooiqr, R = 500)
cat("Female Salary IQR Using Bootstrap = ")
print(bootiqr.female)
