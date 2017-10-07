#model 1
win.data = matrix(c(7,1,2,8), 2, 2)
fisher = fisher.test(win.data)

#model 2
data.m2 = read.table("knicks.csv",header=T, sep=",")
data.m2 = data.m2[1:18,]
attach(data.m2)
CA = 1 * (Player == "Anthony")
fit.m2 = glm(Win~Home + PCT + CA)
drop1(fit.m2, test="Chisq")
