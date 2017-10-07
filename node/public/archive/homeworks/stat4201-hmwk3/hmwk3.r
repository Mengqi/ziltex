# problem 1

fisher.p1 <- fisher.test(matrix(c(149,48,129,68),2,2))
print(fisher.p1)

# problem 3

data.p3 <-
array(c(171, 465, 0, 2,
        243, 753, 0, 3,
         98, 325, 3, 3,
        108, 273, 21, 8),
      dim = c(2, 2, 4),
      dimnames = list(
          Make = c("Ford", "Other"),
          Cause = c("Other", "Tire"),
          Speed = c("0-40", "41-55", "56-65", ">66")))

mantelhaen.p3<- mantelhaen.test(data.p3)
print(mantelhaen.p3)
