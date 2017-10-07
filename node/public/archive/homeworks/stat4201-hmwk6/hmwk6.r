ssize.t.test<-function(sig.level = 0.05, power = 0.9, delta = 5,
                       sigma = 3, alt = "two.sided") 
{
  # check the alternative
  if (alt == "two.sided") {
    alpha = sig.level / 2 
  } else if (alt == "one.sided"){
    alpha = sig.level
  } else {
    print("Warning: incorrect alt")
    return(NULL)
  }

  beta = 1 - power
  
  size = 2 * ((qnorm(alpha) + qnorm(beta))^2 * sigma^2) / delta^2

  return(ceiling(size))  
}https://www.google.com/

ssize.prop.test<-function(p1 = 0.6, p2 = 0.75, sig.level = 0.05,
                          power = 0.9, alt = "two.sided") 
{
  # check the alternative
  if (alt == "two.sided") {
    alpha = sig.level / 2 
  } else if (alt == "one.sided"){
    alpha = sig.level
  } else {
    print("Warning: incorrect alt")
    return(NULL)
  }

  beta = 1 - power
  p = (p1 + p2) / 2
  q = 1 - p
  q1 = 1 - p1
  q2 = 1 - p2
  delta = p2 - p1

  molecular = (qnorm(alpha) * sqrt(2 * p * q)
               + qnorm(beta) * sqrt(p1 * q1 + p2 * q2))^2 
  
  size = molecular / delta^2

  return(ceiling(size))  
}
