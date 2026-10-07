# Function for type II censored data generation of simple SSLT experiment
ExpoSSLTGen<-function(n,tau1,tau2,r)
{
  u1 <- runif(n,0,1)
  v1 <- sort(u1)
  
  t <- 1:n 
  z1 <- 1-exp(-(tau1/theta1))
  z2 <- 1-exp(-(tau1/theta1)- ((tau2-tau1)/theta2))
  n11 <- 0
  n21 <- 0
  for(i in 1:length(v1))
  {
    if(v1[i]<=z1)
    {
      t[i] <- (-(theta1)*(log(1-v1[i])))
      n11 <- n11 + 1
    }
    if(v1[i]>z1 && v1[i]<=z2)
    {
      t[i] <- -((theta2)*log(1-v1[i]))-((theta2/theta1)*(tau1)) + tau1
      n21 <- n21+1
    }
    if(v1[i]>z2)
    {
      t[i] <- -((theta3)*log(1-v1[i]))-((theta3/theta1)*(tau1)) -((theta3/theta2)*(tau2-tau1))  + tau2
    }
  }
  if((n11+n21)<=r)
  {
    n1 <- n11
    n2 <- n21
    n3 <- r - n11 - n21
  }
  if(n11<r && (n11+n21)>=r)
  {
    n1 <- n11
    n2 <- r-n11
    n3 <- 0
  }
  if(n11>=r)
  {
    n1 <- r-n11
    n2 <- 0
    n3 <- 0
  }
  t1 <- t[1:r]
  return(c(n1,n2,n3,t1))
}