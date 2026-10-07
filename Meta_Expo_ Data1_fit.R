K = 2 
# Take a fixed value of n for all K samples
#n = 30
n_all <- c(14,15)
tau1_all <- c(30,30)
tau2_all <- c(50,50)
tau3_all <- c(70,70)
tau4_all <- c(90,90)
r_all <- c(14,15)
r <- sum(r_all)


n1_data1 <- 6
n2_data1 <- 3
n3_data1 <- 3
n4_data1 <- 2
n5_data1 <- 0
data1 <- c(83.50, 91.00, 91.00, 97.00, 107.00, 109.50, 114.00, 115.41, 128.61, 133.53, 138.58, 140.00, 152.08, 155.10)-80
n1_data2 <- 4
n2_data2 <- 6
n3_data2 <- 0
n4_data2 <- 3
n5_data2 <- 2
data2 <- c(91.00, 93.00, 94.00, 98.20, 115.81, 116.00, 116.50, 117.25, 126.75, 127.50, 154.33, 159.50, 164.00, 184.14, 188.33)-80


n_S1 <- n1_data1 + n1_data2 
n_S2 <- n2_data1 + n2_data2 
n_S3 <- n3_data1 + n3_data2
n_S4 <- n4_data1 + n4_data2 
n_S5 <- n5_data1 + n5_data2 

Dataa <- c(data1,data2)
Dataa <- sort(Dataa)
n <- 29
tau1 <- 30
tau2 <- 50
tau3 <- 70
tau4 <- 90

ord_MLE_theta11 <- 72.52000 
ord_MLE_theta21 <- 37.49500 
ord_MLE_theta31 <- 37.49500 
ord_MLE_theta41 <- 15.35429 
ord_MLE_theta51 <- 15.35429

ord_MLE_theta1 <- 1/ord_MLE_theta11
ord_MLE_theta2 <- 1/ord_MLE_theta21
ord_MLE_theta3 <- 1/ord_MLE_theta31
ord_MLE_theta4 <- 1/ord_MLE_theta41
ord_MLE_theta5 <- 1/ord_MLE_theta51


tt <- seq(0,109, by=0.1) 
mle_F <- 1:length(tt) 

for (i in 1:length(tt)){
  if(tt[i]<tau1){
    mle_F[i] <- 1 - exp(-ord_MLE_theta1*(tt[i]))
  }else if(tt[i]<tau2 && tt[i]>=tau1){
    mle_F[i] <- 1 - exp(-(ord_MLE_theta2*((tt[i])-(tau1)))-(ord_MLE_theta1*(tau1)))
  }else if(tt[i]<tau3 && tt[i]>=tau2){
    mle_F[i] <- 1 - exp(-(ord_MLE_theta2*((tau2)-(tau1)))-(ord_MLE_theta3*(((tt[i]))-(tau2)))-(ord_MLE_theta1*(tau1)))
  }else if(tt[i]<tau4 && tt[i]>=tau3){
    mle_F[i] <- 1 - exp(-(ord_MLE_theta2*((tau2)-(tau1))) -(ord_MLE_theta3*((tau3)-(tau2))) -(ord_MLE_theta4*(((tt[i]))-(tau3)))-(ord_MLE_theta1*(tau1)))
  }else if(tt[i]>=tau4){
    mle_F[i] <- 1 - exp(-(ord_MLE_theta2*((tau2)-(tau1))) -(ord_MLE_theta3*((tau3)-(tau2))) -(ord_MLE_theta4*((tau4)-(tau3))) -(ord_MLE_theta5*(((tt[i]))-(tau4)))-(ord_MLE_theta1*(tau1)))
  }
}

#### For Order restricted Bayes Estimate

ord_MLE_theta11 <- 77.22 
ord_MLE_theta21 <- 45.57 
ord_MLE_theta31 <- 34.54 
ord_MLE_theta41 <- 20.67 
ord_MLE_theta51 <- 13.79

ord_MLE_theta1 <- 1/ord_MLE_theta11
ord_MLE_theta2 <- 1/ord_MLE_theta21
ord_MLE_theta3 <- 1/ord_MLE_theta31
ord_MLE_theta4 <- 1/ord_MLE_theta41
ord_MLE_theta5 <- 1/ord_MLE_theta51


tt <- seq(0,109, by=0.1) 
BE_F <- 1:length(tt) 

for (i in 1:length(tt)){
  if(tt[i]<tau1){
    BE_F[i] <- 1 - exp(-ord_MLE_theta1*(tt[i]))
  }else if(tt[i]<tau2 && tt[i]>=tau1){
    BE_F[i] <- 1 - exp(-(ord_MLE_theta2*((tt[i])-(tau1)))-(ord_MLE_theta1*(tau1)))
  }else if(tt[i]<tau3 && tt[i]>=tau2){
    BE_F[i] <- 1 - exp(-(ord_MLE_theta2*((tau2)-(tau1)))-(ord_MLE_theta3*(((tt[i]))-(tau2)))-(ord_MLE_theta1*(tau1)))
  }else if(tt[i]<tau4 && tt[i]>=tau3){
    BE_F[i] <- 1 - exp(-(ord_MLE_theta2*((tau2)-(tau1))) -(ord_MLE_theta3*((tau3)-(tau2))) -(ord_MLE_theta4*(((tt[i]))-(tau3)))-(ord_MLE_theta1*(tau1)))
  }else if(tt[i]>=tau4){
    BE_F[i] <- 1 - exp(-(ord_MLE_theta2*((tau2)-(tau1))) -(ord_MLE_theta3*((tau3)-(tau2))) -(ord_MLE_theta4*((tau4)-(tau3))) -(ord_MLE_theta5*(((tt[i]))-(tau4)))-(ord_MLE_theta1*(tau1)))
  }
}




Emp_cdf <- 1:length(tt)

for(i in 1:(length(Dataa)-1)){
  
  for(j in 1: length(tt)){
    if(tt[j]<Dataa[1]){
      Emp_cdf[j] <- 0
    }
    if(tt[j]>=Dataa[i] && tt[j]<Dataa[i+1]){
      Emp_cdf[j] <- i/29
      
    }
    if(tt[j]>=Dataa[29]){
      Emp_cdf[j] <- 1
    }
  }
  
  
}

plot(tt,Emp_cdf, type='l',col='blue',xlab = 'Data', ylab = 'CDF', main = 'Empirical Vs Fitted CDF')
lines(tt,mle_F,col='red')
lines(tt,BE_F,col='black')
legend('topleft', legend = c('Empirical CDF', 'Fitted CDF(MLE)','Fitted CDF(BE)'), col = c('blue', 'red','black'), lty = 1)


KS_dist_MLE <- max(abs(Emp_cdf-mle_F))
KS_dist_BE <- max(abs(Emp_cdf-BE_F))


