K = 2 
# Take a fixed value of n for all K samples
n_all <- c(14,15)
tau1_all <- c(30,30)
tau2_all <- c(50,50)
tau3_all <- c(70,70)
tau4_all <- c(90,90)
r_all <- c(14,15)
r <- sum(r_all)

tau1 <- 30
tau2 <- 50
tau3 <- 70
tau4 <- 90

theta11 <- 77.22
theta21 <- 45.57 
theta31 <- 34.54 
theta41 <- 20.67 
theta51 <- 13.79

theta1 <- 1/theta11 
theta2 <- 1/theta21
theta3 <- 1/theta31 
theta4 <- 1/theta41 
theta5 <- 1/theta51


alpha <-1

z1 <- 1 - exp(-theta1*(tau1^(alpha)))
z2 <- 1 - exp(-(theta2*((tau2^(alpha))-(tau1^(alpha))))-(theta1*(tau1^alpha)))
#z3 <- 1 - exp(-(lambda02*((tau2^(alpha))-(tau1^(alpha))))-(beta*lambda01*(((tau2+tau1)^alpha)-(tau2^alpha)))-(lambda01*(tau1^alpha)))
z3 <- 1 - exp(-(theta2*((tau2^(alpha))-(tau1^(alpha))))-(theta3*(((tau3)^alpha)-(tau2^alpha)))-(theta1*(tau1^alpha)))
#z4 <- 1 - exp(-(beta*lambda02*(((2*tau2)^(alpha))-((tau1+tau2)^(alpha))))-(lambda02*((tau2^(alpha))-(tau1^(alpha))))-(beta*lambda01*(((tau2+tau1)^alpha)-(tau2^alpha)))-(lambda01*(tau1^alpha)))
z4 <- 1 - exp(-(theta4*(((tau4)^(alpha))-((tau3)^(alpha))))-(theta2*((tau2^(alpha))-(tau1^(alpha))))-(theta3*(((tau3)^alpha)-((tau2)^alpha)))-(theta1*(tau1^alpha)))

theta1 <- theta11
theta2 <- theta21
theta3 <- theta31
theta4 <- theta41
theta5 <- theta51

replication1 <- 2000

KS_dist1 <- 1:replication1

theta1_est <- 1:replication1
theta2_est <- 1:replication1
theta3_est <- 1:replication1
theta4_est <- 1:replication1
theta5_est <- 1:replication1
KS_dist1 <- 1:replication1

count1 <- 1
while(count1<(replication1+1)){
  
  if(count1%%50==0){
    print(count1)
  }
  
  u1<- runif(n_all[1],0,1)
  v1 <- sort(u1)
  t <- 1:n_all[1] 
  n11 <- 0
  n21 <- 0
  n31 <- 0
  n41 <- 0
  n51 <- 0
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
    if(v1[i]>z2 && v1[i]<=z3)
    {
      t[i] <- -((theta3)*log(1-v1[i]))-((theta3/theta1)*(tau1)) -((theta3/theta2)*(tau2-tau1))  + tau2
      n31 <- n31+1
    }
    if(v1[i]>z3 && v1[i]<=z4)
    {
      t[i] <- -((theta4)*log(1-v1[i]))-((theta4/theta1)*(tau1)) -((theta4/theta2)*(tau2-tau1))   -((theta4/theta3)*(tau3-tau2)) + tau3
      n41 <- n41+1
    }
    if(v1[i]>z4)
    {
      t[i] <- -((theta5)*log(1-v1[i]))-((theta5/theta1)*(tau1)) -((theta5/theta2)*(tau2-tau1))   -((theta5/theta3)*(tau3-tau2)) -((theta5/theta4)*(tau4-tau3)) + tau4
      n51 <- n51+1
    }
  }
  
  n1_data1 <- n11
  n2_data1 <- n21
  n3_data1 <- n31
  n4_data1 <- n41
  n5_data1 <- n51
  data1 <- t
  
  u1<- runif(n_all[2],0,1)
  v1 <- sort(u1)
  t <- 1:n_all[2] 
  n11 <- 0
  n21 <- 0
  n31 <- 0
  n41 <- 0
  n51 <- 0
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
    if(v1[i]>z2 && v1[i]<=z3)
    {
      t[i] <- -((theta3)*log(1-v1[i]))-((theta3/theta1)*(tau1)) -((theta3/theta2)*(tau2-tau1))  + tau2
      n31 <- n31+1
    }
    if(v1[i]>z3 && v1[i]<=z4)
    {
      t[i] <- -((theta4)*log(1-v1[i]))-((theta4/theta1)*(tau1)) -((theta4/theta2)*(tau2-tau1))   -((theta4/theta3)*(tau3-tau2)) + tau3
      n41 <- n41+1
    }
    if(v1[i]>z4)
    {
      t[i] <- -((theta5)*log(1-v1[i]))-((theta5/theta1)*(tau1)) -((theta5/theta2)*(tau2-tau1))   -((theta5/theta3)*(tau3-tau2)) -((theta5/theta4)*(tau4-tau3)) + tau4
      n51 <- n51+1
    }
  }
  
  
  n1_data2 <- n11
  n2_data2 <- n21
  n3_data2 <- n31
  n4_data2 <- n41
  n5_data2 <- n51
  data2 <- t
  
  
  n_S1 <- n1_data1 + n1_data2 
  n_S2 <- n2_data1 + n2_data2 
  n_S3 <- n3_data1 + n3_data2
  n_S4 <- n4_data1 + n4_data2 
  n_S5 <- n5_data1 + n5_data2 
  n1_alldata <- c(n1_data1, n1_data2)
  n2_alldata <- c(n2_data1, n2_data2)
  n3_alldata <- c(n3_data1, n3_data2)
  n4_alldata <- c(n4_data1, n4_data2)
  n5_alldata <- c(n5_data1, n5_data2)
  

  
  all_endtime <- c(data1[r_all[1]], data2[r_all[2]])
  
  D11 <- sum(data1[data1<tau1_all[1]]) + ((n_all[1]-n1_data1)*tau1_all[1])
  D12 <- sum(data2[data2<tau1_all[2]]) + ((n_all[2]-n1_data2)*tau1_all[2])
  D1_sum <- D11+D12
  
  D21 <- sum((data1[data1>tau1_all[1] & data1<tau2_all[1]]-tau1_all[1])) + ((n_all[1]-n1_data1-n2_data1)*(tau2_all[1]-tau1_all[1]))
  D22 <- sum((data2[data2>tau1_all[2] & data2<tau2_all[2]]-tau1_all[2])) + ((n_all[2]-n1_data2-n2_data2)*(tau2_all[2]-tau1_all[2]))
  D2_sum <- D21+D22
  
  D31 <- sum((data1[data1>tau2_all[1] & data1<tau3_all[1]]-tau2_all[1])) + ((n_all[1]-n1_data1-n2_data1-n3_data1)*(tau3_all[1]-tau2_all[1]))
  D32 <- sum((data2[data2>tau2_all[2] & data2<tau3_all[2]]-tau2_all[2])) + ((n_all[2]-n1_data2-n2_data2-n3_data2)*(tau3_all[2]-tau2_all[2]))
  D3_sum <- D31+D32
  
  D41 <- sum((data1[data1>tau3_all[1] & data1<tau4_all[1]]-tau3_all[1])) + ((n_all[1]-n1_data1-n2_data1-n3_data1-n4_data1)*(tau4_all[1]-tau3_all[1]))
  D42 <- sum((data2[data2>tau3_all[2] & data2<tau4_all[2]]-tau3_all[2])) + ((n_all[2]-n1_data2-n2_data2-n3_data2-n4_data2)*(tau4_all[2]-tau3_all[2]))
  D4_sum <- D41+D42
  
  D51 <- sum((data1[data1>tau4_all[1]]-tau4_all[1])) + ((n_all[1]-r_all[1])*(all_endtime[1]-tau4_all[1]))
  D52 <- sum((data2[data2>tau4_all[2]]-tau4_all[2])) + ((n_all[2]-r_all[2])*(all_endtime[2]-tau4_all[2]))
  D5_sum <- D51+D52
  
  
  ## Hyper-parameters
  
  a0 <- 0.0001
  b0 <- 0.0001
  a1 <- 1
  a2 <- 1
  a3 <- 1
  a4 <- 1
  b1 <- 1
  b2 <- 1
  b3 <- 1
  b4 <- 1
  repli <- 5000
  
  
  
  if(n_S1>0 && n_S2>0 && n_S3>0 && n_S4>0 && n_S5>0 ){
    
  
  beta1_est <- rep(0,repli)
  beta2_est <- rep(0,repli)
  beta3_est <- rep(0,repli)
  beta4_est <- rep(0,repli)
  theta1_est <- rep(0,repli)
  theta2_est <- rep(0,repli)
  theta3_est <- rep(0,repli)
  theta4_est <- rep(0,repli)
  theta5_est <- rep(0,repli)
  w1 <- rep(0,repli)
  
  cnt <- 1
  
  while(cnt<repli+1){
    beta1_est[cnt] <- runif(1,0.01,0.99)
    beta2_est[cnt] <- runif(1,0.01,0.99)
    beta3_est[cnt] <- runif(1,0.01,0.99)
    beta4_est[cnt] <- runif(1,0.01,0.99)
    A_bta12 <- D1_sum + (D2_sum/beta1_est[cnt]) + (D3_sum/(beta1_est[cnt]*beta2_est[cnt]))  + (D3_sum/(beta1_est[cnt]*beta2_est[cnt]*beta3_est[cnt])) + (D4_sum/(beta1_est[cnt]*beta2_est[cnt]*beta3_est[cnt]*beta4_est[cnt]))
    
    shape1 <- r + b0
    scale1 <- 1/(a0 + A_bta12)
    scale1
    if(shape1>0 && scale1>0){
      th1 <- rgamma(1,shape=shape1, scale=scale1)
      theta1_est[cnt] <- 1/th1
      theta2_est[cnt] <- beta1_est[cnt]*theta1_est[cnt]
      theta3_est[cnt] <- beta1_est[cnt]*beta2_est[cnt]*theta1_est[cnt]
      theta4_est[cnt] <- beta1_est[cnt]*beta2_est[cnt]*beta3_est[cnt]*theta1_est[cnt]
      theta5_est[cnt] <- beta1_est[cnt]*beta2_est[cnt]*beta3_est[cnt]*beta4_est[cnt]*theta1_est[cnt]
      
      w1[cnt] <- ((a0 + A_bta12)^(-r-b0))*(beta1_est[cnt]^(a1-n_S2-n_S4-n_S3-n_S5-1))*((1-beta1_est[cnt])^(b1-1))*(beta2_est[cnt]^(a2-n_S3-n_S4-n_S5-1))*((1-beta2_est[cnt])^(b2-1))*(beta3_est[cnt]^(a3-n_S4-n_S5-1))*((1-beta3_est[cnt])^(b3-1))*(beta4_est[cnt]^(a4-n_S5-1))*((1-beta4_est[cnt])^(b4-1))
      
      #    w1[cnt] <- ((a0 + A_bta12)^(-r-b0))*(beta1_est[cnt]^(a1-n_S2-n_S3-1))*((1-beta1_est[cnt])^(b1-1))*(beta2_est[cnt]^(a2-n_S3-1))*((1-beta2_est[cnt])^(b2-1))
      cnt <- cnt +1
    }
  }
  if(sum(w1)>0){
    ww <- w1/sum(w1)
    theta1_BE <- sum(ww*theta1_est)
    theta2_BE <- sum(ww*theta2_est)
    theta3_BE <- sum(ww*theta3_est)
    theta4_BE <- sum(ww*theta4_est)
    theta5_BE <- sum(ww*theta5_est)
    
    
    ord_MLE_theta11 <- theta1_BE
    ord_MLE_theta21 <- theta2_BE 
    ord_MLE_theta31 <- theta3_BE 
    ord_MLE_theta41 <- theta4_BE 
    ord_MLE_theta51 <- theta5_BE
    
    ord_MLE_theta1 <- 1/ord_MLE_theta11
    ord_MLE_theta2 <- 1/ord_MLE_theta21
    ord_MLE_theta3 <- 1/ord_MLE_theta31
    ord_MLE_theta4 <- 1/ord_MLE_theta41
    ord_MLE_theta5 <- 1/ord_MLE_theta51
    
    data1 <- c(83.50, 91.00, 91.00, 97.00, 107.00, 109.50, 114.00, 115.41, 128.61, 133.53, 138.58, 140.00, 152.08, 155.10)-80
    
    data2 <- c(91.00, 93.00, 94.00, 98.20, 115.81, 116.00, 116.50, 117.25, 126.75, 127.50, 154.33, 159.50, 164.00, 184.14, 188.33)-80
    
    
    Dataa <- c(data1,data2)
    Dataa <- sort(Dataa)
    
    n <- 29
    
    
    tt <- seq(0,(max(Dataa)+0.2), by=0.1) 
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
    
    KS_dist1[count1] <- max(abs(Emp_cdf-BE_F))
    
    
    count1 <- count1 + 1   
  }
   
  
    
  }
  
  
}

KS_dist_obs <- 0.1294

n_obs_KS <-length(KS_dist1[KS_dist1>KS_dist_obs])
KS_pval <- n_obs_KS/replication1
KS_pval

