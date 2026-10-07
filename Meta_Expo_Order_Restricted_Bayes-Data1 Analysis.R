
##Part for MLE estimates

K = 2 

n_all <- c(14,15)
tau1_all <- c(30,30)
tau2_all <- c(50,50)
tau3_all <- c(70,70)
tau4_all <- c(90,90)
r_all <- c(14,15)
r <- sum(r_all)


tau1_all <- tau1_all/100
tau2_all <- tau2_all/100
tau3_all <- tau3_all/100
tau4_all <- tau4_all/100

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

data1 <- data1/100
data2 <- data2/100

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
     beta1_est[cnt] <- runif(1,0.01,0.995)
     beta2_est[cnt] <- runif(1,0.01,0.995)
     beta3_est[cnt] <- runif(1,0.01,0.995)
     beta4_est[cnt] <- runif(1,0.01,0.995)
     A_bta12 <- D1_sum + (D2_sum/beta1_est[cnt]) + (D3_sum/(beta1_est[cnt]*beta2_est[cnt]))  + (D4_sum/(beta1_est[cnt]*beta2_est[cnt]*beta3_est[cnt])) + (D5_sum/(beta1_est[cnt]*beta2_est[cnt]*beta3_est[cnt]*beta4_est[cnt]))
     
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
  
 # burn <- 500
#  repli <- repli1-burn

  if(sum(w1)>0){
    
    # Construction of Symmetric CRI
    
    ww <- w1/sum(w1)
    coeff_95 <- 0.01
    
    
    ## CI for Theta1
  #  aa<- matrix(c(theta5_est,w1), nrow=length(w1), ncol=2, byrow=FALSE)
  #  bb <-aa[order(aa[, 2]), ]
  #  cc<-bb[1:repli,]
  #  cc[,2] <- cc[,2]/sum(cc[,2])
    
 #   repli <- repli1
    
    theta1_w1 <- matrix(c(theta1_est,ww), nrow=length(ww), ncol=2, byrow=FALSE)
    theta1_w1_sort <- theta1_w1[order(theta1_w1[, 1]), ]
    
    w1_sum_L_theta1 <- 0 
    i <- 1
    while(w1_sum_L_theta1<(coeff_95/2)){
      w1_sum_L_theta1 <- w1_sum_L_theta1+ theta1_w1_sort[i,2]
      i <- i+1
    }
    w1_sum_L_theta1 <- w1_sum_L_theta1 - theta1_w1_sort[i-1,2]
    L_theta1_sys_95 = theta1_w1_sort[i-1,1]
    
    w1_sum_U_theta1 <- 0 
    i=repli
    while(w1_sum_U_theta1<(coeff_95/2)){
      w1_sum_U_theta1 <- w1_sum_U_theta1 +  theta1_w1_sort[i,2]
      i <- i-1
    }
    
    w1_sum_U_theta1 = w1_sum_U_theta1 - theta1_w1_sort[i+1,2]
    U_theta1_sys_95 = theta1_w1_sort[i+1,1]
   
    
    ## CI for Theta2
    theta2_w1 <- matrix(c(theta2_est,ww), nrow=length(ww), ncol=2, byrow=FALSE)
    theta2_w1_sort <- theta2_w1[order(theta2_w1[, 1]), ]
    
    w1_sum_L_theta2 <- 0 
    i <- 1
    while(w1_sum_L_theta2<(coeff_95/2)){
      w1_sum_L_theta2 <- w1_sum_L_theta2+ theta2_w1_sort[i,2]
      i <- i+1
    }
    w1_sum_L_theta2 <- w1_sum_L_theta2 - theta2_w1_sort[i-1,2]
    L_theta2_sys_95 = theta2_w1_sort[i-1,1]
    
    w1_sum_U_theta2 <- 0 
    i=repli
    while(w1_sum_U_theta2<(coeff_95/2)){
      w1_sum_U_theta2 <- w1_sum_U_theta2 +  theta2_w1_sort[i,2]
      i <- i-1
    }
    
    w1_sum_U_theta2 = w1_sum_U_theta2 - theta2_w1_sort[i+1,2]
    U_theta2_sys_95 = theta2_w1_sort[i+1,1]
    
    ## CI for Theta3
    theta3_w1 <- matrix(c(theta3_est,ww), nrow=length(ww), ncol=2, byrow=FALSE)
    theta3_w1_sort <- theta3_w1[order(theta3_w1[, 1]), ]
    
    w1_sum_L_theta3 <- 0 
    i <- 1
    while(w1_sum_L_theta3<(coeff_95/2)){
      w1_sum_L_theta3 <- w1_sum_L_theta3+ theta3_w1_sort[i,2]
      i <- i+1
    }
    w1_sum_L_theta3 <- w1_sum_L_theta3 - theta3_w1_sort[i-1,2]
    L_theta3_sys_95 = theta3_w1_sort[i-1,1]
    
    w1_sum_U_theta3 <- 0 
    i=repli
    while(w1_sum_U_theta3<(coeff_95/2)){
      w1_sum_U_theta3 <- w1_sum_U_theta3 +  theta3_w1_sort[i,2]
      i <- i-1
    }
    
    w1_sum_U_theta3 = w1_sum_U_theta3 - theta3_w1_sort[i+1,2]
    U_theta3_sys_95 = theta3_w1_sort[i+1,1]

    ## CI for Theta4
    theta4_w1 <- matrix(c(theta4_est,ww), nrow=length(ww), ncol=2, byrow=FALSE)
    theta4_w1_sort <- theta4_w1[order(theta4_w1[, 1]), ]
    
    w1_sum_L_theta4 <- 0 
    i <- 1
    while(w1_sum_L_theta4<(coeff_95/2)){
      w1_sum_L_theta4 <- w1_sum_L_theta4+ theta4_w1_sort[i,2]
      i <- i+1
    }
    w1_sum_L_theta4 <- w1_sum_L_theta4 - theta4_w1_sort[i-1,2]
    L_theta4_sys_95 = theta4_w1_sort[i-1,1]
    
    w1_sum_U_theta4 <- 0 
    i=repli
    while(w1_sum_U_theta4<(coeff_95/2)){
      w1_sum_U_theta4 <- w1_sum_U_theta4 +  theta4_w1_sort[i,2]
      i <- i-1
    }
    
    w1_sum_U_theta4 = w1_sum_U_theta4 - theta4_w1_sort[i+1,2]
    U_theta4_sys_95 = theta4_w1_sort[i+1,1]
    
    ## CI for Theta5
    theta5_w1 <- matrix(c(theta5_est,ww), nrow=length(ww), ncol=2, byrow=FALSE)
    theta5_w1_sort <- theta5_w1[order(theta5_w1[, 1]), ]
    
    w1_sum_L_theta5 <- 0 
    i <- 1
    while(w1_sum_L_theta5<(coeff_95/2)){
      w1_sum_L_theta5 <- w1_sum_L_theta5+ theta5_w1_sort[i,2]
      i <- i+1
    }
    w1_sum_L_theta5 <- w1_sum_L_theta5 - theta5_w1_sort[i-1,2]
    L_theta5_sys_95 = theta5_w1_sort[i-1,1]
    
    w1_sum_U_theta5 <- 0 
    i=repli
    while(w1_sum_U_theta5<(coeff_95/2)){
      w1_sum_U_theta5 <- w1_sum_U_theta5 +  theta5_w1_sort[i,2]
      i <- i-1
    }
    
    w1_sum_U_theta5 = w1_sum_U_theta5 - theta5_w1_sort[i+1,2]
    U_theta5_sys_95 = theta5_w1_sort[i+1,1]
    
       
    # HPD CI of Theta1
    last_sum <- 0
    i <- repli
    while(last_sum<(1-coeff_95)){
      last_sum <- last_sum + theta1_w1_sort[i,2]
      i <- i-1
    }
    L_theta1 <- 1:(i+1)
    U_theta1 <- 1:(i+1) 
    interval_length_theta1 <- 1:(i+1) 
    for (j in 1:(i+1)){
      k<-j 
      w1_interval_sum <- 0 
      while (w1_interval_sum<(1-coeff_95)){
        w1_interval_sum = w1_interval_sum + theta1_w1_sort[k,2]
        k = k+1
      }
      L_theta1[j] = theta1_w1_sort[j,1] ;
      U_theta1[j] = theta1_w1_sort[k-1,1] ;
      interval_length_theta1[j] =  U_theta1[j] -  L_theta1[j] 
    }
    
    L_theta1_HPD_95 <-  L_theta1[which.min(interval_length_theta1)]
    U_theta1_HPD_95 <-  U_theta1[which.min(interval_length_theta1)]
   
    
    # HPD CI of Theta2
    last_sum <- 0
    i <- repli
    while(last_sum<(1-coeff_95)){
      last_sum <- last_sum + theta2_w1_sort[i,2]
      i <- i-1
    }
    L_theta2 <- 1:(i+1)
    U_theta2 <- 1:(i+1) 
    interval_length_theta2 <- 1:(i+1) 
    for (j in 1:(i+1)){
      k<-j 
      w1_interval_sum <- 0 
      while (w1_interval_sum<(1-coeff_95)){
        w1_interval_sum = w1_interval_sum + theta2_w1_sort[k,2]
        k = k+1
      }
      L_theta2[j] = theta2_w1_sort[j,1] ;
      U_theta2[j] = theta2_w1_sort[k-1,1] ;
      interval_length_theta2[j] =  U_theta2[j] -  L_theta2[j] 
    }
    
    L_theta2_HPD_95 <-  L_theta2[which.min(interval_length_theta2)]
    U_theta2_HPD_95 <-  U_theta2[which.min(interval_length_theta2)]
    
    # HPD CI of Theta3
    last_sum <- 0
    i <- repli
    while(last_sum<(1-coeff_95)){
      last_sum <- last_sum + theta3_w1_sort[i,2]
      i <- i-1
    }
    L_theta3 <- 1:(i+1)
    U_theta3 <- 1:(i+1) 
    interval_length_theta3 <- 1:(i+1) 
    for (j in 1:(i+1)){
      k<-j 
      w1_interval_sum <- 0 
      while (w1_interval_sum<(1-coeff_95)){
        w1_interval_sum = w1_interval_sum + theta3_w1_sort[k,2]
        k = k+1
      }
      L_theta3[j] = theta3_w1_sort[j,1] ;
      U_theta3[j] = theta3_w1_sort[k-1,1] ;
      interval_length_theta3[j] =  U_theta3[j] -  L_theta3[j] 
    }
    
    L_theta3_HPD_95 <-  L_theta3[which.min(interval_length_theta3)]
    U_theta3_HPD_95 <-  U_theta3[which.min(interval_length_theta3)]
   
    # HPD CI of Theta4
    last_sum <- 0
    i <- repli
    while(last_sum<(1-coeff_95)){
      last_sum <- last_sum + theta4_w1_sort[i,2]
      i <- i-1
    }
    L_theta4 <- 1:(i+1)
    U_theta4 <- 1:(i+1) 
    interval_length_theta4 <- 1:(i+1) 
    for (j in 1:(i+1)){
      k<-j 
      w1_interval_sum <- 0 
      while (w1_interval_sum<(1-coeff_95)){
        w1_interval_sum = w1_interval_sum + theta4_w1_sort[k,2]
        k = k+1
      }
      L_theta4[j] = theta4_w1_sort[j,1] ;
      U_theta4[j] = theta4_w1_sort[k-1,1] ;
      interval_length_theta4[j] =  U_theta4[j] -  L_theta4[j] 
    }
    
    L_theta4_HPD_95 <-  L_theta4[which.min(interval_length_theta4)]
    U_theta4_HPD_95 <-  U_theta4[which.min(interval_length_theta4)]
    
    # HPD CI of Theta5
    last_sum <- 0
    i <- repli
    while(last_sum<(1-coeff_95)){
      last_sum <- last_sum + theta5_w1_sort[i,2]
      i <- i-1
    }
    L_theta5 <- 1:(i+1)
    U_theta5 <- 1:(i+1) 
    interval_length_theta5 <- 1:(i+1) 
    for (j in 1:(i+1)){
      k<-j 
      w1_interval_sum <- 0 
      while (w1_interval_sum<(1-coeff_95)){
        w1_interval_sum = w1_interval_sum + theta5_w1_sort[k,2]
        k = k+1
      }
      L_theta5[j] = theta5_w1_sort[j,1] ;
      U_theta5[j] = theta5_w1_sort[k-1,1] ;
      interval_length_theta5[j] =  U_theta5[j] -  L_theta5[j] 
    }
    
    L_theta5_HPD_95 <-  L_theta5[which.min(interval_length_theta5)]
    U_theta5_HPD_95 <-  U_theta5[which.min(interval_length_theta5)]
    
    theta1_BE <- sum(ww*theta1_est)
    theta2_BE <- sum(ww*theta2_est)
    theta3_BE <- sum(ww*theta3_est)
    theta4_BE <- sum(ww*theta4_est)
    theta5_BE <- sum(ww*theta5_est)

  }

  Ordered_Estimates_BE <- c(theta1_BE,theta2_BE,theta3_BE,theta4_BE,theta5_BE)
  Ordered_Sym_CI_95 <- c(L_theta1_sys_95,U_theta1_sys_95,L_theta2_sys_95,U_theta2_sys_95,L_theta3_sys_95,U_theta3_sys_95,L_theta4_sys_95,U_theta4_sys_95,L_theta5_sys_95,U_theta5_sys_95)
  Ordered_HPD_CI_95 <- c(L_theta1_HPD_95,U_theta1_HPD_95,L_theta2_HPD_95,U_theta2_HPD_95,L_theta3_HPD_95,U_theta3_HPD_95,L_theta4_HPD_95,U_theta4_HPD_95,L_theta5_HPD_95,U_theta5_HPD_95)
  
  Ordered_Estimates_BE
  Ordered_Sym_CI_95
  Ordered_HPD_CI_95

