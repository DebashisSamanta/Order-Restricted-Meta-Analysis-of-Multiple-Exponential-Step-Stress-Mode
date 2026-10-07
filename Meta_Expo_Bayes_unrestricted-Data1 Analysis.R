


##Part for MLE estimates

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

a1 <- 0.0001
b1 <- 0.0001
a2 <- 0.0001
b2 <- 0.0001
a3 <- 0.0001
b3 <- 0.0001
a4 <- 0.0001
b4 <- 0.0001
a5 <- 0.0001
b5 <- 0.0001
repli <- 5000

#a1<-10
#a2<- 9
#a3 <- 3 
#a4 <- 5 
#a5 <- 2
#b1 <-  0.1378327
#b2 <- 0.2814161
#b3<- 0.05551786
#b4 <- 0.3332889
#b5 <- 0.1231906



  
  #theta1_BE <- (D1_sum+a1)/(n_S1+b1-1)
  #theta2_BE <- (D2_sum+a2)/(n_S2+b2-1)
  #theta3_BE <- (D3_sum+a3)/(n_S3+b3-1)
  #theta4_BE <- (D4_sum+a4)/(n_S4+b4-1)
  #theta5_BE <- (D5_sum+a5)/(n_S5+b5-1)
  
theta1_BE <- (D1_sum+a1)/(n_S1+b1)
theta2_BE <- (D2_sum+a2)/(n_S2+b2)
theta3_BE <- (D3_sum+a3)/(n_S3+b3)
theta4_BE <- (D4_sum+a4)/(n_S4+b4)
theta5_BE <- (D5_sum+a5)/(n_S5+b5)



  v11 <- ((D1_sum+a1)^2)/(((n_S1+b1-1)^2)*(n_S1+b1-2))
  v22 <- ((D2_sum+a2)^2)/(((n_S2+b2-1)^2)*(n_S2+b2-2))
  v33 <- ((D3_sum+a3)^2)/(((n_S3+b3-1)^2)*(n_S3+b3-2))
  v44 <- ((D4_sum+a4)^2)/(((n_S4+b4-1)^2)*(n_S4+b4-2))
  v55 <- ((D5_sum+a5)^2)/(((n_S5+b5-1)^2)*(n_S5+b5-2))
  
  coeff <- 0.1
  
  shape1 <- n_S1+b1
  scale1 <- 1/(D1_sum+a1)
  L_th1 <- qgamma((coeff/2),shape=shape1, scale=scale1)
  U_th1 <- qgamma((1-(coeff/2)),shape=shape1, scale=scale1)
  U_theta1_sys_95 <- 1/L_th1
  L_theta1_sys_95 <- 1/U_th1
 
  
  shape2 <- n_S2+b2
  scale2 <- 1/(D2_sum+a2)
  L_th2 <- qgamma((coeff/2),shape=shape2, scale=scale2)
  U_th2 <- qgamma((1-(coeff/2)),shape=shape2, scale=scale2)
  U_theta2_sys_95 <- 1/L_th2
  L_theta2_sys_95 <- 1/U_th2
  
  
  
  shape3 <- n_S3+b3
  scale3 <- 1/(D3_sum+a3)
  L_th3 <- qgamma((coeff/2),shape=shape3, scale=scale3)
  U_th3 <- qgamma((1-(coeff/2)),shape=shape3, scale=scale3)
  U_theta3_sys_95 <- 1/L_th3
  L_theta3_sys_95 <- 1/U_th3
 
  
  shape4 <- n_S4+b4
  scale4 <- 1/(D4_sum+a4)
  L_th4 <- qgamma((coeff/2),shape=shape4, scale=scale4)
  U_th4 <- qgamma((1-(coeff/2)),shape=shape4, scale=scale4)
  U_theta4_sys_95 <- 1/L_th4
  L_theta4_sys_95 <- 1/U_th4
  
  
  shape5 <- n_S5+b5
  scale5 <- 1/(D5_sum+a5)
  L_th5 <- qgamma((coeff/2),shape=shape5, scale=scale5)
  U_th5 <- qgamma((1-(coeff/2)),shape=shape5, scale=scale5)
  U_theta5_sys_95 <- 1/L_th5
  L_theta5_sys_95 <- 1/U_th5
  
  qq <- seq(0.0001,0.0999,by=0.0001)
  
  interval_length_theta1 <- 1:length(qq)
  L_theta1 <- 1:length(qq)
  U_theta1 <- 1:length(qq)
  
  for(i in 1:length(qq)){
    L_th1 <- qgamma((qq[i]),shape=shape1, scale=scale1)
    U_th1 <- qgamma((1-(coeff-qq[i])),shape=shape1, scale=scale1)
    L_theta1[i] <- 1/U_th1
    U_theta1[i] <- 1/L_th1
    interval_length_theta1[i] <- U_theta1[i] - L_theta1[i] 
  }
  
L_theta1_HPD_95 <-  L_theta1[which.min(interval_length_theta1)]
U_theta1_HPD_95 <-  U_theta1[which.min(interval_length_theta1)]
 

interval_length_theta2 <- 1:length(qq)
L_theta2 <- 1:length(qq)
U_theta2 <- 1:length(qq)

for(i in 1:length(qq)){
  L_th2 <- qgamma((qq[i]),shape=shape2, scale=scale2)
  U_th2 <- qgamma((1-(coeff-qq[i])),shape=shape2, scale=scale2)
  L_theta2[i] <- 1/U_th2
  U_theta2[i] <- 1/L_th2
  interval_length_theta2[i] <- U_theta2[i] - L_theta2[i] 
}

L_theta2_HPD_95 <-  L_theta2[which.min(interval_length_theta2)]
U_theta2_HPD_95 <-  U_theta2[which.min(interval_length_theta2)]


interval_length_theta3 <- 1:length(qq)
L_theta3 <- 1:length(qq)
U_theta3 <- 1:length(qq)

for(i in 1:length(qq)){
  L_th3 <- qgamma((qq[i]),shape=shape3, scale=scale3)
  U_th3 <- qgamma((1-(coeff-qq[i])),shape=shape3, scale=scale3)
  L_theta3[i] <- 1/U_th3
  U_theta3[i] <- 1/L_th3
  interval_length_theta3[i] <- U_theta3[i] - L_theta3[i] 
}

L_theta3_HPD_95 <-  L_theta3[which.min(interval_length_theta3)]
U_theta3_HPD_95 <-  U_theta3[which.min(interval_length_theta3)]

  
interval_length_theta4 <- 1:length(qq)
L_theta4 <- 1:length(qq)
U_theta4 <- 1:length(qq)

for(i in 1:length(qq)){
  L_th4 <- qgamma((qq[i]),shape=shape4, scale=scale4)
  U_th4 <- qgamma((1-(coeff-qq[i])),shape=shape4, scale=scale4)
  L_theta4[i] <- 1/U_th4
  U_theta4[i] <- 1/L_th4
  interval_length_theta4[i] <- U_theta4[i] - L_theta4[i] 
}

L_theta4_HPD_95 <-  L_theta4[which.min(interval_length_theta4)]
U_theta4_HPD_95 <-  U_theta4[which.min(interval_length_theta4)]


interval_length_theta5 <- 1:length(qq)
L_theta5 <- 1:length(qq)
U_theta5 <- 1:length(qq)

for(i in 1:length(qq)){
  L_th5 <- qgamma((qq[i]),shape=shape5, scale=scale5)
  U_th5 <- qgamma((1-(coeff-qq[i])),shape=shape5, scale=scale5)
  L_theta5[i] <- 1/U_th5
  U_theta5[i] <- 1/L_th5
  interval_length_theta5[i] <- U_theta5[i] - L_theta5[i] 
}

L_theta5_HPD_95 <-  L_theta5[which.min(interval_length_theta5)]
U_theta5_HPD_95 <-  U_theta5[which.min(interval_length_theta5)]



unres_BE <- c(theta1_BE,theta2_BE,theta3_BE,theta4_BE,theta5_BE)
Ordered_Sym_CI_95 <- c(L_theta1_sys_95,U_theta1_sys_95,L_theta2_sys_95,U_theta2_sys_95,L_theta3_sys_95,U_theta3_sys_95,L_theta4_sys_95,U_theta4_sys_95,L_theta5_sys_95,U_theta5_sys_95)
Ordered_HPD_CI_95 <- c(L_theta1_HPD_95,U_theta1_HPD_95,L_theta2_HPD_95,U_theta2_HPD_95,L_theta3_HPD_95,U_theta3_HPD_95,L_theta4_HPD_95,U_theta4_HPD_95,L_theta5_HPD_95,U_theta5_HPD_95)


unres_BE
Ordered_Sym_CI_95
Ordered_HPD_CI_95



