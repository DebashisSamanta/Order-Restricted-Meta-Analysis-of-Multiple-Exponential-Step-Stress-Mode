

##Part for MLE estimates

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
  
  D51 <- sum((data1[data1>tau4_all[1]]-tau4_all[1])) + ((n_all[1]-r_all[1])*(all_endtime[1]-tau2_all[1]))
  D52 <- sum((data2[data2>tau4_all[2]]-tau4_all[2])) + ((n_all[2]-r_all[2])*(all_endtime[2]-tau2_all[2]))
  D5_sum <- D51+D52
  
  unres_theta1_hat <- D1_sum/n_S1
  unres_theta2_hat <- D2_sum/n_S2
  unres_theta3_hat <- D3_sum/n_S3
  unres_theta4_hat <- D4_sum/n_S4
  unres_theta5_hat <- D5_sum/n_S5
  
  theta1_star <- D1_sum/n_S1
  beta1_star <- D2_sum/(n_S2*theta1_star)
  beta2_star <- D3_sum/(n_S3*beta1_star*theta1_star)
  beta3_star <- D4_sum/(n_S4*beta1_star*beta2_star*theta1_star)
  beta4_star <- D5_sum/(n_S5*beta1_star*beta2_star*beta3_star*theta1_star)
  
  if(beta2_star>1 && beta4_star>1){
    beta2_star <- 1
    beta4_star <- 1
    theta1_star <- (D1_sum)/(n_S1)
    beta1_star <- (D2_sum+D3_sum)/((n_S2+n_S3)*theta1_star)
    beta3_star <- (D4_sum+D5_sum)/((n_S4+n_S5)*beta1_star*beta2_star*theta1_star)
   # beta4_star <- D5_sum/(n_S5*beta1_star*beta2_star*beta3_star*theta1_star)
  }
  
  beta1_hat <- beta1_star
  beta2_hat <- beta2_star
  beta3_hat <- beta3_star
  beta4_hat <- beta4_star
  
  theta1_hat <- theta1_star
  theta2_hat <- theta1_hat*beta1_star
  theta3_hat <- theta1_hat*beta1_star*beta2_star
  theta4_hat <- theta1_hat*beta1_star*beta2_star*beta3_star
  theta5_hat <- theta1_hat*beta1_star*beta2_star*beta3_star*beta4_star


   # Unrestricted FI
   h11 <- n_S1/((unres_theta1_hat)^2)
   h22 <- n_S2/((unres_theta2_hat)^2)
   h33 <- n_S3/((unres_theta3_hat)^2)
   h44 <- n_S4/((unres_theta4_hat)^2)
   h55 <- n_S5/((unres_theta5_hat)^2)
   
   
   h12 <- h21 <- h31 <-h13 <- h14<- h41 <- h15<- h51 <- 0
   h32 <- h23 <- h24 <-h42 <- h25<- h52  <- 0
   h34 <-h43 <- h35<- h53  <- 0
   h54<- h45  <- 0
   unres_Fisher_information <- c(h11, h12, h13,h14,h15,  h21, h22, h23,h24,h25, h31, h32, h33,h34,h35,h41, h42, h43,h44,h45, h51, h52, h53,h54,h55)
   
   unres_Fisher_information_mat <- matrix(unres_Fisher_information, nrow=5, ncol=5, byrow=TRUE)
   unres_Dispersion_matrix <- solve(unres_Fisher_information_mat)
   
   unres_Asymp_LL_theta1_90 <- max(0,unres_theta1_hat - 1.64*sqrt(unres_Dispersion_matrix[1,1]))
   unres_Asymp_UL_theta1_90 <- unres_theta1_hat + 1.64*sqrt(unres_Dispersion_matrix[1,1])
   unres_Asymp_LL_theta2_90 <- max(0,unres_theta2_hat - 1.64*sqrt(unres_Dispersion_matrix[2,2]))
   unres_Asymp_UL_theta2_90 <- unres_theta2_hat + 1.64*sqrt(unres_Dispersion_matrix[2,2])
   unres_Asymp_LL_theta3_90 <- max(0,unres_theta3_hat - 1.64*sqrt(unres_Dispersion_matrix[3,3]))
   unres_Asymp_UL_theta3_90 <- unres_theta3_hat + 1.64*sqrt(unres_Dispersion_matrix[3,3])
   unres_Asymp_LL_theta4_90 <- max(0,unres_theta4_hat - 1.64*sqrt(unres_Dispersion_matrix[4,4]))
   unres_Asymp_UL_theta4_90 <- unres_theta4_hat + 1.64*sqrt(unres_Dispersion_matrix[4,4])
   unres_Asymp_LL_theta5_90 <- max(0,unres_theta5_hat - 1.64*sqrt(unres_Dispersion_matrix[5,5]))
   unres_Asymp_UL_theta5_90 <- unres_theta5_hat + 1.64*sqrt(unres_Dispersion_matrix[5,5])
   
   unres_Asymp_LL_theta1_95 <- max(0,unres_theta1_hat - 1.96*sqrt(unres_Dispersion_matrix[1,1]))
   unres_Asymp_UL_theta1_95 <- unres_theta1_hat + 1.96*sqrt(unres_Dispersion_matrix[1,1])
   unres_Asymp_LL_theta2_95 <- max(0,unres_theta2_hat - 1.96*sqrt(unres_Dispersion_matrix[2,2]))
   unres_Asymp_UL_theta2_95 <- unres_theta2_hat + 1.96*sqrt(unres_Dispersion_matrix[2,2])
   unres_Asymp_LL_theta3_95 <- max(0,unres_theta3_hat - 1.96*sqrt(unres_Dispersion_matrix[3,3]))
   unres_Asymp_UL_theta3_95 <- unres_theta3_hat + 1.96*sqrt(unres_Dispersion_matrix[3,3])
   unres_Asymp_LL_theta4_95 <- max(0,unres_theta4_hat - 1.96*sqrt(unres_Dispersion_matrix[4,4]))
   unres_Asymp_UL_theta4_95 <- unres_theta4_hat + 1.96*sqrt(unres_Dispersion_matrix[4,4])
   unres_Asymp_LL_theta5_95 <- max(0,unres_theta5_hat - 1.96*sqrt(unres_Dispersion_matrix[5,5]))
   unres_Asymp_UL_theta5_95 <- unres_theta5_hat + 1.96*sqrt(unres_Dispersion_matrix[5,5])
   
   unres_Asymp_LL_theta1_99 <- max(0,unres_theta1_hat - 2.57*sqrt(unres_Dispersion_matrix[1,1]))
   unres_Asymp_UL_theta1_99 <- unres_theta1_hat + 2.57*sqrt(unres_Dispersion_matrix[1,1])
   unres_Asymp_LL_theta2_99 <- max(0,unres_theta2_hat - 2.57*sqrt(unres_Dispersion_matrix[2,2]))
   unres_Asymp_UL_theta2_99 <- unres_theta2_hat + 2.57*sqrt(unres_Dispersion_matrix[2,2])
   unres_Asymp_LL_theta3_99 <- max(0,unres_theta3_hat - 2.57*sqrt(unres_Dispersion_matrix[3,3]))
   unres_Asymp_UL_theta3_99 <- unres_theta3_hat + 2.57*sqrt(unres_Dispersion_matrix[3,3])
   unres_Asymp_LL_theta4_99 <- max(0,unres_theta4_hat - 2.57*sqrt(unres_Dispersion_matrix[4,4]))
   unres_Asymp_UL_theta4_99 <- unres_theta4_hat + 2.57*sqrt(unres_Dispersion_matrix[4,4])
   unres_Asymp_LL_theta5_99 <- max(0,unres_theta5_hat - 2.57*sqrt(unres_Dispersion_matrix[5,5]))
   unres_Asymp_UL_theta5_99 <- unres_theta5_hat + 2.57*sqrt(unres_Dispersion_matrix[5,5])

   

   
   # Elements of Fisher Information matrix
   f11 <- (n_S1+n_S2+n_S3+n_S4+n_S5)/(theta1_hat^2) - ((2/theta1_hat^3)*(D1_sum + (D2_sum/beta1_hat) +  (D3_sum/(beta1_hat*beta2_hat))+  (D4_sum/(beta1_hat*beta2_hat*beta3_hat))+  (D5_sum/(beta1_hat*beta2_hat*beta3_hat*beta4_hat))))
   f12 <- f21<- - (1/theta1_hat^2)*( (D2_sum/(beta1_hat^2)) +  (D3_sum/((beta1_hat^2)*beta2_hat))+  (D4_sum/((beta1_hat^2)*beta2_hat*beta3_hat))+  (D5_sum/((beta1_hat^2)*beta2_hat*beta3_hat*beta4_hat)))
   f13 <- f31<- - (1/theta1_hat^2)*(  (D3_sum/((beta2_hat^2)*beta1_hat))+  (D4_sum/((beta2_hat^2)*beta1_hat*beta3_hat))+  (D5_sum/((beta2_hat^2)*beta1_hat*beta2_hat*beta3_hat)))
   f14 <- f41<- - (1/theta1_hat^2)*(  (D4_sum/((beta3_hat^2)*beta1_hat*beta2_hat))+  (D5_sum/((beta3_hat^2)*beta1_hat*beta2_hat*beta4_hat)))
   f15 <- f51<- - (1/theta1_hat^2)*(   (D5_sum/((beta4_hat^2)*beta1_hat*beta2_hat*beta3_hat)))
   
  
   f22 <- (n_S2+n_S3+n_S4+n_S5)/(beta1_hat^2) - ((2/(theta1_hat*(beta1_hat^3)))*(D2_sum + (D3_sum/beta2_hat)+ (D4_sum/(beta2_hat*beta3_hat))+ (D5_sum/(beta2_hat*beta3_hat*beta4_hat))))
   f23 <- f32 <- - (1/((theta1_hat)*(beta2_hat^2)*((beta1_hat)^2)))*(D3_sum+ (D4_sum/(beta3_hat))+ (D5_sum/(beta3_hat*beta4_hat)))
   f24 <- f42 <- - (1/((theta1_hat)*(beta3_hat^2)*((beta1_hat)^2)))*((D4_sum/(beta2_hat))+ (D5_sum/(beta2_hat*beta4_hat)))
   f25 <- f52 <- - (1/((theta1_hat)*(beta4_hat^2)*((beta1_hat)^2)))*( (D5_sum/(beta2_hat*beta3_hat)))
   
   f33 <- (n_S3+n_S4+n_S5)/(beta2_hat^2) - ((2)/(theta1_hat*beta1_hat*(beta2_hat^3)))*(D3_sum+ (D4_sum/(beta3_hat))+ (D5_sum/(beta3_hat*beta4_hat)))
   f34 <- f43 <- - (1/((theta1_hat)*(beta3_hat^2)*((beta2_hat)^2)*beta1_hat))*((D4_sum)+ (D5_sum/(beta4_hat)))
   f35 <- f53 <- - (1/((theta1_hat)*(beta4_hat^2)*((beta2_hat)^2)))*( (D5_sum/(beta1_hat*beta3_hat)))
   
   f44 <- (n_S4+n_S5)/(beta3_hat^2) - ((2)/(theta1_hat*beta1_hat*beta2_hat*(beta3_hat^3)))*(D4_sum+ (D5_sum/(beta4_hat)))
   f45 <-f54 <-  - ((1)/(theta1_hat*beta1_hat*beta2_hat*(beta3_hat^2)))*((D5_sum/(beta4_hat^2)))
   
   f55 <- (n_S5)/(beta4_hat^2) - ((2)/(theta1_hat*beta1_hat*beta2_hat*(beta4_hat^3)))*((D5_sum/(beta3_hat)))
   
   Fisher_information <- c(f11, f12, f13,f14,f15,  f21, f22, f23,f24,f25, f31, f32, f33,f34,f35,f41, f42, f43,f44,f45, f51, f52, f53,f54,f55)
   
   
   Fisher_information_mat <- - matrix(Fisher_information, nrow=5, ncol=5, byrow=TRUE)
   
   g11 <- 1
   g12 <- g13 <- g14 <- g15 <-0
   g21 <- beta1_hat
   g22 <- theta1_hat
   g23 <- g24 <- g25 <- 0
   
   
   g31 <- beta1_hat*beta2_hat
   g32 <- theta1_hat*beta2_hat
   g33 <- beta1_hat*theta1_hat
   g34 <- g35 <- 0
   
   g41 <- beta1_hat*beta2_hat*beta3_hat
   g42 <- theta1_hat*beta2_hat*beta3_hat
   g43 <- beta1_hat*theta1_hat*beta3_hat
   g44 <- beta1_hat*theta1_hat*beta2_hat
   g45 <- 0
   
   g51 <- beta1_hat*beta2_hat*beta3_hat*beta4_hat
   g52 <- theta1_hat*beta2_hat*beta3_hat*beta4_hat
   g53 <- beta1_hat*theta1_hat*beta3_hat*beta4_hat
   g54 <- beta1_hat*theta1_hat*beta2_hat*beta4_hat
   g55 <- beta1_hat*theta1_hat*beta2_hat*beta3_hat
   
   Gradian <-  c(g11, g12, g13,g14,g15,  g21, g22, g23,g24,g25, g31, g32, g33,g34,g35,g41, g42, g43,g44,g45, g51, g52, g53,g54,g55)
   
   Grad_mat <-matrix(Gradian, nrow=5, ncol=5, byrow=TRUE)
   Grad_mat_transpose <- t(Grad_mat)
   
   Dispersion_matrix_beta <- solve(Fisher_information_mat)
   
   Dispersion_matrix <- Grad_mat%*%Dispersion_matrix_beta%*%Grad_mat_transpose
   
   Asymp_LL_theta1_90 <- max(0,theta1_hat - 1.64*sqrt(Dispersion_matrix[1,1]))
   Asymp_UL_theta1_90 <- theta1_hat + 1.64*sqrt(Dispersion_matrix[1,1])
   Asymp_LL_theta2_90 <- max(0,theta2_hat - 1.64*sqrt(Dispersion_matrix[2,2]))
   Asymp_UL_theta2_90 <- theta2_hat + 1.64*sqrt(Dispersion_matrix[2,2])
   Asymp_LL_theta3_90 <- max(0,theta3_hat - 1.64*sqrt(Dispersion_matrix[3,3]))
   Asymp_UL_theta3_90 <- theta3_hat + 1.64*sqrt(Dispersion_matrix[3,3])
   Asymp_LL_theta4_90 <- max(0,theta4_hat - 1.64*sqrt(Dispersion_matrix[4,4]))
   Asymp_UL_theta4_90 <- theta4_hat + 1.64*sqrt(Dispersion_matrix[4,4])
   Asymp_LL_theta5_90 <- max(0,theta5_hat - 1.64*sqrt(Dispersion_matrix[5,5]))
   Asymp_UL_theta5_90 <- theta5_hat + 1.64*sqrt(Dispersion_matrix[5,5])
   
   
   Asymp_LL_theta1_95 <- max(0,theta1_hat - 1.96*sqrt(Dispersion_matrix[1,1]))
   Asymp_UL_theta1_95 <- theta1_hat + 1.96*sqrt(Dispersion_matrix[1,1])
   Asymp_LL_theta2_95 <- max(0,theta2_hat - 1.96*sqrt(Dispersion_matrix[2,2]))
   Asymp_UL_theta2_95 <- theta2_hat + 1.96*sqrt(Dispersion_matrix[2,2])
   Asymp_LL_theta3_95 <- max(0,theta3_hat - 1.96*sqrt(Dispersion_matrix[3,3]))
   Asymp_UL_theta3_95 <- theta3_hat + 1.96*sqrt(Dispersion_matrix[3,3])
   Asymp_LL_theta4_95 <- max(0,theta4_hat - 1.96*sqrt(Dispersion_matrix[4,4]))
   Asymp_UL_theta4_95 <- theta4_hat + 1.96*sqrt(Dispersion_matrix[4,4])
   Asymp_LL_theta5_95 <- max(0,theta5_hat - 1.96*sqrt(Dispersion_matrix[5,5]))
   Asymp_UL_theta5_95 <- theta5_hat + 1.96*sqrt(Dispersion_matrix[5,5])
   
   
   Asymp_LL_theta1_99 <- max(0,theta1_hat - 2.57*sqrt(Dispersion_matrix[1,1]))
   Asymp_UL_theta1_99 <- theta1_hat + 2.57*sqrt(Dispersion_matrix[1,1])
   Asymp_LL_theta2_99 <- max(0,theta2_hat - 2.57*sqrt(Dispersion_matrix[2,2]))
   Asymp_UL_theta2_99 <- theta2_hat + 2.57*sqrt(Dispersion_matrix[2,2])
   Asymp_LL_theta3_99 <- max(0,theta3_hat - 2.57*sqrt(Dispersion_matrix[3,3]))
   Asymp_UL_theta3_99 <- theta3_hat + 2.57*sqrt(Dispersion_matrix[3,3])
   Asymp_LL_theta4_99 <- max(0,theta4_hat - 2.57*sqrt(Dispersion_matrix[4,4]))
   Asymp_UL_theta4_99 <- theta4_hat + 2.57*sqrt(Dispersion_matrix[4,4])
   Asymp_LL_theta5_99 <- max(0,theta5_hat - 2.57*sqrt(Dispersion_matrix[5,5]))
   Asymp_UL_theta5_99 <- theta5_hat + 2.57*sqrt(Dispersion_matrix[5,5])
   

Unres_esttimates <- c(unres_theta1_hat,unres_theta2_hat,unres_theta3_hat,unres_theta4_hat,unres_theta5_hat)
Ordered_Estimates <- c(theta1_hat,theta2_hat,theta3_hat,theta4_hat,theta5_hat)

Unres_Asympt_CI_90 <- c(unres_Asymp_LL_theta1_90,unres_Asymp_UL_theta1_90,unres_Asymp_LL_theta2_90,unres_Asymp_UL_theta2_90,unres_Asymp_LL_theta3_90,unres_Asymp_UL_theta3_90,unres_Asymp_LL_theta4_90,unres_Asymp_UL_theta4_90,unres_Asymp_LL_theta5_90,unres_Asymp_UL_theta5_90)
Unres_Asympt_CI_95 <- c(unres_Asymp_LL_theta1_95,unres_Asymp_UL_theta1_95,unres_Asymp_LL_theta2_95,unres_Asymp_UL_theta2_95,unres_Asymp_LL_theta3_95,unres_Asymp_UL_theta3_95,unres_Asymp_LL_theta4_95,unres_Asymp_UL_theta4_95,unres_Asymp_LL_theta5_95,unres_Asymp_UL_theta5_95)
Unres_Asympt_CI_99 <- c(unres_Asymp_LL_theta1_99,unres_Asymp_UL_theta1_99,unres_Asymp_LL_theta2_99,unres_Asymp_UL_theta2_99,unres_Asymp_LL_theta3_99,unres_Asymp_UL_theta3_99,unres_Asymp_LL_theta4_99,unres_Asymp_UL_theta4_99,unres_Asymp_LL_theta5_99,unres_Asymp_UL_theta5_99)

Ordered_Asympt_CI_90 <- c(Asymp_LL_theta1_90,Asymp_UL_theta1_90,Asymp_LL_theta2_90,Asymp_UL_theta2_90,Asymp_LL_theta3_90,Asymp_UL_theta3_90,Asymp_LL_theta4_90,Asymp_UL_theta4_90,Asymp_LL_theta5_90,Asymp_UL_theta5_90)
Ordered_Asympt_CI_95 <- c(Asymp_LL_theta1_95,Asymp_UL_theta1_95,Asymp_LL_theta2_95,Asymp_UL_theta2_95,Asymp_LL_theta3_95,Asymp_UL_theta3_95,Asymp_LL_theta4_95,Asymp_UL_theta4_95,Asymp_LL_theta5_95,Asymp_UL_theta5_95)
Ordered_Asympt_CI_99 <- c(Asymp_LL_theta1_99,Asymp_UL_theta1_99,Asymp_LL_theta2_99,Asymp_UL_theta2_99,Asymp_LL_theta3_99,Asymp_UL_theta3_99,Asymp_LL_theta4_99,Asymp_UL_theta4_99,Asymp_LL_theta5_99,Asymp_UL_theta5_99)


