
source("C:/Users/debas/Dropbox/Research Work_141114/Working Papers/Order Meta multi Expo Type-II censored/R Code/Meta_Expo_data_generation.R")



##Part for MLE estimates

K = 4 
# Take a fixed value of n for all K samples
n = 20

theta1 <- 1.4 
theta2 <- 1.2  
theta3 <- 1
tau1_all <- c(0.2, 0.2, 0.4, 0.5)
tau2_all <- c(0.4, 0.4, 0.7, 0.7)
#tau1_all <- c(0.3, 0.3, 0.5, 0.5)
#tau2_all <- c(0.5, 0.5, 0.8, 0.8)
r_all <- c(round(n*0.4), round(n*0.4), round(n*0.5), round(n*0.5))
#r_all <- c(round(n*0.5), round(n*0.5), round(n*0.6), round(n*0.6))

r <- sum(r_all)



theta1_mse_sum <- 0
theta2_mse_sum <- 0
theta3_mse_sum <- 0

unres_theta1_mse_sum <- 0
unres_theta2_mse_sum <- 0
unres_theta3_mse_sum <- 0


theta1_count_assym_cp_90 = 0 
theta2_count_assym_cp_90 = 0 
theta3_count_assym_cp_90 = 0 

theta1_count_assym_cp_95 = 0 
theta2_count_assym_cp_95 = 0
theta3_count_assym_cp_95 = 0


theta1_count_assym_cp_99 = 0 
theta2_count_assym_cp_99 = 0
theta3_count_assym_cp_99 = 0

unres_theta1_count_assym_cp_90 = 0 
unres_theta2_count_assym_cp_90 = 0 
unres_theta3_count_assym_cp_90 = 0 

unres_theta1_count_assym_cp_95 = 0 
unres_theta2_count_assym_cp_95 = 0
unres_theta3_count_assym_cp_95 = 0


unres_theta1_count_assym_cp_99 = 0 
unres_theta2_count_assym_cp_99 = 0
unres_theta3_count_assym_cp_99 = 0

replic <- 2000

theta1_mle1 <- 1:replic
theta2_mle1 <- 1:replic
theta3_mle1 <- 1:replic

Asymp_Length_theta1_90 <- 1:replic
Asymp_Length_theta2_90 <- 1:replic
Asymp_Length_theta3_90 <- 1:replic

Asymp_Length_theta1_95 <- 1:replic
Asymp_Length_theta2_95 <- 1:replic
Asymp_Length_theta3_95 <- 1:replic

Asymp_Length_theta1_99 <- 1:replic
Asymp_Length_theta2_99 <- 1:replic
Asymp_Length_theta3_99 <- 1:replic

unres_theta1_mle1 <- 1:replic
unres_theta2_mle1 <- 1:replic
unres_theta3_mle1 <- 1:replic

unres_Asymp_Length_theta1_90 <- 1:replic
unres_Asymp_Length_theta2_90 <- 1:replic
unres_Asymp_Length_theta3_90 <- 1:replic

unres_Asymp_Length_theta1_95 <- 1:replic
unres_Asymp_Length_theta2_95 <- 1:replic
unres_Asymp_Length_theta3_95 <- 1:replic

unres_Asymp_Length_theta1_99 <- 1:replic
unres_Asymp_Length_theta2_99 <- 1:replic
unres_Asymp_Length_theta3_99 <- 1:replic


m <- 1
while(m<replic+1)
{
  if(m%%100 == 0)
  {
    print(m)
  }
  
  for(i in 1:K)
  {
    gendataa1 <- ExpoSSLTGen(n,tau1_all[i],tau2_all[i],r_all[i])
    nam <- paste("gendata", i, sep = "")
    assign(nam, gendataa1)
    ## It will creat gendata1, gendata2, gendata3, gendata4
  }
  n1_data1 <- gendata1[1]
  n2_data1 <- gendata1[2]
  n3_data1 <- gendata1[3]
  data1 <- gendata1[4:(r_all[1]+3)]
  n1_data2 <- gendata2[1]
  n2_data2 <- gendata2[2]
  n3_data2 <- gendata2[3]
  data2 <- gendata2[4:(r_all[2]+3)]
  n1_data3 <- gendata3[1]
  n2_data3 <- gendata3[2]
  n3_data3 <- gendata3[3]
  data3 <- gendata3[4:(r_all[3]+3)]
  n1_data4 <- gendata4[1]
  n2_data4 <- gendata4[2]
  n3_data4 <- gendata4[3]
  data4 <- gendata4[4:(r_all[4]+3)]
  
  n_S1 <- n1_data1 + n1_data2 + n1_data3 + n1_data4 
  n_S2 <- n2_data1 + n2_data2 + n2_data3 + n2_data4
  n_S3 <- n3_data1 + n3_data2 + n3_data3 + n3_data4
  n1_alldata <- c(n1_data1, n1_data2, n1_data3, n1_data4)
  n2_alldata <- c(n2_data1, n2_data2, n2_data3, n2_data4)
  n3_alldata <- c(n3_data1, n3_data2, n3_data3, n3_data4)
  all_endtime <- c(data1[r_all[1]], data2[r_all[2]], data3[r_all[3]], data4[r_all[4]])
  if(n_S1>0 && n_S2>0 && n_S3>0){
  D11 <- sum(data1[data1<tau1_all[1]]) + ((n-n1_data1)*tau1_all[1])
  D12 <- sum(data2[data2<tau1_all[2]]) + ((n-n1_data2)*tau1_all[2])
  D13 <- sum(data3[data3<tau1_all[3]]) + ((n-n1_data3)*tau1_all[3])
  D14 <- sum(data4[data4<tau1_all[4]]) + ((n-n1_data4)*tau1_all[4])
  D1_sum <- D11+D12+D13+D14
  
  D21 <- sum((data1[data1>tau1_all[1] & data1<tau2_all[1]]-tau1_all[1])) + ((n-n1_data1-n2_data1)*(tau2_all[1]-tau1_all[1]))
  D22 <- sum((data2[data2>tau1_all[2] & data2<tau2_all[2]]-tau1_all[2])) + ((n-n1_data2-n2_data2)*(tau2_all[2]-tau1_all[2]))
  D23 <- sum((data3[data3>tau1_all[3] & data3<tau2_all[3]]-tau1_all[3])) + ((n-n1_data3-n2_data3)*(tau2_all[3]-tau1_all[3]))
  D24 <- sum((data4[data4>tau1_all[4] & data4<tau2_all[4]]-tau1_all[4])) + ((n-n1_data4-n2_data4)*(tau2_all[4]-tau1_all[4]))
  D2_sum <- D21+D22+D23+D24
  
  D31 <- sum((data1[data1>tau2_all[1]]-tau2_all[1])) + ((n-r_all[1])*(all_endtime[1]-tau2_all[1]))
  D32 <- sum((data2[data2>tau2_all[2]]-tau2_all[2])) + ((n-r_all[2])*(all_endtime[2]-tau2_all[2]))
  D33 <- sum((data3[data3>tau2_all[3]]-tau2_all[3])) + ((n-r_all[3])*(all_endtime[3]-tau2_all[3]))
  D34 <- sum((data4[data4>tau2_all[4]]-tau2_all[4])) + ((n-r_all[4])*(all_endtime[4]-tau2_all[4]))
  D3_sum <- D31+D32+D33+D34
  
  unres_theta1_hat <- D1_sum/n_S1
  unres_theta2_hat <- D2_sum/n_S2
  unres_theta3_hat <- D3_sum/n_S3
  
  theta1_star <- D1_sum/n_S1
  beta1_star <- D2_sum/(n_S2*theta1_star)
  beta2_star <- D3_sum/(n_S3*beta1_star*theta1_star)
  
  if(beta1_star<=1 && beta2_star<=1){
     theta1_star <- theta1_star
     beta1_hat <- beta1_star
     beta2_hat <- beta2_star
     theta1_hat <- theta1_star
     theta2_hat <- theta1_hat*beta1_star
     theta3_hat <- theta1_hat*beta1_star*beta2_star
  }else if(beta1_star>1 && beta2_star<=1){
     beta1_star <- 1
     theta1_star <- (D1_sum+D2_sum)/(n_S1+n_S2)
     beta2_star <- D3_sum/(n_S3*theta1_star)
     if(beta2_star<1){
        beta1_hat <- beta1_star
        beta2_hat <- beta2_star
        theta1_hat <- theta1_star
        theta2_hat <- theta1_hat*beta1_star
        theta3_hat <- theta1_hat*beta1_star*beta2_star
     }else if(beta2_star>1){
        beta2_star <- 1
        theta1_star <- (D1_sum+D2_sum+D3_sum)/(n_S1+n_S2+n_S3)
        beta1_hat <- beta1_star
        beta2_hat <- beta2_star
        theta1_hat <- theta1_star
        theta2_hat <- theta1_hat
        theta3_hat <- theta1_hat
     }
  }else if(beta1_star<=1 && beta2_star>1){
     beta2_star <- 1
     theta1_star <- (D1_sum)/(n_S1)
     beta1_star <- (D2_sum+D3_sum)/((n_S2+n_S3)*theta1_star)
     if(beta1_star<1){
        beta1_hat <- beta1_star
        beta2_hat <- beta2_star
        theta1_hat <- theta1_star
        theta2_hat <- theta1_hat*beta1_star
        theta3_hat <- theta1_hat*beta1_star*beta2_star
     }else if(beta1_star>1){
        beta1_star <- 1
        theta1_star <- (D1_sum+D2_sum+D3_sum)/(n_S1+n_S2+n_S3)
        beta1_hat <- beta1_star
        beta2_hat <- beta2_star
        theta1_hat <- theta1_star
        theta2_hat <- theta1_hat
        theta3_hat <- theta1_hat
     }
  }else if(beta1_star>1 && beta2_star>1){
     beta1_star <- 1
     beta2_star <- 1
     theta1_star <- (D1_sum+D2_sum+D3_sum)/(n_S1+n_S2+n_S3)
     beta1_hat <- beta1_star
     beta2_hat <- beta2_star
     theta1_hat <- theta1_star
     theta2_hat <- theta1_hat
     theta3_hat <- theta1_hat
  }
  
  
 if(theta1_hat<6 && theta2_hat<6 && theta3_hat<6 && unres_theta1_hat<6 && unres_theta2_hat<6 && unres_theta3_hat<6)
 {
  
   theta1_mle1[m] <- theta1_hat
   theta2_mle1[m] <- theta2_hat
   theta3_mle1[m] <- theta3_hat
   
   theta1_mse_sum <- theta1_mse_sum + (theta1_hat - theta1)^2
   theta2_mse_sum <- theta2_mse_sum + (theta2_hat - theta2)^2
   theta3_mse_sum <- theta3_mse_sum + (theta3_hat - theta3)^2
   
   unres_theta1_mle1[m] <- unres_theta1_hat
   unres_theta2_mle1[m] <- unres_theta2_hat
   unres_theta3_mle1[m] <- unres_theta3_hat
   
   unres_theta1_mse_sum <- unres_theta1_mse_sum + (unres_theta1_hat - theta1)^2
   unres_theta2_mse_sum <- unres_theta2_mse_sum + (unres_theta2_hat - theta2)^2
   unres_theta3_mse_sum <- unres_theta3_mse_sum + (unres_theta3_hat - theta3)^2
   
   # Unrestricted FI
   h11 <- n_S1/((unres_theta1_hat)^2)
   h22 <- n_S2/((unres_theta2_hat)^2)
   h33 <- n_S3/((unres_theta3_hat)^2)
   h12 <- h21 <- h31 <-h13 <- h23<- h32 <- 0
   unres_Fisher_information <- c(h11, h12, h13,  h21, h22, h23, h31, h32, h33)
   
   unres_Fisher_information_mat <- matrix(unres_Fisher_information, nrow=3, ncol=3, byrow=TRUE)
   unres_Dispersion_matrix <- solve(unres_Fisher_information_mat)
   
   unres_Asymp_LL_theta1_90 <- max(0,unres_theta1_hat - 1.64*sqrt(unres_Dispersion_matrix[1,1]))
   unres_Asymp_UL_theta1_90 <- unres_theta1_hat + 1.64*sqrt(unres_Dispersion_matrix[1,1])
   unres_Asymp_LL_theta2_90 <- max(0,unres_theta2_hat - 1.64*sqrt(unres_Dispersion_matrix[2,2]))
   unres_Asymp_UL_theta2_90 <- unres_theta2_hat + 1.64*sqrt(unres_Dispersion_matrix[2,2])
   unres_Asymp_LL_theta3_90 <- max(0,unres_theta3_hat - 1.64*sqrt(unres_Dispersion_matrix[3,3]))
   unres_Asymp_UL_theta3_90 <- unres_theta3_hat + 1.64*sqrt(unres_Dispersion_matrix[3,3])
   
   unres_Asymp_LL_theta1_95 <- max(0,unres_theta1_hat - 1.96*sqrt(unres_Dispersion_matrix[1,1]))
   unres_Asymp_UL_theta1_95 <- unres_theta1_hat + 1.96*sqrt(unres_Dispersion_matrix[1,1])
   unres_Asymp_LL_theta2_95 <- max(0,unres_theta2_hat - 1.96*sqrt(unres_Dispersion_matrix[2,2]))
   unres_Asymp_UL_theta2_95 <- unres_theta2_hat + 1.96*sqrt(unres_Dispersion_matrix[2,2])
   unres_Asymp_LL_theta3_95 <- max(0,unres_theta3_hat - 1.96*sqrt(unres_Dispersion_matrix[3,3]))
   unres_Asymp_UL_theta3_95 <- unres_theta3_hat + 1.96*sqrt(unres_Dispersion_matrix[3,3])
   
   unres_Asymp_LL_theta1_99 <- max(0,unres_theta1_hat - 2.57*sqrt(unres_Dispersion_matrix[1,1]))
   unres_Asymp_UL_theta1_99 <- unres_theta1_hat + 2.57*sqrt(unres_Dispersion_matrix[1,1])
   unres_Asymp_LL_theta2_99 <- max(0,unres_theta2_hat - 2.57*sqrt(unres_Dispersion_matrix[2,2]))
   unres_Asymp_UL_theta2_99 <- unres_theta2_hat + 2.57*sqrt(unres_Dispersion_matrix[2,2])
   unres_Asymp_LL_theta3_99 <- max(0,unres_theta3_hat - 2.57*sqrt(unres_Dispersion_matrix[3,3]))
   unres_Asymp_UL_theta3_99 <- unres_theta3_hat + 2.57*sqrt(unres_Dispersion_matrix[3,3])
   
   unres_Asymp_Length_theta1_90[m] <- unres_Asymp_UL_theta1_90 - unres_Asymp_LL_theta1_90
   unres_Asymp_Length_theta2_90[m] <- unres_Asymp_UL_theta2_90 - unres_Asymp_LL_theta2_90
   unres_Asymp_Length_theta3_90[m] <- unres_Asymp_UL_theta3_90 - unres_Asymp_LL_theta3_90
   
   unres_Asymp_Length_theta1_95[m] <- unres_Asymp_UL_theta1_95 - unres_Asymp_LL_theta1_95
   unres_Asymp_Length_theta2_95[m] <- unres_Asymp_UL_theta2_95 - unres_Asymp_LL_theta2_95
   unres_Asymp_Length_theta3_95[m] <- unres_Asymp_UL_theta3_95 - unres_Asymp_LL_theta3_95
   
   unres_Asymp_Length_theta1_99[m] <- unres_Asymp_UL_theta1_99 - unres_Asymp_LL_theta1_99
   unres_Asymp_Length_theta2_99[m] <- unres_Asymp_UL_theta2_99 - unres_Asymp_LL_theta2_99
   unres_Asymp_Length_theta3_99[m] <- unres_Asymp_UL_theta3_99 - unres_Asymp_LL_theta3_99
   
   if(unres_Asymp_UL_theta1_90>=theta1 && unres_Asymp_LL_theta1_90<=theta1)
   {
      unres_theta1_count_assym_cp_90 = unres_theta1_count_assym_cp_90 + 1
   }
   if(unres_Asymp_UL_theta1_95>=theta1 && unres_Asymp_LL_theta1_95<=theta1)
   {
      unres_theta1_count_assym_cp_95 = unres_theta1_count_assym_cp_95 + 1
   }
   if(unres_Asymp_UL_theta1_99>=theta1 && unres_Asymp_LL_theta1_99<=theta1)
   {
      unres_theta1_count_assym_cp_99 = unres_theta1_count_assym_cp_99 + 1
   }
   
   if(unres_Asymp_UL_theta2_90>=theta2 && unres_Asymp_LL_theta2_90<=theta2)
   {
      unres_theta2_count_assym_cp_90 = unres_theta2_count_assym_cp_90 + 1
   }
   if(unres_Asymp_UL_theta2_95>=theta2 && unres_Asymp_LL_theta2_95<=theta2)
   {
      unres_theta2_count_assym_cp_95 = unres_theta2_count_assym_cp_95 + 1
   }
   if(unres_Asymp_UL_theta2_99>=theta2 && unres_Asymp_LL_theta2_99<=theta2)
   {
      unres_theta2_count_assym_cp_99 = unres_theta2_count_assym_cp_99 + 1
   }
   
   if(unres_Asymp_UL_theta3_90>=theta3 && unres_Asymp_LL_theta3_90<=theta3)
   {
      unres_theta3_count_assym_cp_90 = unres_theta3_count_assym_cp_90 + 1
   }
   if(unres_Asymp_UL_theta3_95>=theta3 && unres_Asymp_LL_theta3_95<=theta3)
   {
      unres_theta3_count_assym_cp_95 = unres_theta3_count_assym_cp_95 + 1
   }
   if(unres_Asymp_UL_theta3_99>=theta3 && unres_Asymp_LL_theta3_99<=theta3)
   {
      unres_theta3_count_assym_cp_99 = unres_theta3_count_assym_cp_99 + 1
   }
   
   
   # Elements of Fisher Information matrix
   
   f11 <- (n_S1+n_S2+n_S3)/(theta1_hat^2) - ((2/theta1_hat^3)*(D1_sum + (D2_sum/beta1_hat) +  (D3_sum/(beta1_hat*beta2_hat))))
   f12 <- f21<- - (1/theta1_hat^2)*( (D2_sum/(beta1_hat^2)) +  (D3_sum/((beta1_hat^2)*beta2_hat)))
   f13 <- f31<- - (D3_sum/((theta1_hat^2)*(beta2_hat^2)*beta1_hat))
   f22 <- (n_S2+n_S3)/(beta1_hat^2) - ((2/(theta1_hat*(beta1_hat^3)))*(D2_sum + (D3_sum/beta2_hat)))
   f23 <- f32 <- - (D3_sum/((theta1_hat)*(beta2_hat^2)*((beta1_hat)^2)))
   f33 <- (n_S3)/(beta2_hat^2) - ((2*D3_sum)/(theta1_hat*beta1_hat*(beta2_hat^3)))
   
   Fisher_information <- c(f11, f12, f13,  f21, f22, f23, f31, f32, f33)
   
   Fisher_information_mat <- - matrix(Fisher_information, nrow=3, ncol=3, byrow=TRUE)
   
   g11 <- 1
   g12 <- g13 <- 0
   g21 <- beta1_hat
   g22 <- theta1_hat
   g23 <- 0
   g31 <- beta1_hat*beta2_hat
   g32 <- theta1_hat*beta2_hat
   g33 <- beta1_hat*theta1_hat
   
   Gradian <- c(g11,g12,g13,g21,g22,g23,g31,g32,g33)
   Grad_mat <-matrix(Gradian, nrow=3, ncol=3, byrow=TRUE)
   Grad_mat_transpose <- t(Grad_mat)
   
   Dispersion_matrix_beta <- solve(Fisher_information_mat)
   
   Dispersion_matrix <- Grad_mat%*%Dispersion_matrix_beta%*%Grad_mat_transpose
   
   Asymp_LL_theta1_90 <- max(0,theta1_hat - 1.64*sqrt(Dispersion_matrix[1,1]))
   Asymp_UL_theta1_90 <- theta1_hat + 1.64*sqrt(Dispersion_matrix[1,1])
   Asymp_LL_theta2_90 <- max(0,theta2_hat - 1.64*sqrt(Dispersion_matrix[2,2]))
   Asymp_UL_theta2_90 <- theta2_hat + 1.64*sqrt(Dispersion_matrix[2,2])
   Asymp_LL_theta3_90 <- max(0,theta3_hat - 1.64*sqrt(Dispersion_matrix[3,3]))
   Asymp_UL_theta3_90 <- theta3_hat + 1.64*sqrt(Dispersion_matrix[3,3])
   
   Asymp_LL_theta1_95 <- max(0,theta1_hat - 1.96*sqrt(Dispersion_matrix[1,1]))
   Asymp_UL_theta1_95 <- theta1_hat + 1.96*sqrt(Dispersion_matrix[1,1])
   Asymp_LL_theta2_95 <- max(0,theta2_hat - 1.96*sqrt(Dispersion_matrix[2,2]))
   Asymp_UL_theta2_95 <- theta2_hat + 1.96*sqrt(Dispersion_matrix[2,2])
   Asymp_LL_theta3_95 <- max(0,theta3_hat - 1.96*sqrt(Dispersion_matrix[3,3]))
   Asymp_UL_theta3_95 <- theta3_hat + 1.96*sqrt(Dispersion_matrix[3,3])
   
   Asymp_LL_theta1_99 <- max(0,theta1_hat - 2.57*sqrt(Dispersion_matrix[1,1]))
   Asymp_UL_theta1_99 <- theta1_hat + 2.57*sqrt(Dispersion_matrix[1,1])
   Asymp_LL_theta2_99 <- max(0,theta2_hat - 2.57*sqrt(Dispersion_matrix[2,2]))
   Asymp_UL_theta2_99 <- theta2_hat + 2.57*sqrt(Dispersion_matrix[2,2])
   Asymp_LL_theta3_99 <- max(0,theta3_hat - 2.57*sqrt(Dispersion_matrix[3,3]))
   Asymp_UL_theta3_99 <- theta3_hat + 2.57*sqrt(Dispersion_matrix[3,3])
   
   Asymp_Length_theta1_90[m] <- Asymp_UL_theta1_90 - Asymp_LL_theta1_90
   Asymp_Length_theta2_90[m] <- Asymp_UL_theta2_90 - Asymp_LL_theta2_90
   Asymp_Length_theta3_90[m] <- Asymp_UL_theta3_90 - Asymp_LL_theta3_90
   
   Asymp_Length_theta1_95[m] <- Asymp_UL_theta1_95 - Asymp_LL_theta1_95
   Asymp_Length_theta2_95[m] <- Asymp_UL_theta2_95 - Asymp_LL_theta2_95
   Asymp_Length_theta3_95[m] <- Asymp_UL_theta3_95 - Asymp_LL_theta3_95
   
   Asymp_Length_theta1_99[m] <- Asymp_UL_theta1_99 - Asymp_LL_theta1_99
   Asymp_Length_theta2_99[m] <- Asymp_UL_theta2_99 - Asymp_LL_theta2_99
   Asymp_Length_theta3_99[m] <- Asymp_UL_theta3_99 - Asymp_LL_theta3_99
   
   if(Asymp_UL_theta1_90>=theta1 && Asymp_LL_theta1_90<=theta1)
   {
      theta1_count_assym_cp_90 = theta1_count_assym_cp_90 + 1
   }
   if(Asymp_UL_theta1_95>=theta1 && Asymp_LL_theta1_95<=theta1)
   {
      theta1_count_assym_cp_95 = theta1_count_assym_cp_95 + 1
   }
   if(Asymp_UL_theta1_99>=theta1 && Asymp_LL_theta1_99<=theta1)
   {
      theta1_count_assym_cp_99 = theta1_count_assym_cp_99 + 1
   }
   
   if(Asymp_UL_theta2_90>=theta2 && Asymp_LL_theta2_90<=theta2)
   {
      theta2_count_assym_cp_90 = theta2_count_assym_cp_90 + 1
   }
   if(Asymp_UL_theta2_95>=theta2 && Asymp_LL_theta2_95<=theta2)
   {
      theta2_count_assym_cp_95 = theta2_count_assym_cp_95 + 1
   }
   if(Asymp_UL_theta2_99>=theta2 && Asymp_LL_theta2_99<=theta2)
   {
      theta2_count_assym_cp_99 = theta2_count_assym_cp_99 + 1
   }
   
   if(Asymp_UL_theta3_90>=theta3 && Asymp_LL_theta3_90<=theta3)
   {
      theta3_count_assym_cp_90 = theta3_count_assym_cp_90 + 1
   }
   if(Asymp_UL_theta3_95>=theta3 && Asymp_LL_theta3_95<=theta3)
   {
      theta3_count_assym_cp_95 = theta3_count_assym_cp_95 + 1
   }
   if(Asymp_UL_theta3_99>=theta3 && Asymp_LL_theta3_99<=theta3)
   {
      theta3_count_assym_cp_99 = theta3_count_assym_cp_99 + 1
   }
   
   m<- m+1
 }
}
}
unres_theta1_AE <- mean(unres_theta1_mle1)
unres_theta2_AE <- mean(unres_theta2_mle1)
unres_theta3_AE <- mean(unres_theta3_mle1)

unres_theta1_mse <- unres_theta1_mse_sum/replic
unres_theta2_mse <- unres_theta2_mse_sum/replic
unres_theta3_mse <- unres_theta3_mse_sum/replic


unres_Asymp_AL_theta1_90 <- mean(unres_Asymp_Length_theta1_90)
unres_Asymp_AL_theta2_90 <- mean(unres_Asymp_Length_theta2_90)
unres_Asymp_AL_theta3_90 <- mean(unres_Asymp_Length_theta3_90)

unres_Asymp_AL_theta1_95 <- mean(unres_Asymp_Length_theta1_95)
unres_Asymp_AL_theta2_95 <- mean(unres_Asymp_Length_theta2_95)
unres_Asymp_AL_theta3_95 <- mean(unres_Asymp_Length_theta3_95)

unres_Asymp_AL_theta1_99 <- mean(unres_Asymp_Length_theta1_99)
unres_Asymp_AL_theta2_99 <- mean(unres_Asymp_Length_theta2_99)
unres_Asymp_AL_theta3_99 <- mean(unres_Asymp_Length_theta3_99)


unres_theta1_assym_cp_90 = (unres_theta1_count_assym_cp_90/replic)*100 
unres_theta2_assym_cp_90 = (unres_theta2_count_assym_cp_90/replic)*100 
unres_theta3_assym_cp_90 = (unres_theta3_count_assym_cp_90/replic)*100 

unres_theta1_assym_cp_95 = (unres_theta1_count_assym_cp_95/replic)*100 
unres_theta2_assym_cp_95 = (unres_theta2_count_assym_cp_95/replic)*100 
unres_theta3_assym_cp_95 = (unres_theta3_count_assym_cp_95/replic)*100 

unres_theta1_assym_cp_99 = (unres_theta1_count_assym_cp_99/replic)*100 
unres_theta2_assym_cp_99 = (unres_theta2_count_assym_cp_99/replic)*100 
unres_theta3_assym_cp_99 = (unres_theta3_count_assym_cp_99/replic)*100 


theta1_AE <- mean(theta1_mle1)
theta2_AE <- mean(theta2_mle1)
theta3_AE <- mean(theta3_mle1)

theta1_mse <- theta1_mse_sum/replic
theta2_mse <- theta2_mse_sum/replic
theta3_mse <- theta3_mse_sum/replic


Asymp_AL_theta1_90 <- mean(Asymp_Length_theta1_90)
Asymp_AL_theta2_90 <- mean(Asymp_Length_theta2_90)
Asymp_AL_theta3_90 <- mean(Asymp_Length_theta3_90)

Asymp_AL_theta1_95 <- mean(Asymp_Length_theta1_95)
Asymp_AL_theta2_95 <- mean(Asymp_Length_theta2_95)
Asymp_AL_theta3_95 <- mean(Asymp_Length_theta3_95)

Asymp_AL_theta1_99 <- mean(Asymp_Length_theta1_99)
Asymp_AL_theta2_99 <- mean(Asymp_Length_theta2_99)
Asymp_AL_theta3_99 <- mean(Asymp_Length_theta3_99)


theta1_assym_cp_90 = (theta1_count_assym_cp_90/replic)*100 
theta2_assym_cp_90 = (theta2_count_assym_cp_90/replic)*100 
theta3_assym_cp_90 = (theta3_count_assym_cp_90/replic)*100 

theta1_assym_cp_95 = (theta1_count_assym_cp_95/replic)*100 
theta2_assym_cp_95 = (theta2_count_assym_cp_95/replic)*100 
theta3_assym_cp_95 = (theta3_count_assym_cp_95/replic)*100 

theta1_assym_cp_99 = (theta1_count_assym_cp_99/replic)*100 
theta2_assym_cp_99 = (theta2_count_assym_cp_99/replic)*100 
theta3_assym_cp_99 = (theta3_count_assym_cp_99/replic)*100 

### Without Order Restricted Inference Results

unres_AE_MSE<-c(n,"&",tau1_all, "&", tau2_all,"&",r_all,"&",round(unres_theta1_AE,digits=4),"&",round(unres_theta1_mse,digits=4),"&",round(unres_theta2_AE,digits=4),"&",round(unres_theta2_mse,digits=4),"&",round(unres_theta3_AE,digits=4),"&",round(unres_theta3_mse,digits=4))
unres_ad_AE<-matrix(unres_AE_MSE,nrow=1,ncol=28,byrow=T)
write.table(unres_ad_AE,"C:/Users/debas/Dropbox/Research Work_141114/Working Papers/Order Meta multi Expo Type-II censored/Simulation Results/Classical/set2_unres_meta_expo_SS_AE_MSE.txt", row.names=F, col.names=F,quote=F,append=T)
sink("C:/Users/debas/Dropbox/Research Work_141114/Working Papers/Order Meta multi Expo Type-II censored/Simulation Results/Classical/set2_unres_meta_expo_SS_AE_MSE.txt",append=T)
sink()

unres_Asymp_CI_90 <- c(n,"&",tau1_all,"&", tau2_all,"&",r_all,"&",round(unres_Asymp_AL_theta1_90,digits=4),"&",round(unres_theta1_assym_cp_90,digits=4),"&",round(unres_Asymp_AL_theta2_90,digits=4),"&",round(unres_theta2_assym_cp_90,digits=4),"&",round(unres_Asymp_AL_theta3_90,digits=4),"&",round(unres_theta3_assym_cp_90,digits=4))
unres_ad_CI_90<-matrix(unres_Asymp_CI_90,nrow=1,ncol=28,byrow=T)
write.table(unres_ad_CI_90,"C:/Users/debas/Dropbox/Research Work_141114/Working Papers/Order Meta multi Expo Type-II censored/Simulation Results/Classical/set2_unres_meta_expo_SS_Asym_CI_90.txt", row.names=F, col.names=F,quote=F,append=T)
sink("C:/Users/debas/Dropbox/Research Work_141114/Working Papers/Order Meta multi Expo Type-II censored/Simulation Results/Classical/set2_unres_meta_expo_SS_Asym_CI_90.txt",append=T)
sink()

unres_Asymp_CI_95 <- c(n,"&",tau1_all,"&", tau2_all,"&",r_all,"&",round(unres_Asymp_AL_theta1_95,digits=4),"&",round(unres_theta1_assym_cp_95,digits=4),"&",round(unres_Asymp_AL_theta2_95,digits=4),"&",round(unres_theta2_assym_cp_95,digits=4),"&",round(unres_Asymp_AL_theta3_95,digits=4),"&",round(unres_theta3_assym_cp_95,digits=4))
unres_ad_CI_95<-matrix(unres_Asymp_CI_95,nrow=1,ncol=28,byrow=T)
write.table(unres_ad_CI_95,"C:/Users/debas/Dropbox/Research Work_141114/Working Papers/Order Meta multi Expo Type-II censored/Simulation Results/Classical/set2_unres_meta_expo_SS_Asym_CI_95.txt", row.names=F, col.names=F,quote=F,append=T)
sink("C:/Users/debas/Dropbox/Research Work_141114/Working Papers/Order Meta multi Expo Type-II censored/Simulation Results/Classical/set2_unres_meta_expo_SS_Asym_CI_95.txt",append=T)
sink()

unres_Asymp_CI_99 <- c(n,"&",tau1_all,"&", tau2_all,"&",r_all,"&",round(unres_Asymp_AL_theta1_99,digits=4),"&",round(unres_theta1_assym_cp_99,digits=4),"&",round(unres_Asymp_AL_theta2_99,digits=4),"&",round(unres_theta2_assym_cp_99,digits=4),"&",round(unres_Asymp_AL_theta3_99,digits=4),"&",round(unres_theta3_assym_cp_99,digits=4))
unres_ad_CI_99<-matrix(unres_Asymp_CI_99,nrow=1,ncol=28,byrow=T)
write.table(unres_ad_CI_99,"C:/Users/debas/Dropbox/Research Work_141114/Working Papers/Order Meta multi Expo Type-II censored/Simulation Results/Classical/set2_unres_meta_expo_SS_Asym_CI_99.txt", row.names=F, col.names=F,quote=F,append=T)
sink("C:/Users/debas/Dropbox/Research Work_141114/Working Papers/Order Meta multi Expo Type-II censored/Simulation Results/Classical/set2_unres_meta_expo_SS_Asym_CI_99.txt",append=T)
sink()


### Order Restricted Inference Results

Ord_AE_MSE<-c(n,"&",tau1_all, "&", tau2_all,"&",r_all,"&",round(theta1_AE,digits=4),"&",round(theta1_mse,digits=4),"&",round(theta2_AE,digits=4),"&",round(theta2_mse,digits=4),"&",round(theta3_AE,digits=4),"&",round(theta3_mse,digits=4))
Ord_ad_AE<-matrix(Ord_AE_MSE,nrow=1,ncol=28,byrow=T)
write.table(Ord_ad_AE,"C:/Users/debas/Dropbox/Research Work_141114/Working Papers/Order Meta multi Expo Type-II censored/Simulation Results/Classical/set2_Order_meta_expo_SS_AE_MSE.txt", row.names=F, col.names=F,quote=F,append=T)
sink("C:/Users/debas/Dropbox/Research Work_141114/Working Papers/Order Meta multi Expo Type-II censored/Simulation Results/Classical/set2_Order_meta_expo_SS_AE_MSE.txt",append=T)
sink()

Ord_Asymp_CI_90 <- c(n,"&",tau1_all,"&", tau2_all,"&",r_all,"&",round(Asymp_AL_theta1_90,digits=4),"&",round(theta1_assym_cp_90,digits=4),"&",round(Asymp_AL_theta2_90,digits=4),"&",round(theta2_assym_cp_90,digits=4),"&",round(Asymp_AL_theta3_90,digits=4),"&",round(theta3_assym_cp_90,digits=4))
Ord_ad_CI_90<-matrix(Ord_Asymp_CI_90,nrow=1,ncol=28,byrow=T)
write.table(Ord_ad_CI_90,"C:/Users/debas/Dropbox/Research Work_141114/Working Papers/Order Meta multi Expo Type-II censored/Simulation Results/Classical/set2_Order_meta_expo_SS_Asym_CI_90.txt", row.names=F, col.names=F,quote=F,append=T)
sink("C:/Users/debas/Dropbox/Research Work_141114/Working Papers/Order Meta multi Expo Type-II censored/Simulation Results/Classical/set2_Order_meta_expo_SS_Asym_CI_90.txt",append=T)
sink()


Ord_Asymp_CI_95 <- c(n,"&",tau1_all,"&", tau2_all,"&",r_all,"&",round(Asymp_AL_theta1_95,digits=4),"&",round(theta1_assym_cp_95,digits=4),"&",round(Asymp_AL_theta2_95,digits=4),"&",round(theta2_assym_cp_95,digits=4),"&",round(Asymp_AL_theta3_95,digits=4),"&",round(theta3_assym_cp_95,digits=4))
Ord_ad_CI_95<-matrix(Ord_Asymp_CI_95,nrow=1,ncol=28,byrow=T)
write.table(Ord_ad_CI_95,"C:/Users/debas/Dropbox/Research Work_141114/Working Papers/Order Meta multi Expo Type-II censored/Simulation Results/Classical/set2_Order_meta_expo_SS_Asym_CI_95.txt", row.names=F, col.names=F,quote=F,append=T)
sink("C:/Users/debas/Dropbox/Research Work_141114/Working Papers/Order Meta multi Expo Type-II censored/Simulation Results/Classical/set2_Order_meta_expo_SS_Asym_CI_95.txt",append=T)
sink()


Ord_Asymp_CI_99 <- c(n,"&",tau1_all,"&", tau2_all,"&",r_all,"&",round(Asymp_AL_theta1_99,digits=4),"&",round(theta1_assym_cp_99,digits=4),"&",round(Asymp_AL_theta2_99,digits=4),"&",round(theta2_assym_cp_99,digits=4),"&",round(Asymp_AL_theta3_99,digits=4),"&",round(theta3_assym_cp_99,digits=4))
Ord_ad_CI_99<-matrix(Ord_Asymp_CI_99,nrow=1,ncol=28,byrow=T)
write.table(Ord_ad_CI_99,"C:/Users/debas/Dropbox/Research Work_141114/Working Papers/Order Meta multi Expo Type-II censored/Simulation Results/Classical/set2_Order_meta_expo_SS_Asym_CI_99.txt", row.names=F, col.names=F,quote=F,append=T)
sink("C:/Users/debas/Dropbox/Research Work_141114/Working Papers/Order Meta multi Expo Type-II censored/Simulation Results/Classical/set2_Order_meta_expo_SS_Asym_CI_99.txt",append=T)
sink()
