
source("C:/Users/debas/Dropbox/Research Work_141114/Working Papers/Order Meta multi Expo Type-II censored/R Code/Meta_Expo_data_generation.R")



##Part for MLE estimates

K = 4 
# Take a fixed value of n for all K samples
n = 20

theta1 <- 1.4 
theta2 <- 1.2  
theta3 <- 1
#tau1_all <- c(0.2, 0.2, 0.4, 0.5)
#tau2_all <- c(0.4, 0.4, 0.7, 0.7)
tau1_all <- c(0.3, 0.3, 0.5, 0.5)
tau2_all <- c(0.5, 0.5, 0.8, 0.8)
#r_all <- c(round(n*0.4), round(n*0.4), round(n*0.5), round(n*0.5))
r_all <- c(round(n*0.5), round(n*0.5), round(n*0.6), round(n*0.6))

r <- sum(r_all)

beta1 <- theta2/theta1
beta2 <- theta3/theta2
mm <- 5
mmm <- 5

## Hyper-parameters

a0 <- (mmm-1)*theta1
b0 <- mmm

#a0 <- 0.0001
#b0 <- 0.0001
a1 <- mm*beta1
a2 <- mm*beta2
b1 <- mm*(1-beta1)
b2 <-  mm*(1-beta2)
#a1 <- 1
#a2 <- 1
#b1 <- 1
#b2 <- 1


repli <- 5000



theta1_count_sym_cp_95 = 0 
theta2_count_sym_cp_95 = 0
theta3_count_sym_cp_95 = 0

theta1_count_HPD_cp_95 = 0 
theta2_count_HPD_cp_95 = 0
theta3_count_HPD_cp_95 = 0



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

theta1_BE <- 1:replic
theta2_BE <- 1:replic
theta3_BE <- 1:replic

Length_theta1_sym_95 <- 1:replic
Length_theta2_sym_95 <- 1:replic
Length_theta3_sym_95 <- 1:replic
  
Length_theta1_HPD_95  <- 1:replic
Length_theta2_HPD_95  <- 1:replic
Length_theta3_HPD_95  <- 1:replic

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
  if(m%%5 == 0)
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
  
  
  beta1_est <- rep(0,repli)
  beta2_est <- rep(0,repli)
  theta1_est <- rep(0,repli)
  theta2_est <- rep(0,repli)
  theta3_est <- rep(0,repli)
  w1 <- rep(0,repli)
  
  cnt <- 1
  while(cnt<repli+1){
     beta1_est[cnt] <- runif(1,0.01,0.99)
     beta2_est[cnt] <- runif(1,0.01,0.99)
     A_bta12 <- D1_sum + (D2_sum/beta1_est[cnt]) + (D3_sum/(beta1_est[cnt]*beta2_est[cnt]))
     
     shape1 <- r + b0
     scale1 <- 1/(a0 + A_bta12)
     scale1
     if(shape1>0 && scale1>0){
     th1 <- rgamma(1,shape=shape1, scale=scale1)
     theta1_est[cnt] <- 1/th1
     theta2_est[cnt] <- beta1_est[cnt]*theta1_est[cnt]
     theta3_est[cnt] <- beta1_est[cnt]*beta2_est[cnt]*theta1_est[cnt]
     
     ww1     <- ((a0 + A_bta12)^(-r-b0))*(beta1_est[cnt]^(a1-n_S2-n_S3-1))*((1-beta1_est[cnt])^(b1-1))*(beta2_est[cnt]^(a2-n_S3-1))*((1-beta2_est[cnt])^(b2-1))

     w1[cnt] <- ((a0 + A_bta12)^(-r-b0))*(beta1_est[cnt]^(a1-n_S2-n_S3-1))*((1-beta1_est[cnt])^(b1-1))*(beta2_est[cnt]^(a2-n_S3-1))*((1-beta2_est[cnt])^(b2-1))
     cnt <- cnt +1
     }
  }

  if(sum(w1)>0){
    
    # Construction of Symmetric CRI
    
    ww <- w1/sum(w1)
    coeff_95 <- 0.05
    
    
    ## CI for Theta1
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
    Length_theta1_sym_95[m] <-  U_theta1_sys_95 - L_theta1_sys_95
    
    
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
    Length_theta2_sym_95[m] <-  U_theta2_sys_95 - L_theta2_sys_95
    
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
    Length_theta3_sym_95[m] <-  U_theta3_sys_95 - L_theta3_sys_95
    
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
      while (w1_interval_sum<(1-coeff_95) && k<=repli){
        w1_interval_sum = w1_interval_sum + theta1_w1_sort[k,2]
        k = k+1
      }
      L_theta1[j] = theta1_w1_sort[j,1] ;
      U_theta1[j] = theta1_w1_sort[k-1,1] ;
      interval_length_theta1[j] =  U_theta1[j] -  L_theta1[j] 
    }
    
    L_theta1_HPD_95 <-  L_theta1[which.min(interval_length_theta1)]
    U_theta1_HPD_95 <-  U_theta1[which.min(interval_length_theta1)]
    Length_theta1_HPD_95[m] <- interval_length_theta1[which.min(interval_length_theta1)]
    
    
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
      while (w1_interval_sum<(1-coeff_95) && k<=repli){
        w1_interval_sum = w1_interval_sum + theta2_w1_sort[k,2]
        k = k+1
      }
      L_theta2[j] = theta2_w1_sort[j,1] ;
      U_theta2[j] = theta2_w1_sort[k-1,1] ;
      interval_length_theta2[j] =  U_theta2[j] -  L_theta2[j] 
    }
    
    L_theta2_HPD_95 <-  L_theta2[which.min(interval_length_theta2)]
    U_theta2_HPD_95 <-  U_theta2[which.min(interval_length_theta2)]
    Length_theta2_HPD_95[m] <- interval_length_theta2[which.min(interval_length_theta2)]
    
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
      while (w1_interval_sum<(1-coeff_95) && k<=repli){
        w1_interval_sum = w1_interval_sum + theta3_w1_sort[k,2]
        k = k+1
      }
      L_theta3[j] = theta3_w1_sort[j,1] ;
      U_theta3[j] = theta3_w1_sort[k-1,1] ;
      interval_length_theta3[j] =  U_theta3[j] -  L_theta3[j] 
    }
    
    L_theta3_HPD_95 <-  L_theta3[which.min(interval_length_theta3)]
    U_theta3_HPD_95 <-  U_theta3[which.min(interval_length_theta3)]
    Length_theta3_HPD_95[m] <- interval_length_theta3[which.min(interval_length_theta3)]
    
    
    if(L_theta1_HPD_95<theta1 && U_theta1_HPD_95>theta1){
      theta1_count_HPD_cp_95 <- theta1_count_HPD_cp_95 + 1
    }
    
    if(L_theta2_HPD_95<theta2 && U_theta2_HPD_95>theta2){
      theta2_count_HPD_cp_95 <- theta2_count_HPD_cp_95 + 1
    }
    
    if(L_theta3_HPD_95<theta3 && U_theta3_HPD_95>theta3){
      theta3_count_HPD_cp_95 <- theta3_count_HPD_cp_95 + 1
    }
    
    if(L_theta1_sys_95<theta1 && U_theta1_sys_95>theta1){
      theta1_count_sym_cp_95 <- theta1_count_sym_cp_95 +1
    }
    
    if(L_theta2_sys_95<theta2 && U_theta2_sys_95>theta2){
      theta2_count_sym_cp_95 <- theta2_count_sym_cp_95 +1
    }
    
    if(L_theta3_sys_95<theta3 && U_theta3_sys_95>theta3){
      theta3_count_sym_cp_95 <- theta3_count_sym_cp_95 +1
    }
    
    theta1_BE[m] <- sum(ww*theta1_est)
    theta2_BE[m] <- sum(ww*theta2_est)
    theta3_BE[m] <- sum(ww*theta3_est)
    m<- m+1 
  }
 }


theta1_ABE <- mean(theta1_BE)
theta2_ABE <- mean(theta2_BE)
theta3_ABE <- mean(theta3_BE)

theta1_MSE <- mean((theta1_BE - theta1)^2)
theta2_MSE <- mean((theta2_BE - theta2)^2)
theta3_MSE <- mean((theta3_BE - theta3)^2)

theta1_sym_cp_95 = (theta1_count_sym_cp_95/replic)*100 
theta2_sym_cp_95 = (theta2_count_sym_cp_95/replic)*100
theta3_sym_cp_95 = (theta3_count_sym_cp_95/replic)*100

theta1_HPD_cp_95 = (theta1_count_HPD_cp_95/replic)*100 
theta2_HPD_cp_95 = (theta2_count_HPD_cp_95/replic)*100
theta3_HPD_cp_95 = (theta3_count_HPD_cp_95/replic)*100

theta1_sym_AL_95 <- mean(Length_theta1_sym_95)
theta2_sym_AL_95 <- mean(Length_theta2_sym_95)
theta3_sym_AL_95 <- mean(Length_theta3_sym_95)

theta1_HPD_AL_95 <- mean(Length_theta1_HPD_95)
theta2_HPD_AL_95 <- mean(Length_theta2_HPD_95)
theta3_HPD_AL_95 <- mean(Length_theta3_HPD_95)


AE_MSE<-c(n,"&",tau1_all, "&", tau2_all,"&",r_all,"&",round(theta1_ABE,digits=4),"&",round(theta1_MSE,digits=4),"&",round(theta2_ABE,digits=4),"&",round(theta2_MSE,digits=4),"&",round(theta3_ABE,digits=4),"&",round(theta3_MSE,digits=4))


Ord_AE_MSE<-c(n,"&",tau1_all, "&", tau2_all,"&",r_all,"&",round(theta1_ABE,digits=4),"&",round(theta1_MSE,digits=4),"&",round(theta2_ABE,digits=4),"&",round(theta2_MSE,digits=4),"&",round(theta3_ABE,digits=4),"&",round(theta3_MSE,digits=4))
ord_ad_AE<-matrix(Ord_AE_MSE,nrow=1,ncol=28,byrow=T)
write.table(ord_ad_AE,"C:/Users/debas/Dropbox/Research Work_141114/Working Papers/Order Meta multi Expo Type-II censored/Simulation Results/Bayesian/set2_ninfo_ord_Bayesian_meta_expo_SS_AE_MSE.txt", row.names=F, col.names=F,quote=F,append=T)
sink("C:/Users/debas/Dropbox/Research Work_141114/Working Papers/Order Meta multi Expo Type-II censored/Simulation Results/Bayesian/set2_ninfo_ord_Bayesian_meta_expo_SS_AE_MSE.txt",append=T)
sink()


ord_sym_CI_95 <- c(n,"&",tau1_all,"&", tau2_all,"&",r_all,"&",round(theta1_sym_AL_95,digits=4),"&",round(theta1_sym_cp_95,digits=4),"&",round(theta2_sym_AL_95,digits=4),"&",round(theta2_sym_cp_95,digits=4),"&",round(theta3_sym_AL_95,digits=4),"&",round(theta3_sym_cp_95,digits=4))
ord_ad_CI_95<-matrix(ord_sym_CI_95,nrow=1,ncol=28,byrow=T)
write.table(ord_ad_CI_95,"C:/Users/debas/Dropbox/Research Work_141114/Working Papers/Order Meta multi Expo Type-II censored/Simulation Results/Bayesian/set2_ninfo_ord_Bayes_meta_expo_SS_sym_CI_95.txt", row.names=F, col.names=F,quote=F,append=T)
sink("C:/Users/debas/Dropbox/Research Work_141114/Working Papers/Order Meta multi Expo Type-II censored/Simulation Results/Bayesian/set2_ninfo_ord_Bayes_meta_expo_SS_sym_CI_95.txt",append=T)
sink()

ord_HPD_CI_95 <- c(n,"&",tau1_all,"&", tau2_all,"&",r_all,"&",round(theta1_HPD_AL_95,digits=4),"&",round(theta1_HPD_cp_95,digits=4),"&",round(theta2_HPD_AL_95,digits=4),"&",round(theta2_HPD_cp_95,digits=4),"&",round(theta3_HPD_AL_95,digits=4),"&",round(theta3_HPD_cp_95,digits=4))
ord_ad_CI_95<-matrix(ord_HPD_CI_95,nrow=1,ncol=28,byrow=T)
write.table(ord_ad_CI_95,"C:/Users/debas/Dropbox/Research Work_141114/Working Papers/Order Meta multi Expo Type-II censored/Simulation Results/Bayesian/set2_ninfo_ord_Bayes_meta_expo_SS_HPD_CI_95.txt", row.names=F, col.names=F,quote=F,append=T)
sink("C:/Users/debas/Dropbox/Research Work_141114/Working Papers/Order Meta multi Expo Type-II censored/Simulation Results/Bayesian/set2_ninfo_ord_Bayes_meta_expo_SS_HPD_CI_95.txt",append=T)
sink()



