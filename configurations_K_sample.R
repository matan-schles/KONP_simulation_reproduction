#labeling is like in the article

n <- 10^5


############################################################################################################
##Scenario Null , K=3
############################################################################################################

Scenario_Null_3_light<-function(n) # 25% for for all groups
{
  trt<-c(rep(1,n/3),rep(2,n/3),rep(3,n/3))
  U1 <- runif(n/3) 
  U2 <- runif(n/3) 
  U3 <- runif(n/3)
  T1 <- (1/U1)-1
  T2 <- (1/U2)-1
  T3 <- (1/U3)-1
  event_time<-c(T1,T2,T3)
  C1 <- rlnorm(n/3,1.1,0.25)
  C2 <- rlnorm(n/3,1.1,0.25)
  C3 <- rlnorm(n/3,1.1,0.25)
  censor_time<-c(C1,C2,C3)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


Scenario_Null_3_Medium<-function(n) # 50% for all groups
{
  trt<-c(rep(1,n/3),rep(2,n/3),rep(3,n/3))
  U1 <- runif(n/3) 
  U2 <- runif(n/3) 
  U3 <- runif(n/3)
  T1 <- (1/U1)-1
  T2 <- (1/U2)-1
  T3 <- (1/U3)-1
  event_time<-c(T1,T2,T3)
  C1 <- rlnorm(n/3,0,0.25)
  C2 <- rlnorm(n/3,0,0.25)
  C3 <- rlnorm(n/3,0,0.25)
  censor_time <- c(C1,C2,C3)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


# 24% for trt=3 and 59% for trt=1,2

Scenario_Null_3_part_diff<-function(n) 
{
  trt<-c(rep(1,n/3),rep(2,n/3),rep(3,n/3))
  U1 <- runif(n/3) 
  U2 <- runif(n/3) 
  U3 <- runif(n/3)
  T1 <- (1/U1)-1
  T2 <- (1/U2)-1
  T3 <- (1/U3)-1
  event_time<-c(T1,T2,T3)
  C1_unif<-runif(n/3,0,10)
  C1_exp<-rexp(n/3,0.85)
  C1<-ifelse(C1_exp<C1_unif,C1_exp,C1_unif)
  C2_unif<-runif(n/3,0,10)
  C2_exp<-rexp(n/3,0.85)
  C2<-ifelse(C2_exp<C2_unif,C2_exp,C2_unif)
  C3<-runif(n/3,0,10)
  censor_time <- c(C1,C2,C3)
  delta <- as.numeric(event_time<=censor_time)
  time <- ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


# 58% for trt==1, 41% fro trt==2, 24% for trt==3
Scenario_Null_3_all_diff<-function(n) 
{
  trt<-c(rep(1,n/3),rep(2,n/3),rep(3,n/3))
  U1 <- runif(n/3) 
  U2 <- runif(n/3) 
  U3 <- runif(n/3)
  T1 <- (1/U1)-1
  T2 <- (1/U2)-1
  T3 <- (1/U3)-1
  event_time<-c(T1,T2,T3)
  C1_unif<-runif(n/3,0,10)
  C1_exp<-rexp(n/3,0.85)
  C1<-ifelse(C1_exp<C1_unif,C1_exp,C1_unif)
  C2_unif <- runif(n/3,0,10)
  C2_exp <- rexp(n/3,0.25)
  C2 <- ifelse(C2_exp<C2_unif,C2_exp,C2_unif)
  C3 <- runif(n/3,0,10)
  censor_time <- c(C1,C2,C3)
  delta <- as.numeric(event_time<=censor_time)
  time <- ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


############################################################################################################
##Scenario Null , K=4
############################################################################################################

Scenario_Null_4_light<-function(n) # 25% for for all groups
{
  trt<-c(rep(1,n/4),rep(2,n/4),rep(3,n/4),rep(4,n/4))
  U1 <- runif(n/4) 
  U2 <- runif(n/4) 
  U3 <- runif(n/4)
  U4 <- runif(n/4)
  T1 <- (1/U1)-1
  T2 <- (1/U2)-1
  T3 <- (1/U3)-1
  T4 <- (1/U4)-1
  event_time<-c(T1,T2,T3,T4)
  C1 <- rlnorm(n/4,1.1,0.25)
  C2 <- rlnorm(n/4,1.1,0.25)
  C3 <- rlnorm(n/4,1.1,0.25)
  C4 <- rlnorm(n/4,1.1,0.25)
  censor_time<-c(C1,C2,C3,C4)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


Scenario_Null_4_Medium<-function(n) # 50% for all groups
{
  trt<-c(rep(1,n/4),rep(2,n/4),rep(3,n/4),rep(4,n/4))
  U1 <- runif(n/4) 
  U2 <- runif(n/4) 
  U3 <- runif(n/4)
  U4 <- runif(n/4)
  T1 <- (1/U1)-1
  T2 <- (1/U2)-1
  T3 <- (1/U3)-1
  T4 <- (1/U4)-1
  event_time<-c(T1,T2,T3,T4)
  C1 <- rlnorm(n/4,0,0.25)
  C2 <- rlnorm(n/4,0,0.25)
  C3 <- rlnorm(n/4,0,0.25)
  C4 <- rlnorm(n/4,0,0.25)
  censor_time <- c(C1,C2,C3,C4)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


# 24% for trt=3,4 and 59% for trt=1,2

Scenario_Null_4_part_diff<-function(n) 
{
  trt<-c(rep(1,n/4),rep(2,n/4),rep(3,n/4),rep(4,n/4))
  U1 <- runif(n/4) 
  U2 <- runif(n/4) 
  U3 <- runif(n/4)
  U4 <- runif(n/4)
  T1 <- (1/U1)-1
  T2 <- (1/U2)-1
  T3 <- (1/U3)-1
  T4 <- (1/U4)-1
  event_time<-c(T1,T2,T3,T4)
  C1_unif<-runif(n/4,0,10)
  C1_exp<-rexp(n/4,0.85)
  C1<-ifelse(C1_exp<C1_unif,C1_exp,C1_unif)
  C2_unif<-runif(n/4,0,10)
  C2_exp<-rexp(n/4,0.85)
  C2<-ifelse(C2_exp<C2_unif,C2_exp,C2_unif)
  C3<-runif(n/4,0,10)
  C4<-runif(n/4,0,10)
  censor_time <- c(C1,C2,C3,C4)
  delta <- as.numeric(event_time<=censor_time)
  time <- ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


# 58% for trt==1, 41% fro trt==2, 24% for trt==3, 18% for trt==4
Scenario_Null_4_all_diff<-function(n) 
{
  trt<-c(rep(1,n/4),rep(2,n/4),rep(3,n/4),rep(4,n/4))
  U1 <- runif(n/4) 
  U2 <- runif(n/4) 
  U3 <- runif(n/4)
  U4 <- runif(n/4)
  T1 <- (1/U1)-1
  T2 <- (1/U2)-1
  T3 <- (1/U3)-1
  T4 <- (1/U4)-1
  event_time<-c(T1,T2,T3,T4)
  C1_unif<-runif(n/4,0,10)
  C1_exp<-rexp(n/4,0.85)
  C1<-ifelse(C1_exp<C1_unif,C1_exp,C1_unif)
  C2_unif <- runif(n/4,0,10)
  C2_exp <- rexp(n/4,0.25)
  C2 <- ifelse(C2_exp<C2_unif,C2_exp,C2_unif)
  C3 <- runif(n/4,0,10)
  C4 <- rlnorm(n/4,1.5,0.25)
  censor_time <- c(C1,C2,C3,C4)
  delta <- as.numeric(event_time<=censor_time)
  time <- ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


############################################################################################################
##Scenario Null , K=5
############################################################################################################

Scenario_Null_5_light<-function(n) # 25% for for all groups
{
  trt<-c(rep(1,n/5),rep(2,n/5),rep(3,n/5),rep(4,n/5),rep(5,n/5))
  U1 <- runif(n/5) 
  U2 <- runif(n/5) 
  U3 <- runif(n/5)
  U4 <- runif(n/5)
  U5 <- runif(n/5)
  T1 <- (1/U1)-1
  T2 <- (1/U2)-1
  T3 <- (1/U3)-1
  T4 <- (1/U4)-1
  T5 <- (1/U5)-1
  event_time<-c(T1,T2,T3,T4,T5)
  C1 <- rlnorm(n/5,1.1,0.25)
  C2 <- rlnorm(n/5,1.1,0.25)
  C3 <- rlnorm(n/5,1.1,0.25)
  C4 <- rlnorm(n/5,1.1,0.25)
  C5 <- rlnorm(n/5,1.1,0.25)
  censor_time<-c(C1,C2,C3,C4,C5)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


Scenario_Null_5_Medium<-function(n) # 50% for all groups
{
  trt<-c(rep(1,n/5),rep(2,n/5),rep(3,n/5),rep(4,n/5),rep(5,n/5))
  U1 <- runif(n/5) 
  U2 <- runif(n/5) 
  U3 <- runif(n/5)
  U4 <- runif(n/5)
  U5 <- runif(n/5)
  T1 <- (1/U1)-1
  T2 <- (1/U2)-1
  T3 <- (1/U3)-1
  T4 <- (1/U4)-1
  T5 <- (1/U5)-1
  event_time<-c(T1,T2,T3,T4,T5)
  C1 <- rlnorm(n/5,0,0.25)
  C2 <- rlnorm(n/5,0,0.25)
  C3 <- rlnorm(n/5,0,0.25)
  C4 <- rlnorm(n/5,0,0.25)
  C5 <- rlnorm(n/5,0,0.25)
  censor_time <- c(C1,C2,C3,C4,C5)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


# 24% for trt=3,4,5 and 59% for trt=1,2

Scenario_Null_5_part_diff<-function(n) 
{
  trt<-c(rep(1,n/5),rep(2,n/5),rep(3,n/5),rep(4,n/5),rep(5,n/5))
  U1 <- runif(n/5) 
  U2 <- runif(n/5) 
  U3 <- runif(n/5)
  U4 <- runif(n/5)
  U5 <- runif(n/5)
  T1 <- (1/U1)-1
  T2 <- (1/U2)-1
  T3 <- (1/U3)-1
  T4 <- (1/U4)-1
  T5 <- (1/U5)-1
  event_time<-c(T1,T2,T3,T4,T5)
  C1_unif<-runif(n/5,0,10)
  C1_exp<-rexp(n/5,0.85)
  C1<-ifelse(C1_exp<C1_unif,C1_exp,C1_unif)
  C2_unif<-runif(n/5,0,10)
  C2_exp<-rexp(n/5,0.85)
  C2<-ifelse(C2_exp<C2_unif,C2_exp,C2_unif)
  C3<-runif(n/5,0,10)
  C4<-runif(n/5,0,10)
  C5<-runif(n/5,0,10)
  censor_time <- c(C1,C2,C3,C4,C5)
  delta <- as.numeric(event_time<=censor_time)
  time <- ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


# 58% for trt==1, 41% fro trt==2, 24% for trt==3, 18% for trt==4, 67 for trt==5
Scenario_Null_5_all_diff<-function(n) 
{
  trt<-c(rep(1,n/5),rep(2,n/5),rep(3,n/5),rep(4,n/5),rep(5,n/5))
  U1 <- runif(n/5) 
  U2 <- runif(n/5) 
  U3 <- runif(n/5)
  U4 <- runif(n/5)
  U5 <- runif(n/5)
  T1 <- (1/U1)-1
  T2 <- (1/U2)-1
  T3 <- (1/U3)-1
  T4 <- (1/U4)-1
  T5 <- (1/U5)-1
  event_time<-c(T1,T2,T3,T4,T5)
  C1_unif<-runif(n/5,0,10)
  C1_exp<-rexp(n/5,0.85)
  C1<-ifelse(C1_exp<C1_unif,C1_exp,C1_unif)
  C2_unif <- runif(n/5,0,10)
  C2_exp <- rexp(n/5,0.25)
  C2 <- ifelse(C2_exp<C2_unif,C2_exp,C2_unif)
  C3 <- runif(n/5,0,10)
  C4 <- rlnorm(n/5,1.5,0.25)
  C5 <- rexp(n/5,1.5)
  censor_time <- c(C1,C2,C3,C4,C5)
  delta <- as.numeric(event_time<=censor_time)
  time <- ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}



############################################################################################################
##Scenario D , K=3
############################################################################################################

Scenario_D_3_light<-function(n) # 26% for trt==1, 24% for trt===2,3
{
  trt<-c(rep(1,n/3),rep(2,n/3),rep(3,n/3))
  U1=runif(n/3,0,1)
  U2=runif(n/3,0,1)
  U3=runif(n/3,0,1)
  U1=sort(U1)
  U2=sort(U2)
  U3=sort(U3)
  T1<-(-2)*log(U1)
  T1[U1<=0.807]=(-10)*log(U1[U1<=0.807]+0.15)
  T1[U1<=0.75]=(-2/3)*(log(U1[U1<=0.75])-1.25)
  T1[U1<=0.38]=(-1)*log(U1[U1<=0.38])+0.5
  T2<-(-2/3)*log(U2)
  T2[U2<=0.5619]=(-10)*(log(U2[U2<=0.5619]+0.44))+0.4
  T2[U2<=0.5]=(-2)*log(U2[U2<=0.5]+0.1009)
  T2[U2<=0.38]=(-1)*log(U2[U2<=0.38])+0.5
  T3<-(-2/3)*log(U3)
  T3[U3<=0.5619]=(-10)*(log(U3[U3<=0.5619]+0.44))+0.4
  T3[U3<=0.5]=(-2)*log(U3[U3<=0.5]+0.1009)
  T3[U3<=0.38]=(-1)*log(U3[U3<=0.38])+0.5
  event_time<-c(T1,T2,T3)
  C1 <- runif(n/3,1.1,3)
  C2 <- runif(n/3,1.1,3)
  C3 <- runif(n/3,1.1,3)
  censor_time<-c(C1,C2,C3)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


Scenario_D_3_medium<-function(n) # 59% for trt==1, 45% for trt==2,3
{
  trt<-c(rep(1,n/3),rep(2,n/3),rep(3,n/3))
  U1=runif(n/3,0,1)
  U2=runif(n/3,0,1)
  U3=runif(n/3,0,1)
  U1=sort(U1)
  U2=sort(U2)
  U3=sort(U3)
  T1<-(-2)*log(U1)
  T1[U1<=0.807]=(-10)*log(U1[U1<=0.807]+0.15)
  T1[U1<=0.75]=(-2/3)*(log(U1[U1<=0.75])-1.25)
  T1[U1<=0.38]=(-1)*log(U1[U1<=0.38])+0.5
  T2<-(-2/3)*log(U2)
  T2[U2<=0.5619]=(-10)*(log(U2[U2<=0.5619]+0.44))+0.4
  T2[U2<=0.5]=(-2)*log(U2[U2<=0.5]+0.1009)
  T2[U2<=0.38]=(-1)*log(U2[U2<=0.38])+0.5
  T3<-(-2/3)*log(U3)
  T3[U3<=0.5619]=(-10)*(log(U3[U3<=0.5619]+0.44))+0.4
  T3[U3<=0.5]=(-2)*log(U3[U3<=0.5]+0.1009)
  T3[U3<=0.38]=(-1)*log(U3[U3<=0.38])+0.5
  event_time<-c(T1,T2,T3)
  C1 <- runif(n/3,0.1,2.1)
  C2 <- runif(n/3,0.1,2.1)
  C3 <- runif(n/3,0.1,2.1)
  censor_time <- c(C1,C2,C3)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


# 68% for trt=1 and 45% for trt=2, 27% for trt=3

Scenario_D_3_part_diff<-function(n) 
{
  trt<-c(rep(1,n/3),rep(2,n/3),rep(3,n/3))
  U1=runif(n/3,0,1)
  U2=runif(n/3,0,1)
  U3=runif(n/3,0,1)
  U1=sort(U1)
  U2=sort(U2)
  U3=sort(U3)
  T1<-(-2)*log(U1)
  T1[U1<=0.807]=(-10)*log(U1[U1<=0.807]+0.15)
  T1[U1<=0.75]=(-2/3)*(log(U1[U1<=0.75])-1.25)
  T1[U1<=0.38]=(-1)*log(U1[U1<=0.38])+0.5
  T2<-(-2/3)*log(U2)
  T2[U2<=0.5619]=(-10)*(log(U2[U2<=0.5619]+0.44))+0.4
  T2[U2<=0.5]=(-2)*log(U2[U2<=0.5]+0.1009)
  T2[U2<=0.38]=(-1)*log(U2[U2<=0.38])+0.5
  T3<-(-2/3)*log(U3)
  T3[U3<=0.5619]=(-10)*(log(U3[U3<=0.5619]+0.44))+0.4
  T3[U3<=0.5]=(-2)*log(U3[U3<=0.5]+0.1009)
  T3[U3<=0.38]=(-1)*log(U3[U3<=0.38])+0.5
  event_time<-c(T1,T2,T3)
  C1_unif<-runif(n/3,0.5,3.5)
  C1_exp<-rexp(n/3,0.5)
  C1<-ifelse(C1_exp<C1_unif,C1_exp,C1_unif)
  C2_unif<-runif(n/3,0.5,3.5)
  C2_exp<-rexp(n/3,0.5)
  C2<-ifelse(C2_exp<C2_unif,C2_exp,C2_unif)
  C3<-runif(n/3,0.5,3.5)
  censor_time <- c(C1,C2,C3)
  delta <- as.numeric(event_time<=censor_time)
  time <- ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


# 50% for trt==1, 45% fro trt==2, 27% for trt==3
Scenario_D_3_all_diff<-function(n) 
{
  trt<-c(rep(1,n/3),rep(2,n/3),rep(3,n/3))
  U1=runif(n/3,0,1)
  U2=runif(n/3,0,1)
  U3=runif(n/3,0,1)
  U1=sort(U1)
  U2=sort(U2)
  U3=sort(U3)
  T1<-(-2)*log(U1)
  T1[U1<=0.807]=(-10)*log(U1[U1<=0.807]+0.15)
  T1[U1<=0.75]=(-2/3)*(log(U1[U1<=0.75])-1.25)
  T1[U1<=0.38]=(-1)*log(U1[U1<=0.38])+0.5
  T2<-(-2/3)*log(U2)
  T2[U2<=0.5619]=(-10)*(log(U2[U2<=0.5619]+0.44))+0.4
  T2[U2<=0.5]=(-2)*log(U2[U2<=0.5]+0.1009)
  T2[U2<=0.38]=(-1)*log(U2[U2<=0.38])+0.5
  T3<-(-2/3)*log(U3)
  T3[U3<=0.5619]=(-10)*(log(U3[U3<=0.5619]+0.44))+0.4
  T3[U3<=0.5]=(-2)*log(U3[U3<=0.5]+0.1009)
  T3[U3<=0.38]=(-1)*log(U3[U3<=0.38])+0.5
  event_time<-c(T1,T2,T3)
  C1_unif <- runif(n/3,0.5,3.5)
  C1_exp<-rexp(n/3,0.3)
  C1<-ifelse(C1_exp<C1_unif,C1_exp,C1_unif)
  C2_unif <- runif(n/3,0.5,3.5)
  C2_exp <- rexp(n/3,0.5)
  C2 <- ifelse(C2_exp<C2_unif,C2_exp,C2_unif)
  C3 <- runif(n/3,0.5,3.5)
  censor_time <- c(C1,C2,C3)
  delta <- as.numeric(event_time<=censor_time)
  time <- ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}



############################################################################################################
##Scenario J_2_3 , K=3
############################################################################################################

Scenario_J_2_3_light<-function(n) # 23% for trt==1, 26% for trt===2,3
{
  trt<-c(rep(1,n/3),rep(2,n/3),rep(3,n/3))
  T1 <- rexp(n/3,1)
  T2 <- rexp(n/3,1) 
  T2[T2>0.1] <- 0.1 + rexp(sum(T2>0.1),1.7) 
  T2[T2>0.45] <- 0.45 + rexp(sum(T2>0.45),0.5) 
  T3 <- rexp(n/3,1) 
  T3[T3>0.1] <- 0.1 + rexp(sum(T3>0.1),1.7) 
  T3[T3>0.45] <- 0.45 + rexp(sum(T3>0.45),0.5) 
  event_time<-c(T1,T2,T3)
  C1 <- rexp(n/3,0.3)
  C2 <- rexp(n/3,0.3)
  C3 <- rexp(n/3,0.3)
  censor_time <- c(C1,C2,C3)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


Scenario_J_2_3_medium<-function(n) # 50% for trt==1, 49% for trt==2,3
{
  trt<-c(rep(1,n/3),rep(2,n/3),rep(3,n/3))
  T1 <- rexp(n/3,1)
  T2 <- rexp(n/3,1) 
  T2[T2>0.1] <- 0.1 + rexp(sum(T2>0.1),1.7) 
  T2[T2>0.45] <- 0.45 + rexp(sum(T2>0.45),0.5) 
  T3 <- rexp(n/3,1) 
  T3[T3>0.1] <- 0.1 + rexp(sum(T3>0.1),1.7) 
  T3[T3>0.45] <- 0.45 + rexp(sum(T3>0.45),0.5) 
  event_time<-c(T1,T2,T3)
  C1 <- rexp(n/3,1)
  C2 <- rexp(n/3,1)
  C3 <- rexp(n/3,1)
  censor_time <- c(C1,C2,C3)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


# 54% for trt=1 and 52% for trt=2, 29% for trt=3

Scenario_J_2_3_part_diff<-function(n) 
{
  trt<-c(rep(1,n/3),rep(2,n/3),rep(3,n/3))
  T1 <- rexp(n/3,1)
  T2 <- rexp(n/3,1) 
  T2[T2>0.1] <- 0.1 + rexp(sum(T2>0.1),1.7) 
  T2[T2>0.45] <- 0.45 + rexp(sum(T2>0.45),0.5) 
  T3 <- rexp(n/3,1) 
  T3[T3>0.1] <- 0.1 + rexp(sum(T3>0.1),1.7) 
  T3[T3>0.45] <- 0.45 + rexp(sum(T3>0.45),0.5) 
  event_time<-c(T1,T2,T3)
  C1_unif<-runif(n/3,0,4)
  C1_exp<-rexp(n/3,0.9)
  C1<-ifelse(C1_exp<C1_unif,C1_exp,C1_unif)
  C2_unif<-runif(n/3,0,4)
  C2_exp<-rexp(n/3,0.9)
  C2<-ifelse(C2_exp<C2_unif,C2_exp,C2_unif)
  C3<-runif(n/3,0,4)
  censor_time <- c(C1,C2,C3)
  delta <- as.numeric(event_time<=censor_time)
  time <- ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


# 54% for trt==1, 45% fro trt==2, 29% for trt==3
Scenario_J_2_3_all_diff<-function(n) 
{
  trt<-c(rep(1,n/3),rep(2,n/3),rep(3,n/3))
  T1 <- rexp(n/3,1)
  T2 <- rexp(n/3,1) 
  T2[T2>0.1] <- 0.1 + rexp(sum(T2>0.1),1.7) 
  T2[T2>0.45] <- 0.45 + rexp(sum(T2>0.45),0.5) 
  T3 <- rexp(n/3,1) 
  T3[T3>0.1] <- 0.1 + rexp(sum(T3>0.1),1.7) 
  T3[T3>0.45] <- 0.45 + rexp(sum(T3>0.45),0.5) 
  event_time<-c(T1,T2,T3)
  C1_unif<-runif(n/3,0,4)
  C1_exp<-rexp(n/3,0.9)
  C1<-ifelse(C1_exp<C1_unif,C1_exp,C1_unif)
  C2_unif <- runif(n/3,0,4)
  C2_exp <- rexp(n/3,0.5)
  C2 <- ifelse(C2_exp<C2_unif,C2_exp,C2_unif)
  C3 <- runif(n/3,0,4)
  censor_time <- c(C1,C2,C3)
  delta <- as.numeric(event_time<=censor_time)
  time <- ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


K=3
sapply(1:K,function(x){mean(1-delta[trt==x])})

# library(survival)
# plot(survfit(Surv(time,delta) ~ trt))
