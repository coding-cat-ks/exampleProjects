# Code by KS Advanced Marketing Models
####### this code requires the package "numDeriv" if this is not installed in your r please do.
# install.packages("numDeriv")
# install.packages("readr")

#######  --------------------------------------- 1 ---------------------------------------  #######
#is in pdf-latex document

#######  --------------------------------------- 2 ---------------------------------------  #######

LogL=function(theta, pie, y, X){
  N=dim(y)[1]
  K=length(pie)
  T=dim(y)[2]
  dens.all=0
  dens.ind=0
  
  for(i in 1:N){
      for(c in 1:K){
        dens.ik= pie[c]* 1/(2*pi)^{(T/2)} * exp(-(t(y[i,]-X%*%theta[c((2*c)-1,2*c)])%*%(y[i,]-X%*%theta[c((2*c)-1,2*c)]))/2)
        dens.ind=dens.ind+dens.ik
      }
    dens.all=dens.all+log(dens.ind)
    dens.ind=0
  }
  LogLvalue=dens.all
  return(LogLvalue)
}


#######  --------------------------------------- 3 ---------------------------------------  #######
EStep=function(theta, pie, y, X){
  N=dim(y)[1]
  K=length(pie)
  T=dim(y)[2]
  prob.ind=rep(NA,K)
  #dens.ind=0
  prob.all=matrix(NA,N,K)
 
  for(i in 1:N){
   for(c in 1:K){
     prob.ind[c]= pie[c]* (1/(2*pi)^{(T/2)} * exp(-(t(y[i,]-X%*%theta[c((2*c)-1,2*c)])%*%(y[i,]-X%*%theta[c((2*c)-1,2*c)]))/2))
   }
  prob.ind=prob.ind/sum(prob.ind)
  prob.all[i,]=t(prob.ind)
  }
  return(prob.all)
}


#######  --------------------------------------- 4 ---------------------------------------  ####### 
#is in pdf-latex document


#######  --------------------------------------- 5 ---------------------------------------  #######
MStep=function(W, y, X){
  N=dim(y)[1]
  T=dim(y)[2]
  K=dim(W)[2]
  pie.new=colMeans(W)
  
  theta.new=c(rep(NA,2*K))
  X.matrix=rep(1,N) %x% X 
  y.matrix=as.vector(t(y))
  
  for(c in 1:K){
  w=W[,c]
  w=rep(w, each=T)
  theta.new[c((2*c)-1,2*c)] <- lm.wfit(x=X.matrix, y=y.matrix, w=w)$coefficients
  }
  MList <- list("theta.new" = theta.new, "pie.new" = pie.new)
  return(MList)
}


#######  --------------------------------------- 6 ---------------------------------------  #######
EM= function(K, y,X){
  threshold=1
  N=dim(y)[1]
  T=dim(y)[2]
  
  #create starting values for pie, theta, and based on it compute W
  pie.new=rep(NA,K)
  theta.new=rep(NA,2*K)
  group <- kmeans(y,K)$cluster
  for(c in 1:K){
    pie.new[c] <- sum(group==c)/N
    y.loop=as.vector(t(y[group==c,]))
    X.loop=rep(1,sum(group==c)) %x% X 
    theta.new[c((2*c)-1,2*c)] <- lm.fit(x=X.loop, y=y.loop)$coefficients
  }
  W.new=EStep(theta.new, pie.new, y, X)
  
  #create updating loop
  while(threshold>0.0001){
  theta.old= theta.new
  pie.old= pie.new
  W.old=W.new
  theta.new=MStep(W.old,y,X)$theta.new
  pie.new=MStep(W.old,y,X)$pie.new
  W.new=EStep(theta.new, pie.new, y, X)
  threshold=sum((W.old-W.new)^2)/(N*K)
  }
  EMList <- list("theta.stable" = theta.old, "pie.stable" = pie.old)
  return(EMList)
}


#######  --------------------------------------- 7 and 8 ---------------------------------------  #######
#8A
LogLExtended=function(theta, gamma, y, X){
  K=length(pie)/2
  expon=exp(gamma)
  trans=c(expon/(1+sum(expon)),1/(1+sum(expon)))
  loglike.adj=LogL(theta, trans, y, X)
  return(loglike.adj)
}

#7 and 8 continued
Estimate=function(K,y,X){
  pie.all=matrix(NA,10,K)
  theta.all=matrix(NA,10,2*K)
  loglike.all=matrix(NA,10,1)
  se.theta.all=matrix(NA,10,2*K)
  library(numDeriv)
  
  for(r in 1:10){
    theta.r=EM(K,y,X)$theta.stable
    theta.all[r,]=theta.r
    pie.r=EM(K,y,X)$pie.stable
    pie.all[r,]=pie.r
    loglike.r=LogL(theta.r, pie.r, y, X)
    loglike.all[r]=loglike.r
    
    #extension
    gamma.r=pie.r[-c(K)]-pie.r[K]
    H.t.r=hessian(LogLExtended, x=theta.r, method="Richardson", gamma=gamma.r, y=y, X=X)
    I.t.r=solve(-H.t.r)
    se.t.r<-sqrt(diag(I.t.r))
    se.t.r<-diag(diag(se.t.r))
    se.theta.all[r,]=se.t.r
  }
  row.select=which.max(loglike.all)
  loglike.best=loglike.all[row.select,]
  theta.best=theta.all[row.select,]
  pie.best=pie.all[row.select,]
  se.theta.best=se.theta.all[row.select,]
  
  EstimateList <- list("theta.best" = theta.best, "pie.best" = pie.best, "loglike.best"=loglike.best, "se.theta.best"=se.theta.best)
  return(EstimateList)
}



#######  --------------------------------------- 9 ---------------------------------------  #######
setwd("/Users/katrinszabo/Desktop/Assignment")
data <- read_csv("data498029.csv", col_types = cols(ID = col_number(), Price = col_number(), Sales = col_number()))
data=data[,-c(1)]

#bring data in correct form, as we were asked to programm the functions
data$LogSales=log(data$Sales)
data$LogPrice=log(data$Price)
N=500
T=25
y=matrix(NA,N,T)
for(i in 1:500){
  yi=data$LogSales[((i-1)*25+1):(i*25)]
  y[i,]=yi
}
X=cbind(rep(1,25),data$LogPrice[1:25])

#execute "Estimate" for K=2,3,4 with this y and X 
Estimate(K=2,y,X)
Estimate(K=3,y,X)
Estimate(K=4,y,X)
