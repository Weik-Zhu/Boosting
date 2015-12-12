# Question 1

# (1)write train function, X is a matrix the columns of which are the training 
#vectors x(1).....x(n), and w and y are vectors containing the weights and class labels.
#return (j,theta,m)

ups <- read.table('uspsdata.txt',header = FALSE,col.names = c(1:256))
y <- read.table('uspscl.txt',header = FALSE)

train<- function(x,w,y){
  j<- ncol(x)
  n<- nrow(x)
  theta<- array()
  weight<-array()

  for (i in 1:j){
    newx<- sort(x[,i])
    neworder<- order(x[,i])
    w_cum<-array(w[neworder[1]]*y[neworder[1]])
    
    for (k in 2:n){
      w_cum[k] <- w_cum[k-1]+w[neworder[k]]*y[neworder[k]]
    }
    if (w_cum[1]>0){
      location<- which.max(w_cum)
      weight[i]<- max(w_cum)
    }else{
      location<- which.min(w_cum)
      weight[i]<- min(w_cum)
    }
    theta[i]<-newx[location]
  }
  
  j<-which.max(weight)
  finaltheta<-theta[j]
  if (weight[j]>0){
    m<-1
  }else{
    m<--1
  }
  return (list(j,finaltheta,m))
}

# (2)write classify function

classify<- function(X,pars){
  j<-pars[[1]][1]
  theta<-pars[[2]][1]
  m<- pars[[3]][1]
  n<- nrow(X)
  y<-array()
  
  x<-X[,j]
  for (i in 1:n){
    if (x[i]>theta){
      y[i]<-m
    }else{
      y[i]<--m
    }
  }
  return(y)
}

# (3) write agg_class function, allpars is a matrix,each row means each par(j,theta,m)
agg_class<-function(X,alpha,allpars){
  npar<- nrow(allpars)
  n<- nrow(X)
  label<-array()
  
  for (i in 1:n){
    x<-X[i,]
    sum<-0
    for (k in 1:npar){
      j<-allpars[k,1]
      theta<-allpars[k,2]
      m<-allpars[k,3]
      if (x[j]>theta){
        sum<-sum + alpha[k]*m
      }else{
        sum<-sum - alpha[k]*m
      }
    }
    if (sum>0){
      label[i]<-1
    }else{
      label[i]<--1
    }
  }
  return(label)
}