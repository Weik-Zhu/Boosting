# write AdaBoost algorithm
adaboost<-function(x,y,b){
  alpha<-c()
  allpars<-matrix(nrow = b,ncol = 3)
  n<-nrow(x)
  w<-c(rep((1/n),times=n))

  for(i in 1:b){
    pars<-train(x,w,y)
    allpars[i,]<-c(pars[[1]][1],pars[[2]][1],pars[[3]][1])
    label<-classify(x,pars)
    wrong<-0
    temp<-c()
    count<-1
    for(k in 1:n){
      if(label[k]!=y[k]){
        wrong<-wrong+w[k]
        temp[count]<-k
        count<-count+1
      }
    }
    error<-wrong/sum(w)
    alpha[i]<-log((1-error)/error,2)
    for(m in 1:length(temp)){
      k<-temp[m]
      w[k]<-w[k]*(1-error)/error
    }
  }
  list1<-list(alpha,allpars)
  return(list1)

}

# cross-validation data and test data
random<-sample(1:200,40)
testdat<-ups[random,]
testy<-y[random,]
traindat<-ups[-random,]
trainy<-y[-random,]

k1<-c(1:32)
k2<-c(33:64)
k3<-c(65:96)
k4<-c(97:128)
k5<-c(129:160)

cross<-list(k1,k2,k3,k4,k5)


# calculate error and test error
error<-function(trainx,trainy,testx,testy,b){
  model<-adaboost(trainx,trainy,b)
  alpha<-model[[1]]
  allpars<-model[[2]]
  newlabel<-agg_class(testx,alpha,allpars)
  
  n<-length(testy)
  count<-0
  for(i in 1:n){
    if(newlabel[i]!=testy[i]){
      count<-count+1
    }
  }
  error<-count/n
  return(error)
}

# caculate cross_validation error
cross_error<-function(traindat,trainy,cross,b){
  n<-length(cross)
  err<-0
  for(i in 1:n){
    testx<-traindat[cross[[i]],]
    testy<-trainy[cross[[i]]]
    cross_x<-traindat[-cross[[i]],]
    cross_y<-trainy[-cross[[i]]]
    err<-err+error(cross_x,cross_y,testx,testy,b)
  }
  return(err/n)
}