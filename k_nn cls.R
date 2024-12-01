rm(list=ls())
data=iris
n=nrow(data)
set.seed(150)
sa=sample(1:n,floor(0.75*n),replace = F)
#
train=data[sa,]
cl=train$Species
train=train[,-5]
#
test=data[-sa,]
true_lab=test$Species
test=test[,-5]
k_nn=0
library(class)
for(k in 1:20){
  pred_lab=knn(train,test,cl,k)
  noinclab=sum(pred_lab!=true_lab)
  mis_rate=noinclab/length(true_lab)
  cat(k,mis_rate,"\n")
  k_nn[k]=mis_rate
}
k=which.min(k_nn);k
#
test=c(7.5,3.5,5.5,2.3)
knn(train,test,cl,k)
##------------------------------------------------
rm(list = ls())
data_for_kNN <- read_excel("C:/Users/user/Downloads/data_for_kNN.xlsx",sheet="Sheet1")
View(data_for_kNN)
train=data_for_kNN[,-8]
solve_knn <- read_excel("C:/Users/user/Downloads/data_for_kNN.xlsx",sheet="Sheet2")
View(solve_knn)
test=solve_knn
k_nn=0
library(class)
for(k in 1:10){
  pred_lab=knn(train,test,data_for_kNN$COPD,k)
  noinclab=sum(pred_lab!=data_for_kNN$COPD)
  mis_rate=noinclab/length(data_for_kNN$COPD)
  cat(k,mis_rate,"\n")
  k_nn[k]=mis_rate
}
k=which.min(k_nn);k
result=knn(train,test,data_for_kNN$COPD,k)
cbind(solve_knn ,result)
#-----------------------------------------------------------------