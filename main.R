setwd("/Users/niclaslovsjo/Library/Mobile Documents/com~apple~CloudDocs/Kurser/Data mining/projekt")
house<-read.table("housing.data.txt")

colnames(house)<-c("CRIM","ZN","INDUS","CHAS","NOX","RM","AGE","DIS",
                   "RAD","TAX","PTRATIO","B","LSTAT","MEDV")

house[]<-lapply(house,factor)
dim(house)

library(bnlearn)
?bnlearn
dag<-gs(house[,c(1:3,5:8,11:12,14)])
plot(dag)
str(house)
bn.fit(dag,data=house[,c(1:3,5:8,10:14)],method="mle")



GS<-function(data){
  #This function will perform the GrowShrink-algo that can be seen in the project-pdf as "algorithm1".
  k<-ncol(data)
  sigma<-sample(x = 1:k,size = k,replace = FALSE)
  MB.list<-list()
  changes.made<-TRUE
  U<-c()

  iter<-1
  changes.made<-FALSE
  print("out")
  while(iter<=k){
    MB<-c()
    X<-sigma[iter]
    U<-sigma[-which(sigma==X)]
    for(element in U){
      ifelse(length(MB)==0,v<-ci.test(x=data[,X],y=data[,element]),v<-ci.test(x=data[,X],y=data[,element],z=data[,MB]))
      if(v$p.value>0.05){
        MB<-c(MB,element)
        print("first")
        print(MB)
        changes.made<-TRUE
      }
    }
    for (element in MB){
      ifelse(length(MB)==0,v<-ci.test(x=data[,X],y=data[,element]),v<-ci.test(x=data[,X],y=data[,element],z=data[,MB]))
      if(v$p.value<0.05){
        MB<-MB[-which(MB==element)]
        print("second")
        print(MB)
        changes.made<-TRUE
      }
    }
    iter<-iter+1
    if(length(MB)>0){MB.list[[colnames(data)[X]]]<-colnames(data)[MB]}
    print(MB.list)
  }
  MB.list
  return(MB.list)
}

GS(house[1:20,c(1:3,5:8)])->mm

par(mfrow=c(4,4))
for(i in 1:14){
  hist(house[,i],breaks=50)
}
colnames(house)
data(iris)
iris
GS(iris[,1:4])->m
m
gs(iris[,1:4])->dag2
par(mfrow=c(1,1))
plot(dag2)
par(mfrow=c(2,2))
for(i in 1:4){hist(iris[,i],breaks=30)}
