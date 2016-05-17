#Data exploring:
require(bnlearn)
library(corrplot)
library(corpcor)
library(igraph)
setwd("/Users/niclaslovsjo/Library/Mobile Documents/com~apple~CloudDocs/Kurser/Data mining/projekt/BayesNets")
house<-read.table("housing.data.txt")

colnames(house)<-c("CRIM","ZN","INDUS","CHAS","NOX","RM","AGE","DIS",
                   "RAD","TAX","PTRATIO","B","LSTAT","MEDV")

par(mfrow=c(4,4))
for(i in 1:14){hist(house[,i],breaks=30,main=paste(colnames(house)[i]),xlab="Value")}
house.cor<-cor(house)

inv.cor<-cor2pcor(house.cor)
colnames(inv.cor)<-colnames(house)
inv.cor
corrplot(inv.cor,type="upper")

par(mfrow=c(1,1))
corrplot(cor(house),type="upper")
interest<-matrix(c(0,0),ncol=2)
for (i in 1:14){
  for (j in 1:14){
    if ((abs(inv.cor[i,j])>0.4)&(i!=j)){interest<-rbind(interest,c(i,j))}
  }
}
interest
reals<-c(1,3,5,6,7,8,11,12,13,14)
stru1 <- iamb(house[,reals], test = "cor",alpha=0.1)
plot(stru1)


#Next: Learning the parameters


