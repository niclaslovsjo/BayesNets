##################################################################################################
#This is a first example of a Bayesian Network in the course Data mining at Linköping University.#
##################################################################################################

#source("http://bioconductor.org/biocLite.R")
#biocLite(c("graph", "RBGL", "Rgraphviz"))
#(To make gRain work)


#Setup:
library(gRain)
library(bnlearn)
data(Boston)
feat<-c("ptratio","crim","age","lstat","medv")
n<-dim(Boston)[1]
#bnlearn::discretize(as.data.frame(Boston[,feat]),breaks=2,method="quantile")->disc.boston
#we decided to do this manually(using the same values) since we need good labels:
disc.boston<-Boston[,feat]
borders<-c(19.1,0.257,77.5,11.4,21.2)
for(i in 1:5){
  factor(ifelse(disc.boston[,i]<borders[i],0,1))->disc.boston[,i]
}
#start creating network
dag <- model2network("[ptratio][age][crim|ptratio][lstat|age][medv|crim:lstat]")
bn.mle<-bn.fit(dag,data=disc.boston,method="mle")
bn.mle
graphviz.plot(dag)
#Test some properties
  #Markov blankets: 
mb(dag,node="ptratio")
mb(dag,node="lstat")
  #Conditional independency:
ci.test("medv","ptratio",data=disc.boston)
ci.test("medv","ptratio","crim",data=disc.boston)
ci.test("age","crim",data=disc.boston)
  #testing all nodes:
arc.strength(dag,data=disc.boston)
  #all edges are significant.
  #Test d-separation (note: only per our definition, since the BN is given by us)
dsep(dag,"medv","age","lstat")  #Chain connection
dsep(dag,"medv","age")          #Unconditioned gives false
#Inference:
#We are interested in queries here, e.g. what is the prob. of high medv given low lstat and high crim?
junction<-compile(as.grain(bn.mle))     #converts from bn-form to work with gRain
junction
querygrain(junction,nodes="medv")
querygrain(junction,nodes=c("medv","ptratio"),type="joint")
cond<-setEvidence(junction,nodes=c("lstat","age","ptratio","crim"),states=c("1","0","0","1"))
querygrain(cond,nodes=c("medv"))->m

#Learning and prediction:
index<-sample(1:n,floor(0.7*n),replace=FALSE)
train<-disc.boston[index,]
test<-disc.boston[-index,]
fit.train<-bn.fit(dag,data=train,method="mle")
preds<-c()
for(i in 1:dim(test)[1]){
  cond<-setEvidence(junction,nodes=c("lstat","age","ptratio","crim"),states=as.character(test[i,1:4]))
  querygrain(cond,nodes=c("medv"))->m
  ifelse(m$medv[1]>m$medv[2],preds[i]<-0,preds[i]<-1)
}
preds
test[,5]

#This is totally baaaad.

inv.cor<-cor2pcor(house.cor)
colnames(inv.cor)<-colnames(house[,feat])
inv.cor
corrplot(inv.cor,type="upper")

