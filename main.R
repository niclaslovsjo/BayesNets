setwd("/Users/niclaslovsjo/Library/Mobile Documents/com~apple~CloudDocs/Kurser/Data mining/projekt")
house<-read.table("housing.data.txt")
head(house)
colnames(house)<-c("CRIM","ZN","INDUS","CHAS","NOX","RM","AGE","DIS",
                   "RAD","TAX","PTRATIO","B","LSTAT","MEDV")

#7. Attribute Information:

# 1. CRIM      per capita crime rate by town
# 2. ZN        proportion of residential land zoned for lots over 
# 25,000 sq.ft.
# 3. INDUS     proportion of non-retail business acres per town
# 4. CHAS      Charles River dummy variable (= 1 if tract bounds 
#                                            river; 0 otherwise)
# 5. NOX       nitric oxides concentration (parts per 10 million)
# 6. RM        average number of rooms per dwelling
# 7. AGE       proportion of owner-occupied units built prior to 1940
# 8. DIS       weighted distances to five Boston employment centres
# 9. RAD       index of accessibility to radial highways
# 10. TAX      full-value property-tax rate per $10,000
# 11. PTRATIO  pupil-teacher ratio by town
# 12. B        1000(Bk - 0.63)^2 where Bk is the proportion of blacks 
# by town
# 13. LSTAT    % lower status of the population
# 14. MEDV     Median value of owner-occupied homes in $1000's

library(bnlearn)
?bnlearn
dag<-gs(house[,c(1:3,5:8,10:14)])
plot(dag)
str(house)
bn.fit(dag,data=house[,c(1:3,5:8,10:14)],method="mle")
library(gRbase)
library(gRim)


GS<-function(data){
  
}