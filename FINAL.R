final.data = read.csv("DA_final_data.csv")
library(ISLR)

#lasso
#ANXIOUS
install.packages("glmnet")
library(glmnet)
x=model.matrix(ANXIOUS1~. , data = final.data)[,-1]
y=final.data$ANXIOUS1
set.seed(1)
cv.out1=cv.glmnet(x,y,alpha=1, nfolds = 5)
bestlam1=cv.out1$lambda.min
bestlam1
lasso.final1=glmnet(x,y,alpha=1, lambda = bestlam1)
lasso.final1
coef(lasso.final1)
#WORRY
library(glmnet)
x=model.matrix(WORRY1~. , data = final.data)[,-1]
y=final.data$WORRY1
set.seed(1)
cv.out2=cv.glmnet(x,y,alpha=1, nfolds = 5)
bestlam2=cv.out2$lambda.min
bestlam2
lasso.final2=glmnet(x,y,alpha=1, lambda = bestlam2)
lasso.final2
coef(lasso.final1)
#INTEREST
library(glmnet)
x=model.matrix(INTEREST1~. , data = final.data)[,-1]
y=final.data$INTEREST1
set.seed(1)
cv.out3=cv.glmnet(x,y,alpha=1, nfolds = 5)
bestlam3=cv.out3$lambda.min
bestlam3
lasso.final3=glmnet(x,y,alpha=1, lambda = bestlam3)
lasso.final3
coef(lasso.final1)

#KNN
library(class)
set.seed(1)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)




