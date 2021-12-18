install.packages("tidyverse")
library(tidyverse)
install.packages("caret")#for nearzvar function 
library(caret)
install.packages("glmnet")
library(glmnet)

#Clean everything done before
rm(list=ls())

#Data importing

train=read.csv("C:/Users/CIHAN/Desktop/Kaggle/House Pricing/train.csv")
test=read.csv("C:/Users/CIHAN/Desktop/Kaggle/House Pricing/test.csv")

#before we combine test and train datset, number of feature must be the same. Since we do not have SalePrice column in test data set, we will add this column to test dataset.
test$SalePrice=0


#Combine the datasets.
df=rbind(train,test)
c=rbind(train,test)   
#we could remove SalePrice from traing and then combine as well.
#df=rbind(with(train,rm("SalePrice")),test)

#check for the NA values and order the columns has NA values
col_NA=which(colSums(is.na(df))>0)
sort(colSums(sapply(df[col_NA],is.na)),decreasing = TRUE)


#How to replace NA values
#Mode: "MSZoning","Utilities","Functional","Exterior1st","Exterior2nd","Electrical","KitchenQual","GarageCars","SaleType",
#Median:"BsmtFinSF2","BsmtUnfSF",
#None: "PoolQC", "MiscFeature","Alley", "Fence","FireplaceQu","GarageFinish","GarageQual","GarageCond","BsmtCond","BsmtExposure","BsmtQual","BsmtFinType2","BsmtFinType1","MasVnrType","GarageType"                                                              
#0: "LotFrontage","MasVnrArea","BsmtFullBath","BsmtHalfBath","TotalBsmtSF","GarageArea"
#mean: "BsmtFinSF1",

#We assume that if GarageYrBlt is NA, it built in the same time with building, GarageYrBlt=YearBuilt

#replacing NA values with 0
y= c("LotFrontage","MasVnrArea","BsmtFullBath","BsmtHalfBath","TotalBsmtSF","GarageArea")
df[,y]=apply(df[,y],2,function(x){
  replace(x,is.na(x),0)
})

#Second way to replace NA values with 0
#a=df
#a[x]=replace(a[x],is.na(a[x]),0)


#replacing NA values with "None"
x=c("PoolQC", "MiscFeature","Alley", "Fence","FireplaceQu","GarageFinish","GarageQual","GarageCond","BsmtCond","BsmtExposure","BsmtQual","BsmtFinType2","BsmtFinType1","MasVnrType","GarageType")
df[,x]=apply(df[,x],2,function(x){
  replace(x,is.na(x),"None")
})



#replacing NA values with mean
df["BsmtFinSF1"]=replace(df["BsmtFinSF1"],is.na(df["BsmtFinSF1"]),mean(df[["BsmtFinSF1"]],na.rm = TRUE))

#replacing NA values with mode
z=c("MSZoning","Utilities","Functional","Exterior1st","Exterior2nd","Electrical","KitchenQual","GarageCars","SaleType")
df[,z]=apply(df[,z],2,function(x){
  replace(x,is.na(x),names(which.max(table(x))))
})


#replacing NA values with median
u=c("BsmtFinSF2","BsmtUnfSF")
df[,u]=apply(df[,u],2,function(x){
  replace(x,is.na(x),median(x,na.rm = T))
})

#we assume that GarageYrBlt is the same with YearBuilt
df$GarageYrBlt[is.na(df$GarageYrBlt)]= df$YearBuilt[is.na(df$GarageYrBlt)]

#we want to make sure whether variables classified correctly.
table(sapply(df,class))

#convert characters to factor.
classes= sapply(df,class)
classes.character= names(classes[which(classes =="character")])
df[classes.character]=lapply(df[classes.character], factor)

summary(df$GarageYrBlt)

#Assuming that garage can not be build before the house.
#df$GarageYrBlt[df$GarageYrBlt<df$YearBuilt]= df$YearBuilt[df$GarageYrBlt<df$YearBuilt]

#correcting typo 
df$GarageYrBlt[df$GarageYrBlt==2207]=2007

#change character variables into numeric variables
df$ExterQual<- recode(df$ExterQual,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
df$ExterCond<- recode(df$ExterCond,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"None"=5)
df$BsmtQual<- recode(df$BsmtQual,"None"=0,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
df$BsmtCond<- recode(df$BsmtCond,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4)
df$BsmtExposure<- recode(df$BsmtExposure,"None"=0,"No"=1,"Mn"=2,"Av"=3,"Gd"=4)
df$BsmtFinType1<- recode(df$BsmtFinType1,"None"=0,"Unf"=1,"LwQ"=2,"Rec"=3,"BLQ"=4,"ALQ"=5,"GLQ"=6)
df$BsmtFinType2<- recode(df$BsmtFinType2,"None"=0,"Unf"=1,"LwQ"=2,"Rec"=3,"BLQ"=4,"ALQ"=5,"GLQ"=6)
df$HeatingQC<- recode(df$HeatingQC,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
df$KitchenQual<- recode(df$KitchenQual,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
df$Functional<- recode(df$Functional,"Sal"=1,"Sev"=2,"Maj2"=3,"Maj1"=4,"Mod"=5,"Min2"=6,"Min1"=7,"Typ"=8)
df$FireplaceQu<- recode(df$FireplaceQu,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
df$GarageFinish<- recode(df$GarageFinish,"None"=0,"Unf"=1,"RFn"=2,"Fin"=3)
df$GarageQual<- recode(df$GarageQual,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
df$GarageCond<- recode(df$GarageCond,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
df$PoolQC<- recode(df$PoolQC,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
df$Fence<- recode(df$Fence,"None"=0,"MnWw"=1,"GdWo"=2,"MnPrv"=3,"GdPrv"=4)

#MSSubClass,MoSold and YearBuilt are taken as a numeric, we should convert it to factor.
df$MSSubClass=factor(df$MSSubClass)
df$MoSold=factor(df$MoSold)
df$YearBuilt=factor(df$YearBuilt)


#dealing with outliers----------------------------------------------------------------------------
df$GrLivArea[df$GrLivArea>4500]=4476 #changed with max value
df$LotArea[df$LotArea>50000]= 50000 #changed with max value
df$X1stFlrSF[df$X1stFlrSF>3000]=3000  #changed with max value
df$TotalBsmtSF[df$TotalBsmtSF>3000]=3000  #changed with max value
df$TotalBath[df$TotalBath>4.5]=4.5 #changed with max value

#feature engineering--------------------------------------------------------------------------
#total area feature = basement area+ ground living area
df$TotalSF=df$TotalBsmtSF + df$X1stFlrSF + df$X2ndFlrSF 

#total bath
df$TotalBath= df$BsmtFullBath+df$BsmtHalfBath*0.5+df$FullBath+df$HalfBath*0.5

#garage score
df$GarageScore=df$GarageArea*df$GarageQual

#remodeled
df$Remodeled=ifelse(df$YrSold-df$YearRemodAdd<2,1,0)

#age of the house
#convert facto to numeric value without any lose to calculate age of the house.
df$YearBuilt = as.numeric(levels(df$YearBuilt)[df$YearBuilt])
df$Age=2010-df$YearBuilt
b=df

#convert YearBuilt to factor again.
df$YearBuilt=factor(df$YearBuilt)

#skewness
ggplot(train,aes(SalePrice))+geom_density()

#we take log(SalePice) as our output since the distribution is more near to normal distribution.
df$SalePrice=log(df$SalePrice)

#split df to train and test
df_train=df[1:1460,]
df_test=df[1461:2919,]

#apply lasso regression
df_test$SalePrice= NULL

#dividing x and y values and creating matrix for lasso reg.
y=df_train%>%select(SalePrice)%>%as.matrix()
x=df_train%>%select(-SalePrice)%>%as.matrix()

#tuning paramter
lambdas=10^seq(-3,3,length.out = 100)


class(df_train$SalePrice)

table(sapply(df_test,class))

b=df
dummies <- dummyVars(b, data = b)
bDummy <- as.data.frame(predict(dummies, newdata = b))



#split df to train and test
bDummy_train=bDummy[1:1460,]
bDummy_test=bDummy[1461:2919,]

#apply lasso regression
bDummy_test$SalePrice= NULL

#dividing x and y values and creating matrix for lasso reg.
y=bDummy_train%>%select(SalePrice)%>%as.matrix()
x=bDummy_train%>%select(-SalePrice)%>%as.matrix()

#tuning paramter
lambdas=10^seq(-3,3,length.out = 100)
lasso.fit=glmnet(x,y,alpha =1, lambda=lambdas)
lasso.cv=cv.glmnet(x,y, alpha=1,lambda = lambdas, nfolds = 10)
sqrt(lasso.cv$cvm[lasso.cv$lambda == lasso.cv$lambda.min])
lasso.best=glmnet(x,y,alpha = 1, lambda = lasso.cv$lambda.min)

lasso_pred= exp(predict(lasso.best,as.matrix(bDummy_test)))
lasso_sub=data.frame(id=c(df_test$Id),SalePrice= c(lasso_pred))
colnames(lasso_sub)=c("Id", "SalePrice")
write.csv(lasso_sub, file="lasso_sub1.csv",row.names = F)


#applying elastic net
elastic.fit=glmnet(x,y,alpha =0.5, lambda=lambdas)
elastic.cv=cv.glmnet(x,y, alpha=1,lambda = lambdas, nfolds = 10)
elastic.best=glmnet(x,y,alpha = 1, lambda = elastic.cv$lambda.min)

elastic_pred= exp(predict(elastic.best,as.matrix(bDummy_test)))
elastic_sub=data.frame(id=c(df_test$Id),SalePrice= c(elastic_pred))
colnames(elastic_sub)=c("Id", "SalePrice")
write.csv(elastic_sub, file="elastic_sub1.csv",row.names = F)


#applying lasso without creating dummy variables
fe_training=df_train  
x1=select(df_train,-SalePrice)
y1=select(df_train,SalePrice)
set.seed(46)
lasso <-  cv.glmnet(x = data.matrix(fe_training[, - which(names(fe_training) %in% c('SalePrice'))]), y = fe_training$SalePrice, nfolds = 10)
lasso_pred <- as.numeric(exp(predict(lasso, newx = data.matrix(fe_test[, - which(names(fe_test) %in% c('SalePrice'))]), s = "lambda.min"))-1)
hist(lasso_pred, main="Histogram of Lasso Predictions", xlab = "Predictions")

lassoCv=cv.glmnet(data.matrix(x1),data.matrix(y1),alpha=1, nfolds = 10)
lassoCv_pred=as.numeric(exp(predict(lassoCv,data.matrix(df_test))))
lassoCvN_pred=as.numeric(exp(predict(lassoCv,data.matrix(df_test),s="lambda.min"))-1)

LassoCv_sub=data.frame(Id=test$Id,SalePrice=lassoCv_pred)
write.csv(LassoCv_sub, file = "LassoCv1_sub.csv", row.names = F)



head(lassoCvN_pred)
class(lassoCvN_pred)



















