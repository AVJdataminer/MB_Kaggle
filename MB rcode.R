home=("/Users/aidenjohnson/Dropbox/Hustle/Data Science/Data Projects/MB")
setwd(home)
list.files()
df=read.csv('train.csv')
require(nnet)
x0=data.frame(class.ind(df$X0))
names(x0)=paste(names(x0),".0",sep = "")
x1=data.frame(class.ind(df$X1))
names(x1)=paste(names(x1),".1",sep = "")
x2=data.frame(class.ind(df$X2))
names(x2)=paste(names(x2),".2",sep = "")
x3=data.frame(class.ind(df$X3))
names(x3)=paste(names(x3),".3",sep = "")
x4=data.frame(class.ind(df$X4))
names(x4)=paste(names(x4),".4",sep = "")
x5=data.frame(class.ind(df$X5))
names(x5)=paste(names(x5),".5",sep = "")
x6=data.frame(class.ind(df$X6))
names(x6)=paste(names(x6),".6",sep = "")
x8=data.frame(class.ind(df$X8))
names(x8)=paste(names(x8),".8",sep = "")

# test data update --------------------------------------------------------
test=read.csv("test.csv")
xt0=data.frame(class.ind(test$X0))
names(xt0)=paste(names(xt0),".0",sep = "")
xt1=data.frame(class.ind(test$X1))
names(xt1)=paste(names(xt1),".1",sep = "")
xt2=data.frame(class.ind(test$X2))
names(xt2)=paste(names(xt2),".2",sep = "")
xt3=data.frame(class.ind(test$X3))
names(xt3)=paste(names(xt3),".3",sep = "")
xt4=data.frame(class.ind(test$X4))
names(xt4)=paste(names(xt4),".4",sep = "")
xt5=data.frame(class.ind(test$X5))
names(xt5)=paste(names(xt5),".5",sep = "")
xt6=data.frame(class.ind(test$X6))
names(xt6)=paste(names(xt6),".6",sep = "")
xt8=data.frame(class.ind(test$X8))
names(xt8)=paste(names(xt8),".8",sep = "")

# find missing columns in x0 and xt0----------------------------------------------
# z0=pmatch(names(x0),names(xt0))
# tame=names(x0) %in% names(xt0)
# xt0$aa.0=rep(0,nrow(xt0))
# xt0$ab.0=rep(0,nrow(xt0))
# xt0$ac.0=rep(0,nrow(xt0))
# xt0$q.0=rep(0,nrow(xt0))
# tame=names(xt0) %in% names(x0)
# tm=cbind(names(xt0),tame)
# 
# x0$ae.0=rep(0,nrow(x0))
# x0$an.0=rep(0,nrow(x0))
# x0$ag.0=rep(0,nrow(x0))
# x0$av.0=rep(0,nrow(x0))
# x0$bb.0=rep(0,nrow(x0))
# x0$p.0=rep(0,nrow(x0))


# finding missing levels in x1 --------------------------------------------
train=df
tame=names(train)%in% names(test)
tm=data.frame(names(train),tame)
#where tame =False use that variable name to make a variable in xt
# nan=filter(tm,tame=='FALSE')
# mat=matrix(rep(0,(nrow(x0)*nrow(nan))),nrow=nrow(x0),ncol = nrow(nan))
# mad=data.frame(mat)
# names(mad)=as.character(nan$names.x2.)
# xt2=data.frame(xt2,mad)

# tame=names(xt2)%in% names(x2)
# tm=data.frame(names(xt2),tame)
# nan=filter(tm,tame=='FALSE')
# mat=matrix(rep(0,(nrow(x0)*nrow(nan))),nrow=nrow(x0),ncol = nrow(nan))
# mad=data.frame(mat)
# names(mad)=as.character(nan[,1])
# x2=data.frame(x2,mad)

source("Find_vars.R")
Find_vars(home,x5,xt5)#function to check for matching variables and create as needed

##read in those needed and append together the train and test data
#already good:x1,x3,x4,x6,x8
x0=read.csv("x0.csv")
x2=read.csv("x2.csv")
x5=read.csv("x5.csv")

require(PCAmixdata)
data.split <- splitmix(df)
train=data.frame(x0,x1,x2,x3,x4,x5,x6,x8,data.split$X.quanti)
write.csv(train, "clean_train.csv", row.names = F)
#already good:x1,x3,x4,x6,x8
xt0=read.csv("xt0.csv")
xt2=read.csv("xt2.csv")
xt5=read.csv("xt5.csv")

data.split <- splitmix(test)
test=data.frame(xt0,xt1,xt2,xt3,xt4,xt5,xt6,xt8,data.split$X.quanti)
write.csv(test, "clean_test.csv",row.names = F)
require(dplyr)


#predict y for each ID based on X's

# clean and outlier removal -----------------------------------------------

setwd(home)
require(caret)
train=read.csv("clean_train.csv")
test=read.csv("clean_test.csv")
nzv <- nearZeroVar(train)
Ftrain <- train[, -nzv]
pca=princomp(ccd)
#remove outliers above 150
Ftrain0=dplyr::filter(Ftrain,y<150)
require(dplyr)
#review for more outliers
cooksd=cooks.distance(fit2)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 5*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>5*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  
influential <- ifelse((cooksd > 5*mean(cooksd, na.rm=T)),1,0) 

newdata=data.frame(newdata,influential)
ccd=dplyr::filter(newdata,influential==0)
ccd=dplyr::select(ccd, -influential)
ccd1 <- ccd%>%filter(y<123)%>%filter(y>83)

outs=car::outlierTest(fit)
nm=paste(names(outs$p), sep="")
indata= data.frame(ccd$y,pca$scores[,1])
fit1=loess(ccd.y~.,data =indata)
summary(fit1)
plot(fit1)

newd=princomp()
pred=predict(fit1,pca$scores[,1])
plot(pred,ccd.y)#looks like clusters!!

error <-indata$ ccd.y - pred 
RMSE <- sqrt(mean((error)^2))
RMSE
actual=indata$ccd.y
R2 <- 1 - (sum((actual-pred )^2)/sum((actual-mean(actual))^2))
R2

pred6 <- predict(fit2,test)
pred6out=data.frame(test$ID,pred6)

names(pred6out)=c("ID","y")
write.csv(pred6out,"Lm with outliers meanby5removed.csv",row.names = F)

#try some means and variance calcs
require(dplyr)
me=colMeans(select(Ftrain0,-ID,-y))
va=var(select(Ftrain0,-ID,-y))

fit2=lm(Ftrain0$y,me)

names(me)=paste0(names(Ftrain0),'_me')

tag=me[me >1]
#select and remove those vars with NA for coefs


ctrl <- trainControl(method = "repeatedcv", repeats = 10)
blass <- train(y~., data=newdata, method = "blasso",
               trControl = ctrl,preProcess=c("center","scale"), tuneLength=2)
blass
# Neural net --------------------------------------------------------------

set.seed(998)



feats <- names(select(train,-y))
# Concatenate strings
f <- paste(feats,collapse=' + ')
f <- paste('y ~',f)
# Convert to formula
f <- as.formula(f)
f
#install.packages('neuralnet')
library(neuralnet)
nn <- neuralnet(f,train,hidden=5, err.fct="sse", linear.output=T)
# Compute Predictions off Test Set
predicted.nn.values <- neuralnet::compute(nn,select(train, -y))
# Check out net.result
print(head(predicted.nn.values$net.result))
predict<- sapply(predicted.nn.values$net.result,round,digits=2)
actual=train$y
R2 <- 1 - (sum((actual-predict )^2)/sum((actual-mean(actual))^2))
R2

#perhaps complete a pca to reduce variables
pca=princomp(dplyr::select(Ftrain, -ID,-y))
pdf=data.frame(Ftrain,pca$scores[,1])
fitp=lm(y~.-ID,data = pdf)
summary(fitp)
fit1=lm(y~.,-ID,data = Ftrain)
summary(fit1)
fita=lm(y~.,-ID,data=train)
summary(fita)
pred=predict(fita,test)
outdf=data.frame(test$ID,pred)
names(outdf)=c("ID","y")
write.csv(outdf,"simple_lm_predictions.csv",row.names = F)
#try simulating normally distributed data 1000X to create new lm model


#check against actual test set
tester=select_(test, .dots=names(select(training, -y)))
predicted.nn.values <- neuralnet::compute(nn,tester)

# Check out net.result
print(head(predicted.nn.values$net.result))
predict<- round(predicted.nn.values$net.result, digits=15)
predict=cbind(test$ID,predict)
actual=testing$y
actual=cbind()
R2 <- 1 - (sum((actual-predict )^2)/sum((actual-mean(actual))^2))
R2

# Try naive bayes or random forest ----------------------------------------
#neural net was not good

# Boosted Smoothing Spline ------------------------------------------------
require(caret)
ctrl <- trainControl(method = "repeatedcv", mstop = 10,nu=0.1)

BSFit <- train(Ftrain0$y~.-ID, data=Ftrain0, method = "bstSm", 
               bst_control= ctrl)
BSFit


# PCANNET -----------------------------------------------------------------
seeds <- vector(mode = "list", length = nrow(Ftrain0) + 1)
seeds <- lapply(seeds, function(x) 1:20)
cctrl1 <- trainControl(method = "cv", number = 10, returnResamp = "all", 
                       seeds = seeds)
set.seed(849)
test_class_cv_form <- train(y ~ .-ID, data = Ftrain0, 
                            method = "pcaNNet", 
                            trControl = cctrl1,
                            preProc = c("center", "scale"),
                            trace = TRUE,thresh=0.95)
test_class_cv_form
pred <- predict(test_class_cv_form, select(Ftrain0,-y))
actual=Ftrain0$y
R2 <- 1 - (sum((actual-pred )^2)/sum((actual-mean(actual))^2))
R2                                                        

#cubist------------------
ctrl <- trainControl(method = "repeatedcv", repeats=10)

cubist.fit <- train(y~.-ID, data=newdata, method = "cubist", trControl= ctrl)
cubist.fit
## Extract faithful[,2] into faithful2
faithful2<-faithful[,2]
require(GLDEX)
## Uses clara clustering method
clara.faithful2<-fun.class.regime.bi(Ftrain0, 0.01, clara)
c1=Ftrain0[clara.faithful2$data.a,]
c1$class=rep(1,nrow(c1))
c2=Ftrain0[clara.faithful2$data.b,]
c2$class=rep(2,nrow(c2))
cdf=rbind(c1,c2)
library("flexmix")
m1 <- FLXMRlmm(y ~ .-ID, lm.fit='smooth.spline',data = cdf, k = 2,
               control= cdf$class)
m2 <- clara(Ftrain0, 2, metric = "manhattan")
m2
## plot clusters
plot(Ftrain0$y, col = m2$cluster)
## plot centers
points(m2$centers, col = 1:2, pch = 8)

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}
wssplot(test)
km.fit=kmeans(Ftrain,2)
d1=Ftrain[km.fit$cluster==2,]
fit1=lm(y~.-ID,data=d1)
summary(fit1)
d2=Ftrain[km.fit$cluster==1,]
fit2=lm(y~.-ID,data=d2)
summary(fit2)
#for the test data now
km.fit=kmeans(test,2)
t1=test[km.fit$cluster==2,]
t2=test[km.fit$cluster==1,]
pred.1=predict(fit1,t1)
pred.2=predict(fit2,t2)
y=c(pred.1,pred.2)
pred6out=data.frame(test$ID,y)
names(pred6out)=c("ID","y")
write.csv(pred6out,"Kmeans_predictions.csv",row.names = F)

# extreme learning machine ------------------------------------------------
require(elmNN)
traindata=dplyr::select(Ftrain0,-ID,-y)
feats <- names(traindata)
# Concatenate strings
f <- paste(feats,collapse=' + ')
f <- paste('y ~',f)
# Convert to formula
f <- as.formula(f)
f
model <- elmtrain.formula(f, data=Ftrain, nhid=5000, actfun="sig")
#print.elmNN(model)
p <- predict(model,newdata=Ftrain)
actual=Ftrain$y
R2 <- 1 - (sum((actual-p )^2)/sum((actual-mean(actual))^2))
R2  
error <- Ftrain$y - p
RMSE <- sqrt(mean((error)^2))
RMSE
test=data.frame(y=0,test)
test$y <- predict.elmNN(model,newdata=test)

install.packages("twilio")
#Then load the package and you can quickly start firing off messages:
  library(twilio)

# First you need to set up your accound SID and token as environmental variables
Sys.setenv(TWILIO_SID = "AC8ea39bd8323a436434562a263437d7ed")
Sys.setenv(TWILIO_TOKEN = "5892e7675cc89cb93439baefcd852db8")

# Then we're just going to store the numbers in some variables
my_phone_number <- "14062021470"
twilios_phone_number <- "14062047208"

# Now we can send away!
tw_send_message(from = twilios_phone_number, to = my_phone_number, 
                body = "Hello from R 👋")

#merge with train to make a new gbm
alldf=rbind(train,test)

names(pred6out)=c("ID","y")
write.csv(pred6out,"ELMNN_predictions3.csv",row.names = F)
#merge together train and test
test$y=pred6
allt=full_join(Ftrain0,test)
plot(Ftrain0$ID,Ftrain0$y,col="blue")
points(test$ID,test$y)
abline(h=108.6, col="red")
abline(h=90.4,col="red")

plot(density(p))
lines(density(pred),col="blue")
lines(density(y),col="purple")
lines(density(Ftrain$y),col="red")
abline(v=90.4,col="red")
abline(v=108.6,col="red")
abline(v=83,col="green")
abline(v=123,col="green")
abline(v=103,col="orange")

sim=rnorm(mean=mean(Ftrain0$y),sd=sd(Ftrain0$y), n=nrow(Ftrain0))

plot(density(Ftrain$y))
lines(density(p),col="red")
lines(density(Second_predictions_MB.csv$y),col="blue")
lines(density(Kmeans_predictions.csv$y),col="blue",lty = 2)

require(caret)
set.seed(1234)
ctrl <- trainControl(method = "repeatedcv", repeats = 10)
rfFit <- train(train.y~., data=pcdata, method = "rf", trControl = ctrl, ntree=150)
rfFit

rf.p=predict(knnFit2,test)

library(caret)
library(mlbench)
# best model yet ---------------------------------------------------------
train=read.csv("clean_train.csv")
test=read.csv("clean_test.csv")
#build more data from extreme learner predictions and re-run gbm model



require(dplyr)
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)

set.seed(825)

gbmFit2 <- train(y ~ .-ID, data = alldf, 
                 method = "gbm", 
                 trControl = fitControl,
                 verbose = FALSE)
gbmFit2

#ggplot(gbmFit1)
trellis.par.set(caretTheme())
plot(gbm.Fit)  
pca=prcomp(select(test, -ID))
pctest=data.frame(test, pca$x[,1])
gb.pred=predict(gbmFit1,pctest)
outd=data.frame(test$ID,gb.pred)
names(outd)=c("ID","y")
write.csv(outd, "Third_predictions_MB.csv", row.names=F)

LmFit1 <- train(train.y ~ ., data = pcdata, 
                 method = "lm",
                 trControl = fitControl,
                 verbose = FALSE)

set.seed(825)
svmFit <- train(y ~ .-ID, data = train, 
                method = "svmRadial", 
                trControl = fitControl, 
                preProc = c("center", "scale"),
                tuneLength = 8,
                metric = "RMSE")
svmFit

# SVM ---------------------------------------------------------------------


require('e1071')
#remove near zero variance
nzv <- nearZeroVar(train)
Ftrain <- train[, -nzv]
#remove outliers above 150
Ftrain0=dplyr::filter(Ftrain,y<150)


svm_tune <- tune(svm, train.x=select(Ftrain0,-y,-ID), train.y=select(Ftrain0,y), 
                 kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))
print(svm_tune)
svm_model_after_tune <- svm(y ~ ., data=select(Ftrain0,-ID), 
                            kernel="radial", cost=1, gamma=0.5)
summary(svm_model_after_tune)


pred <- predict(svm_model_after_tune,Ftrain0)
plot(pred,Ftrain0$y)

error <- Ftrain0$y - pred 

RMSE <- sqrt(mean((error)^2))
RMSE

pred6 <- predict(svm_model_after_tune,test)
pred6out=data.frame(test$ID,pred5)
names(pred6out)=c("ID","y")
write.csv(pred5out,"Sixthpredictions.csv",row.names = F)

#another sm--linear test

# 10 fold cross validation
ctr <- trainControl(method='repeatedcv',
                    number=10,
                    repeats=3)

# Recall as C increases, the margin tends to get wider
grid <- data.frame(C=c(0.0001, 0.001, 0.01, 1))

svm.fit <- train(log(y) ~., Ftrain0,
                 method='svmLinear',
                 preProc=c('center','scale'),
                 trControl=ctr,
                 tuneGrid=grid)
svm.fit

pred6 <- predict(svm.fit,test)
pred6out=data.frame(test$ID,pred6)
names(pred6out)=c("ID","y")
write.csv(pred6out,"Seventhpredictions.csv",row.names = F)



cv.ctrl <- trainControl(method = "repeatedcv", repeats = 1,number = 3, 
                        #summaryFunction = twoClassSummary,
                        classProbs = TRUE,
                        allowParallel=T)
set.seed(45)
xgb_tune <-train(y~.-ID,
                 data=train,
                 method="xgbTree",
                 trControl=cv.ctrl,
                 booster = "gblinear", 
                 objective = "reg:linear", 
                 metric="RMSE",
                 nthread =3)



# testing more caret ------------------------------------------------------
#build a NNet with pca using caret

ccd=na.omit(train)
nzv <- nearZeroVar(train)
filteredDescr <- train[, -nzv]
train=filteredDescr
training=sample_n(filteredDescr,500)
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)

fit2 <- train(y ~ ., data = pcdata, method = 'nnet', 
              preProcess = c('pca','scale'), trControl = fitControl, 
              tuneGrid=expand.grid(size=c(10), decay=c(0.1)))
fit2

# PCA --------------------------------------------
pca=prcomp(select(train, -ID))
str(pca)
#compute variance
std_dev=pca$sdev
pr_var <- std_dev^2
#proportion of variance explained
prop_varex <- pr_var/sum(pr_var)
prop_varex[1:20]
#scree plot
plot(prop_varex, xlab = "Principal Component",
       ylab = "Proportion of Variance Explained",
       type = "b")

#cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component",
       ylab = "Cumulative Proportion of Variance Explained",
       type = "b")

pcdata=data.frame(train$y,pca$x[,1])

## another gbm test -------------------------------
set.seed(123)
fitControl <- trainControl(method = 'cv', number = 5, summaryFunction=defaultSummary)
grid <- expand.grid(n.trees = seq(100,10000,100), interaction.depth = 2, shrinkage = .001, n.minobsinnode = 2)
fit.gbm <- train(y ~ . - ID, data=train, method = 'gbm', trControl=fitControl, tuneGrid=grid, metric='Rsquared')
plot(fit.gbm)
 
gb.pred=predict(fit.gbm,test)
outd=data.frame(test$ID,gb.pred)
names(outd)=c("ID","y")
write.csv(outd, "Fourth_predictions_MB.csv", row.names=F)
