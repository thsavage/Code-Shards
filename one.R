# If it can be digitized, it can be analyzed.
# Character recognition was an early success story for machine learning.
# A smaller version of the larger MNIST data has been curated. 

# Read in train and test sets and recode label as data factors.
train <- read.csv('https://github.com/ozt-ca/tjo.hatenablog.samples/raw/master/r_samples/public_lib/jp/mnist_reproduced/short_prac_train.csv')
test <- read.csv('https://github.com/ozt-ca/tjo.hatenablog.samples/raw/master/r_samples/public_lib/jp/mnist_reproduced/short_prac_test.csv')
train$label <- as.factor(train$label)
test$label <- as.factor(test$label)

# Display the images.
par(mfrow=c(3,4))
for(i in 1:10){
  image(t(apply(matrix(as.vector(as.matrix(train[(i-1)*500+50,-1])),ncol=28,nrow=28,byrow=T),2,rev)),col=grey(seq(0,1,length.out=256)))
}


train<-read.csv('https://github.com/ozt-ca/tjo.hatenablog.samples/raw/master/r_samples/public_lib/jp/mnist_reproduced/short_prac_train.csv')
test<-read.csv('https://github.com/ozt-ca/tjo.hatenablog.samples/raw/master/r_samples/public_lib/jp/mnist_reproduced/short_prac_test.csv')
train$label<-as.factor(train$label)
test$label<-as.factor(test$label)





library(randomForest)
train.rf<-randomForest(label~.,train)
table(test$label, predict(train.rf, newdata=test[,-1]))
sum(diag(table(test$label,predict(train.rf,newdata=test[,-1]))))/nrow(test)

par(mfrow=c(3,4))
for(i in 1:10){
  image(t(apply(matrix(as.vector(as.matrix(train[(i-1)*500+50,-1])),ncol=28,nrow=28,byrow=T),2,rev)),col=grey(seq(0,1,length.out=256)))
}

train<-read.csv('https://github.com/ozt-ca/tjo.hatenablog.samples/raw/master/r_samples/public_lib/jp/mnist_reproduced/short_prac_train.csv')
test<-read.csv('https://github.com/ozt-ca/tjo.hatenablog.samples/raw/master/r_samples/public_lib/jp/mnist_reproduced/short_prac_test.csv')

library(xgboost)
library(Matrix)
train.mx<-sparse.model.matrix(label~., train)
test.mx<-sparse.model.matrix(label~., test)
dtrain<-xgb.DMatrix(train.mx, label=train$label)
dtest<-xgb.DMatrix(test.mx, label=test$label)

params = list(objective="multi:softmax", num_class=10, eval_metric="mlogloss", eta=0.3, 
              max_depth=5, subsample=1, colsample_bytree=0.5)
watchlist = list(train = dtrain, test = dtest)

train.gdbt<-xgb.train(params= params, data=dtrain, nrounds=70, watchlist=watchlist)