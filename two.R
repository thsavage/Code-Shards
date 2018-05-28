tic <- proc.time()

require(deepnet)

setwd("C:/Users/tsavage/Desktop/NMIST")
mnist <- load.mnist("C:/Users/tsavage/Desktop/NMIST/")

train.x <- mnist$train$x / 255
train.y <- mnist$train$yy
test.x <- mnist$test$x / 255
test.y <- mnist$test$yy

dnn <- dbn.dnn.train(x = train.x, y = train.y, hidden = c(100, 150, 100), numepochs = 3, cd = 3)

error.dnn <- nn.test(dnn, test.x, test.y)
dnn.predict <- nn.predict(dnn, test.x)
pred.label <- max.col((dnn.predict)) - 1
print(error.dnn)
table(pred.label)
table(mnist$test$y)

print(proc.time() - tic)

a <- seq(1:dim(train.x)[1])

for (i  in 1:dim(train.x)[1]) {
  print(train.y$V1[,i])
}

# Set up parameters for DNN.
# Note output layer should be 10.
data <- mx.symbol.Variable("data")
fc1 <- mx.symbol.FullyConnected(data, name="fc1", num_hidden=128)
act1 <- mx.symbol.Activation(fc1, name="relu1", act_type="relu")
fc2 <- mx.symbol.FullyConnected(act1, name="fc2", num_hidden=64)
act2 <- mx.symbol.Activation(fc2, name="relu2", act_type="relu")
fc3 <- mx.symbol.FullyConnected(act1, name="fc3", num_hidden=64)
act3 <- mx.symbol.Activation(fc3, name="relu3", act_type="relu")
fc4 <- mx.symbol.FullyConnected(act2, name="fc4", num_hidden=10)
softmax <- mx.symbol.SoftmaxOutput(fc3, name="sm")

devices <- mx.cpu()

tic <- proc.time()
mx.set.seed(1066)
model <- mx.model.FeedForward.create(softmax, X=train.x, y=train.y,
                                     ctx=devices, num.round=5, array.batch.size=100,
                                     learning.rate=0.07, momentum=0.9,  eval.metric=mx.metric.accuracy,
                                     initializer=mx.init.uniform(0.07),
                                     epoch.end.callback=mx.callback.log.train.metric(100))
print(proc.time() - tic)
