library(h2o)
h2o.init(nthreads = -1)

index <- sample(seq(1:nrow(iris)),0.8*nrow(iris),replace=F)


iris <- as.h2o(iris)
parts <- h2o.splitFrame(iris,0.8)
train <- parts[[1]]
test <- parts[[2]]

m_AE_4 = h2o.deeplearning(x = c("Sepal.Length", "Sepal.Width","Petal.Length",
                                "Petal.Width"),
                          training_frame=train,
                          autoencoder=TRUE,
                          epochs = 100,
                          model_id = "m_AE_4",
                          train_samples_per_iteration=nrow(train),
                          hidden = c(4,3,4),
                          activation="Tanh"
                          
)

sh = h2o.scoreHistory(m_AE_4)
plot(as.data.frame(h2o.scoreHistory(m_AE_4))$training_mse,type="l")

train_1 <- h2o.deepfeatures(m_AE_4,train,3)
plot(as.data.frame(train_1))

d1 <- as.data.frame(train_1)$DF.L2.C1
dens  <- densityMclust(d1)

summary(dens)
plot(dens, what = "density", data = d1)

plot(as.data.frame(iris[,2:5]),col=dens$classification)



predictions <- h2o.predict(m_AE_4,iris)




# GBM

mGBM <- h2o.gbm(2:5,6,train)

mGBM

h2o.performance(mGBM,test)

