library(h2o)
h2o.init()




m_AE_4 = h2o.deeplearning(x = c("x", "y"),
                          training_frame=cc,
                          autoencoder=TRUE,
                          epochs = 500,
                          score_each_iteration=T,
                          model_id = "m_AE_4",
                          train_samples_per_iteration=nrow(cc),
                          activation = "Tanh",
                          hidden = c(2,1,2)
                          
)

sh = h2o.scoreHistory(m_AE_4)
plot(as.data.frame(h2o.scoreHistory(m_AE_4))$training_mse,type="l")

train_1 <- h2o.deepfeatures(m_AE_4,cc,layer=3)

d1 <- as.data.frame(train_1)
plot(d1)





Lyrae <- read.table("blg_met_rrl.dat",header=T) %>% mutate(.,Period = log10(Period)) %>%
  filter(X.Fe.H. > -1.647874 && X.Fe.H. < -0.279256)

test_index <- sample(seq_len(nrow(Lyrae)),replace=F, size = 20000)
Lyrae_short <- Lyrae[test_index,c("Period","X.Fe.H.","R21","Imag")] 


LyraeH20  <- as.h2o(Lyrae)

parts <- h2o.splitFrame(LyraeH20,0.8)
train <- parts[[1]]
test <- parts[[2]]

m_AE_4 = h2o.deeplearning(x = c("Period", "R21","X.Fe.H.","Imag"),
                          training_frame=train,
                          autoencoder=TRUE,
                          epochs = 500,
                          score_each_iteration=T,
                          model_id = "m_AE_4",
                          train_samples_per_iteration=nrow(train),
                          activation = "Tanh",
                          hidden = c(50,25,2,25,50)
                          
)

sh = h2o.scoreHistory(m_AE_4)
plot(as.data.frame(h2o.scoreHistory(m_AE_4))$training_mse,type="l")

train_1 <- h2o.deepfeatures(m_AE_4,train,layer=3)



d1 <- as.data.frame(train_1)
dens  <- Mclust(train_1)

summary(dens)
plot(dens, what = "density", data = d1)

plot(as.data.frame(Lyrae_short),col=dens$classification)








# GBM

mGBM <- h2o.gbm(2:5,6,train)

mGBM

h2o.performance(mGBM,test)

