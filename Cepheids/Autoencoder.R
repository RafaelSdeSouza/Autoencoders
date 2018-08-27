



tmp <- load("installed_old.rda")

install.packages(installedpkgs,dependencies = T)



install.packages("h2o")

library(h2o)
h2o.init()




Lyrae <- read.table("blg_met_rrl.dat",header=T) %>% mutate(.,Period = log10(Period)) %>%
  filter(X.Fe.H. > -1.647874 && X.Fe.H. < -0.279256)

test_index <- sample(seq_len(nrow(Lyrae)),replace=F, size = 5000)
Lyrae_short <- Lyrae[test_index,c("Period","X.Fe.H.","R21")] 


LyraeH20  <- as.h2o(Lyrae)

parts <- h2o.splitFrame(LyraeH20,0.8)
train <- parts[[1]]
test <- parts[[2]]

m_AE_4 = h2o.deeplearning(x = c("Period", "R21","X.Fe.H."),
                          training_frame=train,
                          autoencoder=TRUE,
                          epochs = 100,
                          model_id = "m_AE_4",
                          train_samples_per_iteration=nrow(train),
                          hidden = c(3,2,1,2,3),
                          activation="Tanh"
                          
)

sh = h2o.scoreHistory(m_AE_4)
plot(as.data.frame(h2o.scoreHistory(m_AE_4))$training_mse,type="l")

train_1 <- h2o.deepfeatures(m_AE_4,train,3)


d1 <- as.data.frame(train_1)$DF.L2.C1
dens  <- densityMclust(d1)

summary(dens)
plot(dens, what = "density", data = d1)

plot(as.data.frame(Lyrae_short),col=dens$classification)








# GBM

mGBM <- h2o.gbm(2:5,6,train)

mGBM

h2o.performance(mGBM,test)

