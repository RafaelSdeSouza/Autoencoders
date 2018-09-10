library(h2o)
h2o.init(nthreads = -1)
require(imager)
library(rvest)
search <- read_html("https://www.google.com/search?site=&tbm=isch&q=parrot")

#Grab all <img> tags, get their "src" attribute, a URL to an image
urls <- search %>% html_nodes("img") %>% html_attr("src") #Get urls of parrot pictures

#Load the first four, return as image list, display
map_il(urls[1],load.image) %>% plot

iris <- as.h2o(iris)
#parts <- h2o.splitFrame(iris,1)
train <- iris
#test <- parts[[2]]

m_AE_4 = h2o.deeplearning(x = c("Sepal.Length", "Sepal.Width","Petal.Length",
                                "Petal.Width"),
                          training_frame=train,
                          autoencoder=TRUE,
                          epochs = 100,
                          model_id = "m_AE_4",
                          train_samples_per_iteration=nrow(train),
                          hidden = c(4,2,4),
                          activation="Tanh"
                          
)

sh = h2o.scoreHistory(m_AE_4)
plot(as.data.frame(h2o.scoreHistory(m_AE_4))$training_mse,type="l")

train_1 <- as.data.frame(h2o.deepfeatures(m_AE_4,train,2))

gd <- data.frame(x=train_1[,1], y=train_1[,2],col=as.data.frame(iris[,5]))
ggplot(gd,aes(x=x,y=y,colour=Species))+
  geom_point()

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

