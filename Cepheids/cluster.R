require(mclust)
require(dbscan)
source("gg_ellipse.R")
require(rgl)
library(Rtsne)
library(plotly)
c_dat <- read.table("blg_met_rrl.dat",header = T)
head(c_dat)


x <- log(c_dat$Period)
y <- c_dat$R21
z <- c_dat$X.Fe.H.

plot3d(x,y, z,  box = F,
       type ="p", size=0.01,alpha=0.1,xlab = "EWHa", ylab = "LogNII_Ha", 
       zlab = "LogOIII_Hb",col="gray90",cex=2)


sdat <- data.frame(x,y,z)

sdat <- as.data.frame(scale(sdat))

index <- sample(1:nrow(sdat ), 5000,replace=F)



tsne <- Rtsne(sdat[index,], dims = 2, perplexity=50, verbose=TRUE, max_iter = 10000)

plot(tsne$Y)


CLUST <- Mclust(tsne$Y,G = 5,initialization=list(size=1000),
                  modelName = "VVV")

plot(CLUST)

plot_ly(x = x, y = y, z = z,color  = as.factor(cl$cluster),type = "scatter3d", mode = "markers") %>% 
  layout(scene = list(
    xaxis = list(title = "Log(P)"), 
    yaxis = list(title = "R21"), 
    zaxis = list(title = "[Fe/H]")))  



ellips <- ellipse3d(CLUST$parameters$variance$sigma[,,1], 
                    centre = c(CLUST$parameters$mean[1,1], CLUST$parameters$mean[2,1], CLUST$parameters$mean[3,1]), level = 0.95)
ellips2 <- ellipse3d(CLUST$parameters$variance$sigma[,,2], 
                     centre = c(CLUST$parameters$mean[1,2], CLUST$parameters$mean[2,2], CLUST$parameters$mean[3,2]), level = 0.95)
ellips3 <- ellipse3d(CLUST$parameters$variance$sigma[,,3], 
                     centre = c(CLUST$parameters$mean[1,3], CLUST$parameters$mean[2,3], CLUST$parameters$mean[3,3]), level = 0.95)
ellips4 <- ellipse3d(CLUST$parameters$variance$sigma[,,4], 
                     centre = c(CLUST$parameters$mean[1,4], CLUST$parameters$mean[2,4], CLUST$parameters$mean[3,4]), level = 0.95)
ellips5 <- ellipse3d(CLUST$parameters$variance$sigma[,,5], 
                     centre = c(CLUST$parameters$mean[1,5], CLUST$parameters$mean[2,5], CLUST$parameters$mean[3,5]), level = 0.95)



index <- sample(seq_len(nrow(sdat)),replace=F, size = 20000)
xx <-  sdat[index,1]
yy <-  sdat[index,2]
zz <-  sdat[index,3]  

## Some configuration parameters:
fig.width       <- 1000
fig.height      <- 1000
def.font.size   <- 1.5
label.font.size <- 2
grid.lwd        <- 3

#mypal = pal_npg("nrc", alpha = 0.7)(4)
mypal = c("cyan3" ,"magenta","orange","green2","brown")

group.col <- mypal
source("rgl_add_axes.R")
plot3d(xx,yy, zz,  box = F,
       type ="p", size=1,alpha=1,xlab = "Log(P)", ylab = "R21", 
       zlab = "[Fe/H]",col="gray15",cex=2)
#plot3d(x,y, z,  box = F,
#              type ="p", size=0.01,alpha=0.1,xlab = "x", ylab = "y", 
#              zlab = "z",col="gray90",cex=2)
# Add bounding box decoration
#rgl.bbox(color=c("gray90","black"),  shininess=3, alpha=0.8, nticks = 3 ) 
#rgl_add_axes(x, y, z, show.bbox = FALSE)
plot3d(ellips, col = mypal[1], alpha = 0.95, type = "wire",add = TRUE)
plot3d(ellips2, col = mypal[2], alpha = 0.95, add = T, type = "wire")
plot3d(ellips3, col = mypal[3], alpha = 0.95, add = T, type = "wire")
plot3d(ellips4, col = mypal[4], alpha = 0.95, add = TRUE, type = "wire")
plot3d(ellips5, col = mypal[5], alpha = 0.95, add = TRUE, type = "wire")
aspect3d(1,1,1)
## Add the grid
grid3d(side = c('x+','y+','z-'), lwd=grid.lwd)
