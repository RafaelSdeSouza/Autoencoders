require(ellipse)
require(plyr)
# Charlotte Wickhams function to generate ellipse coordinates
get.ellipses <- function(coords, mclust.fit,level=0.95){
  centers <- mclust.fit$parameters$mean[coords, ]
  vars <- mclust.fit$parameters$variance$sigma[coords, coords, ]
  ldply(1:ncol(centers), function(cluster){
    data.frame(ellipse(vars[,,cluster], centre = centers[, cluster],
                       level = level), classification = cluster)
  })
}

# Function to generate data frame for producing ellipses (DM)

df.ellipses <- function(mclustobj,level=0.95){
  
 
  nms <- rownames(mclustobj$parameters$mean)
  n <- length(nms)
  grid <- expand.grid(x = 1:n, y = 1:n)
  grid <- subset(grid, x < y)
  grid <- cbind(grid[, 2], grid[, 1])
  ldply(1:nrow(grid), function(i){
    coords <- as.numeric(grid[i, ])
    ell <- get.ellipses(c(nms[coords]), mclustobj,level=level)
    names(ell) <- c('yval', 'xval', 'classification')
    data.frame(xvar = nms[coords[1]], yvar = nms[coords[2]], ell)
  })
}