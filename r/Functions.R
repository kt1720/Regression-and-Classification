# Subset the data base on the number of folds
get.folds = function(n, K) {
  n.fold = ceiling(n / K) 
  fold.ids.raw = rep(1:K, times = n.fold) 
  fold.ids = fold.ids.raw[1:n] 
  folds.rand = fold.ids[sample.int(n)]
  return(folds.rand)
}

# Return the MSPE
get.MSPE = function(Y, Y.hat){
  return(mean((Y - Y.hat)^2))
}

# Rescale the dataset with min-max normalization
rescale <- function(x1,x2){
  for(col in 1:ncol(x1)){
    a <- min(x2[,col])
    b <- max(x2[,col])
    x1[,col] <- (x1[,col]-a)/(b-a)
  }
  x1
}

# Draw the linear relationship of the X's against Y
draw_linear <- function(var){
  draw <- ggplot(data = raw_data, mapping = aes_string(x = var, y = "Y")) +
    geom_point() +
    geom_smooth(se = FALSE, method = 'lm') +
    ggtitle(paste0("Linear relationship between ", var, " and Y")) +
    theme(plot.title = element_text(hjust = 0.5))
  print(draw)
}