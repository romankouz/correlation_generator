create_correlated_vectors <- function(r, d, x1=NULL, output_mag=1, preserve_mean = TRUE) {
  
  # start with 2 vectors
  if (is.null(x1)) {
    x1 <- rnorm(d)
  }
  x2 <- rnorm(d)
  
  # create the 0 centers and scale x1 to be a unit vector
  orig_x1_mean = mean(x1)
  x1 = (x1 - orig_x1_mean)
  x2 = (x2 - mean(x2))
  x1 = x1/norm(as.matrix(x1), "F")
  
  # make x2 orthogonal to x1
  x2 <- (diag(d) - outer(x1, x1)) %*% x2
  
  # scale x2 to be a unit vector
  x2 <- x2/norm(as.matrix(x2), "F")
  
  # transform x2 to have cor r with x1
  if (r != 0) {
    x2 <- x2 + (r/sqrt(1-r^2)) * x1
    x2 <- x2/norm(as.matrix(x2), "F")
  }
  
  x1 <- output_mag * x1
  x2 <- output_mag * x2
  
  if (preserve_mean) {
    x1 = x1 + orig_x1_mean
    x2 = x2 + orig_x1_mean
  }
  
  print(paste("Correlation:", cor(x1, x2)))
  
  return (list(x1, x2))
  
}

create_correlated_vectors(0.5, 10, x1=c(1,2,3,4,5,6,7,8,9,10), output_mag=10)