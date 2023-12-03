Lin_Reg <- function(x , y){
  x<-cbind(rep(1,100),x)
  beta = solve(t(x) %*% x) %*% t(x) %*% Y
  y <- x%*% beta
  RSS <- sum((Y-y)^2)
  return(list(beta, RSS ))
}
Lin_Reg1 <- function(x , y){
  x<-cbind(rep(1,100),x)
  beta = solve(t(x) %*% x) %*% t(x) %*% Y
  y <- x%*% beta
  RSS <- sum((Y-y)^2)
  return(RSS)
}

# Lasso regression using coordinate descent
# X: predictor matrix, Y: response variable, lambda: penalty parameter
# alpha: mixing parameter (0: Lasso, 1: Ridge)

lasso_cd <- function(X, Y, lambda, alpha, tol = 1e-5, max_iter = 1000) {
  
  n <- nrow(X)
  p <- ncol(X)
  
  # Center and scale the predictors
  X <- scale(X, center = TRUE, scale = TRUE)
  
  # Initialize beta
  beta <- rep(0, p)
  
  # Set penalty factor
  penalty <- lambda * (1 - alpha) / 2
  
  # Coordinate descent
  for (iter in 1:max_iter) {
    
    # Keep track of previous beta for convergence check
    beta_prev <- beta
    
    # Update beta for each coordinate
    for (j in 1:p) {
      
      # Compute partial residual
      r <- Y - X %*% beta + X[, j] * beta[j]
      
      # Compute coordinate-wise update for beta_j
      z <- sum(X[, j]^2)
      if (z == 0) {
        beta[j] <- 0
      } else {
        u <- sum(X[, j] * r) / z
        beta[j] <- sign(u) * pmax(0, abs(u) - penalty)
      }
    }
    
    # Check for convergence
    if (max(abs(beta - beta_prev)) < tol) {
      break
    }
  }
  
  # Rescale beta
  beta <- beta / attr(X, "scaled:scale")
  beta[1] <- beta[1] + attr(X, "scaled:center") %*% beta
  
  # Return coefficient vector
  return(beta)
}


set.seed(42)

# Generate independent variables X
X <- matrix(rnorm(2000, mean = 0, sd = 1), nrow = 100, ncol = 20)

# Generate indices for dependent variables
i <- sample(1:20, size = 4, replace = FALSE)
print(i)

# Generate coefficients for dependent variables
coefficients <- rnorm(4, mean = 0, sd = 0.5)

# Generate dependent variables Y
n <- rnorm(100, mean = 0, sd = 0.1)
Y <- X[,i] %*% coefficients + n # Y is a 100 X 1 vector
beta = Lin_Reg(X,Y)[1]

#Q1.B) Compute and print the coefficient vector ˆ βls using linear regression.
 print(beta)

# #Q1.c) Consider the forward selection method. Compute the best four (out
# of 20) parameters that minimizes the residual sum of squares. Print
# both the best set of parameters and the coefficient vector corresponding
# to the best set.

best_predictor = c()
best_predictor_matrix = matrix(nrow = 100 , ncol = 0)

  while(length(best_predictor)< 4){
    min_rss <- Inf
    index = -1
    #print(dim(best_predictor_matrix))
  for (i in 1:20) {
    if (!(is.element(i,best_predictor))){
      temp  = cbind(best_predictor_matrix, X[,i])
      #print(temp)
      rss = Lin_Reg1(temp , Y)
      #print(rss)
      if(rss < min_rss){
        index = i;
        min_rss = rss
        #print(min_rss)
      }
    }
    
  }
    best_predictor <- c(best_predictor, index)
    best_predictor_matrix <- cbind(best_predictor_matrix, X[,index])
  }
print(best_predictor)
print(best_predictor_matrix)


# Q1.D)Consider ridge regression. Center the data, fix λ = 0.01, and compute
# the coefficient vector ˆ βridge corresponding to this value of λ.
# Print ˆ βridge. Sort the coefficients ˆ βridge in decreasing order of their
# absolute value, and then print the indices that correspond to the
# five highest coefficients.
Lambda = 0.01

x<-cbind(rep(1,100),X)
beta1 = solve(t(x) %*% x + Lambda*diag(21)) %*% t(x) %*% Y
beat1 = cbind(seq(1:21) , beta1)
print(beta1)
top5_beta1 <- head(sort(abs(beta1), decreasing = TRUE), n = 5)
print(top5_beta1)



# (e) Consider lasso method. Repeat what you did for ridge regression.
z = lasso_cd(X,Y,0.01 , -1)

print(z)
