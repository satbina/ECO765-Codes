
library(MASS)

mean = c(1,0)
cov = matrix(c(1,0,0,1), nrow = 2)
m = mvrnorm(10, mean, cov)
mean1 = c(0,1)
m1 = mvrnorm(10, mean1, cov)
cov1 = matrix(c(0.1,0,0,0.1), nrow = 2)

x = matrix(0, nrow = 100, ncol = 2)
for (i in 1:10){
  x[((i-1)*10 + 1): (i*10),] = mvrnorm(10, m[i,], cov1)
}

x1 = matrix(0, nrow = 100, ncol = 2)
for (i in 1:10){
  x1[((i-1)*10 + 1) : (i*10),] = mvrnorm(10, m1[i,], cov1)
}

plot(x[1:100,1], x[1:100,2], xlab = "x1", ylab = "x2", col = "blue", pch = 19)
points(x1[1:100,1], x1[1:100,2], col = "red", pch = 19)

y = c(rep(1,100), rep(-1,100))
x = cbind(1,rbind(x,x1))
beta = solve(t(x) %*% x) %*% t(x) %*% y
u = seq(min(x[,2]), max(x[,2]), length.out = 200)
v = -(beta[1] + beta[2]*u)/beta[3]
lines(u,v)

miss_data = 0
Y = x%*%beta
for (i in 1:200){
  if(Y[i] < 0 & y[i] == 1)
    miss_data = miss_data + 1
  if(Y[i] >= 0 & y[i] == -1)
    miss_data = miss_data + 1
}
miss_data/200

# Q3,4
knn <- function(x, y, k) {
  Y_predict <- matrix(nrow=200, ncol=1)
  for (i in 1:200) {
    distance <- matrix(nrow=200, ncol=2)
    for (j in 1:200) {
      distance[j,1] <- (x[i,1] - x[j,1])^2 + (x[i,2] - x[j,2])^2
      distance[j,2] <- j
    }
    distance <- distance[order(distance[,1]),]
    Y_predict[i,1] <- 0
    for (r in 1:k) {
      Y_predict[i,1] <- Y_predict[i,1] + y[distance[r,2]]
    }
    Y_predict[i,1] <- Y_predict[i,1] / k
    if (Y_predict[i,1] < 0) {
      Y_predict[i,1] <- -1
    } else {
      Y_predict[i,1] <- 1
    }
  }
  miss_data <- 0
  for (i in 1:200) {
    if (Y_predict[i,1] != y[i]) {
      miss_data <- miss_data + 1
    }
  }
  return(list(Traning_Error=miss_data/200, Y_predict=Y_predict))
}




t = knn(x,y,15)
print(t[1])





# Q5 
mean = c(1,0)
cov = matrix(c(1,0,0,1), nrow = 2)
m = mvrnorm(10, mean, cov)
mean1 = c(0,1)
m1 = mvrnorm(10, mean1, cov)
cov1 = matrix(c(0.1,0,0,0.1), nrow = 2)

x = matrix(0, nrow = 5000, ncol = 2)
for (i in 1:10){
  x[((i-1)*500 + 1): (i*500),] = mvrnorm(500, m[i,], cov1)
}

x1 = matrix(0, nrow = 5000, ncol = 2)
for (i in 1:10){
  x1[((i-1)*500 + 1) : (i*500),] = mvrnorm(500, m1[i,], cov1)
}

plot(x[1:5000,1], x[1:5000,2], xlab = "x1", ylab = "x2", col = "blue", pch = 19)
points(x1[1:5000,1], x1[1:5000,2], col = "red", pch = 19)

y = c(rep(1,5000), rep(-1,5000))
x = cbind(1,rbind(x,x1))
beta = solve(t(x) %*% x) %*% t(x) %*% y
u = seq(min(x[,2]), max(x[,2]), length.out = 10000)
v = -(beta[1] + beta[2]*u)/beta[3]
lines(u,v)

miss_data = 0
Y = x%*%beta
for (i in 1:10000){
  if(Y[i] < 0 & y[i] == 1)
    miss_data = miss_data + 1
  if(Y[i] >= 0 & y[i] == -1)
    miss_data = miss_data + 1
}
miss_data/10000
