# (a) Construct the data set for a 3-class classification as follows. Generate
# (X1,X2, . . . ,X50) ∈ R2 such that they are i.i.d. ∼ N([0, 0], I2). Label
# them all in bin 1. Generate (X51,X52, . . . ,X100) ∈ R2 such that
# they are i.i.d. ∼ N([2, 0], I2). Label them all in bin 2. Generate
# (X101,X102, . . . ,X150) ∈ R2 such that they are i.i.d. ∼ N([1,√3], I2).
# Label them all in bin 3. Plot the generated features in the form of
# a scatterplot. Represent the features having different labels with
# different colors.


# (b) Compute the classifier using linear classifier with indicator matrices.
# Plot the classifying lines along with the scatter plot of the generated
# features.



library(ggplot2)

# Set seed for reproducibility
set.seed(123)

# Generate data for 3-class classification
X1 <- matrix(rnorm(100), ncol = 2)
X2 <- matrix(rnorm(100, mean = c(2, 0)), ncol = 2)
X3 <- matrix(rnorm(100, mean = c(1, sqrt(3))), ncol = 2)
X <- rbind(X1, X2, X3)
Y <- rep(1:3, each = 50)

# Compute linear classifier using indicator matrices
G <- matrix(0, nrow = 150, ncol = 3)
for (i in 1:3) {
  G[Y == i, i] <- 1
}
beta <- solve(t(X) %*% X) %*% t(X) %*% G

# Create data frame for plotting
df <- data.frame(X, Y = factor(Y))
df$Y <- as.factor(df$Y)

# Plot scatter plot and classifying lines
ggplot(df, aes(x = X1, y = X2, color = Y)) +
  geom_point(size = 3) +
  geom_abline(intercept = -beta[1,1]/beta[2,1], slope = -beta[1,3]/beta[2,1], color = "red") +
  geom_abline(intercept = -beta[1,2]/beta[2,2], slope = -beta[2,3]/beta[2,2], color = "green") +
  labs(x = "X1", y = "X2", color = "Class") +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73"))

