# https://cran.r-project.org/web/packages/mlma/vignettes/MLMAvignette.html

install.packages("mlma")

library(Matrix)
library(lme4)  # Required for mixed models
library(mlma)
loadedNamespaces()

# Set seed for reproducibility
set.seed(1)

# Define parameters
n <- 20         # Number of observations per group
J <- 600 / n    # Number of groups (30 groups)
level <- rep(1:J, each = n)

# Coefficients for covariates
alpha_211 <- 0.8
alpha_1111 <- 0.8
alpha_2111 <- 0.8
beta_1 <- 0.4
beta_2 <- 0.4
beta_3 <- 0.4
beta_4 <- 0.4
beta_5 <- 0.4

# Variance definitions
v1 <- 5        # Level 1 variance
v2 <- v1 / 5   # Level 2 variance

# Generate exposure variables
x1 <- rbinom(600, 1, 0.5)  # Binary level 1 exposure
x2 <- rep(rnorm(J), each = n)  # Continuous level 2 exposure

# Generate mediators
m2 <- rep(rbinom(J, 1, exp(alpha_211 * unique(x2^2)) / (1 + exp(alpha_211 * unique(x2^2)))), each = n)  # Binary level 2 mediator
u1 <- rep(rnorm(J, 0, 0.5), each = n)  # Level 2 variance for m1
e1 <- rnorm(n * J)  # Level 1 variance for m1
m1 <- u1 + alpha_1111 * x1 + alpha_2111 * x2 + e1  # Continuous level 1 mediator

# Generate response variable
y <- rep(rnorm(J, 0, v2), each = n) + 
     beta_1 * x1 + beta_2 * x2 + 
     beta_3 * ifelse(x2 <= 0, 0, log(1 + x2)) + 
     beta_4 * m1 + beta_5 * m2 + 
     rnorm(n * J, 0, v1)

# Data Transformation and Organization
example1 <- data.org(
  x = cbind(x1 = x1, x2 = x2), 
  m = cbind(m1 = m1, m2 = m2), 
  f01y = list(2, c("x", "ifelse(x > 0, log(x + 1), 0)")), 
  level = level, 
  f01km2 = list(matrix(c(2, 2), 1, 2), "x^2")
)

# Perform multilevel mediation analysis
mlma.e1 <- mlma(y = y, data1 = example1, intercept = FALSE)

# Display results
print(mlma.e1)

# Summary of mediation analysis
summary(mlma.e1)

