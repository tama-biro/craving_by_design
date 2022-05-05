

# Distributions of parameters

# Alpha
c_alpha <- rbeta(200, 22, 3)
hist(c_alpha, main = "Alpha")

# Beta
c_beta <- runif(200, 0, 50)
hist(c_beta, main = "Beta")

# Lambda
c_lambda <- rtruncnorm(200, a = 0, b = Inf, mean = 1.955, sd = 0.14)
hist(c_lambda, main = "Lambda")

# Kappa 1
c_kappa_1 <- runif(200, 0.1, 1)
hist(c_kappa_1, main = "Kappa 1")

# Kappa 2
c_kappa_2 <- runif(200, 0.2, 1)
hist(c_kappa_2, main = "Kappa 2")

# Theta
c_theta <- runif(200, 0.5, 1)
hist(c_theta, main = "Theta")

# Unc
c_unc <- runif(200, 2, 5)
hist(c_unc, main = "Uncertainty")


