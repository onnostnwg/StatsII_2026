#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

set.seed(123)

data <- rcauchy(1000, location = 0, scale = 1)

ks_normal_test <- function(data) {
  data_sorted <- sort(data)
  n <- length(data_sorted)
  ECDF <- ecdf(data_sorted)
  empiricalCDF <- ECDF(data_sorted)
  theoreticalCDF <- pnorm(data_sorted, mean = 0, sd = 1)
  D <- max(abs(empiricalCDF - theoreticalCDF))
  return(D)}

D_value <- ks_normal_test(data)
D_value


ks_pvalue <- function(D, n, tol = 1e-6) {
  lambda <- (sqrt(n) + 0.12 + 0.11 / sqrt(n)) * D
  k <- 1
  sum <- 0
  term <- 1
  while(term > tol) {
    term <- 2 * (-1)^(k-1) * exp(-2 * k^2 * lambda^2)
    sum <- sum + term
    k <- k + 1
  }
  return(sum)
}

p_value <- ks_pvalue(D_value, length(data))
p_value

#####################
# Problem 2
#####################

set.seed(123)
data <- data.frame(x = runif(200, 1, 10))
data$y <- 0 + 2.75 * data$x + rnorm(200, 0, 1.5)

ols_lm <- lm(y ~ x, data = data)
coef(ols_lm)

neg_loglik <- function(params, y, X) {
  beta <- params[1:ncol(X)]
  log_sigma <- params[ncol(X) + 1]
  sigma <- exp(log_sigma)
  n <- length(y)
  residuals <- y - X %*% beta
  -(-n/2*log(2*pi*sigma^2) - sum(residuals^2)/(2*sigma^2))}

X <- cbind(1, data$x)
y <- data$y

start <- c(0, 0, log(1.5))

fit <- optim(start, neg_loglik, y = y, X = X, method = "BFGS", hessian = TRUE)

beta_hat <- fit$par[1:2]
sigma_hat <- exp(fit$par[3])

beta_hat
sigma_hat


