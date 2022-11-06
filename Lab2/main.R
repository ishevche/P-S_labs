id <- 26
set.seed(id)

test <- function(n) {
  lambda <- id + 10
  K <- 1e3
  sample_means <- colMeans(matrix(rexp(n * K, rate = lambda), nrow = n))

  mu <- 1 / lambda
  sigma <- 1 / (lambda * sqrt(n))

  xlims <- c(mu - 3 * sigma, mu + 3 * sigma)
  Fs <- ecdf(sample_means)
  plot(Fs,
       xlim = xlims,
       col = "blue",
       lwd = 2,
       main = "Comparison of ecdf and cdf")
  curve(pnorm(x, mean = mu, sd = sigma), col = "red", lwd = 2, add = TRUE)


  x <- seq(min(sample_means), max(sample_means), by = .0001)
  return(max(abs(ecdf(sample_means)(x) - pnorm(x, mean = mu, sd = sigma))))
}

test(5)
test(10)
test(50)
