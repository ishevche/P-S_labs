id <- 26
set.seed(id)

simulate_sample <- function(n, graph = FALSE) {
  nu1 <- id + 10
  K <- 1e3
  sample_means <- colMeans(matrix(rexp(n * K, rate = nu1), nrow = n))

  mu <- 1 / nu1
  sigma <- 1 / (nu1 * sqrt(n))

  if (graph) {
    xlims <- c(mu - 3 * sigma, mu + 3 * sigma)
    Fs <- ecdf(sample_means)
    plot(Fs,
         xlim = xlims,
         col = "blue",
         xlab = "value",
         ylab = "P(mean < value)",
         lwd = 2,
         main = paste("Compparation of empitrical cdf of ", K, " X ", n, "sample with normal distrbution cdf"))
    curve(pnorm(x, mean = mu, sd = sigma), col = "red", lwd = 2, add = TRUE)
  }

  x <- seq(min(sample_means), max(sample_means), by = .0001)
  return(max(abs(ecdf(sample_means)(x) - pnorm(x, mean = mu, sd = sigma))))
}

cat(paste("The max difference for n = 5: ", simulate_sample(5), "\n"))
cat(paste("The max difference for n = 10: ", simulate_sample(10), "\n"))
cat(paste("The max difference for n = 50: ", simulate_sample(50), "\n"))
cat(paste("The max difference for n = 1000: ", simulate_sample(1000, TRUE), "\n"))


K <- 1e3
nu1 <- (id + 10)

simulate_geiger <- function(n) {
  K <- 1e3
  nu1 <- (id + 10)
  sums <- colSums(matrix(rexp(100 * K, rate = nu1 * n), nrow = 100))
  return(length(sums[sums >= 1]) / K)
}

N_markov <- 100 / (nu1 * 0.95)
N_chebashev <- (sqrt(5 / 0.95) + 100) / nu1
N_clt <- (uniroot(function(x) pnorm(x) - 0.05, c(-2, -1))$root * 10 + 100) / nu1
N_actual <- uniroot(function(x) simulate_geiger(x) - 0.95,
                    c(0.5, 5))$root


cat(paste("The probability location identified as safe for n calculated by Markov\'s inequality (",
          N_markov, "): ", simulate_geiger(N_markov), "\n"))
cat(paste("The probability location identified as safe for n calculated by Chebyshev\'s inequality (",
          N_chebashev, "): ", simulate_geiger(N_chebashev), "\n"))
cat(paste("The probability location identified as safe for n calculated by CLT (",
          N_clt, "): ", simulate_geiger(N_clt), "\n"))
cat(paste("The probability location identified as safe for n calculated by simulations (",
          N_actual, "): ", simulate_geiger(N_actual), "\n"))