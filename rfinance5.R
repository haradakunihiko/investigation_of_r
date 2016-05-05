n <- 10000
# Fixing the seed gives us a consistent set of simulated returns
set.seed(106)
z <- rnorm(n)        # mean = 0 and sd = 1 are defaults

mu <- 0.10
sd <- 0.15
delta_t <- 0.25
# apply to expression (*) above
qtr_returns <- mu*delta_t + sd*z*sqrt(delta_t)  

hist(qtr_returns, breaks = 100, col = "green")

mean(qtr_returns) * 4
sd(qtr_returns) * sqrt(4)
  