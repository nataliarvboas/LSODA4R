setwd("~/Desktop/LSODAForRLib/build")
library(Rcpp)
dyn.load("libLSODA4R.so")

step_solver <- function(ode, t0, tf, y, rtol = 1e-5, atol = 1e-6) {
  .Call("Lsoda", as.double(t0), as.double(tf), as.double(y), as.double(rtol), as.double(atol), environment())
}


fun <- function(t, y) {
  dydt <- c(0, 0)
  dydt[1] <- y[1] - t * t;
  dydt[2] <- -y[2] + t;
  return(dydt)
}

t0 <- 0
tf <- 3.25
dt <- 0.25

t <- seq(t0, tf, dt)
np <- length(t)

y1 <- rep(0, np)
y2 <- rep(0, np)
y1[1] <- 1
y2[1] <- 1

for (i in 1:(np-1)) {
  res <- step_solver(fun, t[i], t[i+1], c(y1[i], y2[i]))
  y1[i + 1] <- res[1]
  y2[i + 1] <- res[2]
}

plot(t, y1, type = "o", col = "blue", xlim = c(0,3.5), ylim = c(-7, 4))
lines(t, y2, type = "o", col="red")