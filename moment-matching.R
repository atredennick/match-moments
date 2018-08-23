get_gamma_params <- function(mu, stddev){
  sh <- mu^2 / stddev^2
  ra <- mu / stddev^2
  return(c(sh,ra))
}


mu = 15
stddev = 8
pars <- get_gamma_params(mu, stddev)
hist(rgamma(1000, shape = pars[1], rate = pars[2]))


get_beta_params <- function(mu, stddev){
  var <- stddev^2
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(c(alpha, beta))
}

mu = 0.95
stddev = 0.05
pars <- get_beta_params(mu, stddev)
hist(rbeta(1000, shape1 = pars[1], shape2 = pars[2]))
