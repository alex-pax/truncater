

dlnorm_trunc <- function(x, meanlog = 0, sdlog = 1, low = 0, upp = Inf, log = FALSE) {


  if (sdlog < 0) {
    warning(paste0("sdlog: ", sdlog ,". sdlog must be >=0.  NaNs produced."))
    return(rep(NaN, length(x)))
  }


  F_upp <- if (is.infinite(upp)) 1 else (plnorm(upp, meanlog = meanlog, sdlog = sdlog))
  F_low <- if (low <= 0) 0 else (plnorm(low, meanlog = meanlog, sdlog = sdlog))

  f_x   <- dlnorm(x, meanlog = meanlog, sdlog = sdlog)
  ## If x is outside of truncation bounds, then f(x), the density, is defined to
  ##  be zero.
  f_x[which(x <= low | x > upp)] <- 0

  ## Handle case for log-densities
  if (log) {
    log(f_x) - log(F_upp - F_low)
  } else {
    f_x / (F_upp - F_low)
  }

}


plnorm_trunc <- function(q, meanlog = 0, sdlog = 1, low = 0, upp = Inf, lower.tail = TRUE, log.p = FALSE) {

  if (sdlog < 0) {
    warning(paste0("sdlog: ", sdlog ,". sdlog must be >=0.  NaNs produced."))
    return(rep(NaN, length(x)))
  }


  F_upp <- if (is.infinite(upp)) 1 else (plnorm(upp, meanlog = meanlog, sdlog = sdlog))
  F_low <- if (low <= 0) 0 else (plnorm(low, meanlog = meanlog, sdlog = sdlog))

  F_x   <- (plnorm(q, meanlog = meanlog, sdlog = sdlog) - F_low) / (F_upp - F_low)

  ## For x <= low; F_x = 0
  F_x[which(q <= low)] <- 0

  ## For X >= upp; F_x = 1
  F_x[which(q >= upp)] <- 1

  if(lower.tail) {
    out <- F_x
  } else {
    out <- 1 - F_x
  }

  if(log.p) {
    log(out)
  } else {
    out
  }
}

qlnorm_trunc <- function(p, meanlog = 0, sdlog = 1, low = 0, upp = Inf, lower.tail = TRUE, log.p = FALSE) {

  if (sdlog < 0) {
    warning(paste0("sdlog: ", sdlog ,". sdlog must be >=0.  NaNs produced."))
    return(rep(NaN, length(x)))
  }

  if(log.p) {
    p <- exp(p)
  }

  if(!lower.tail) {
    p <- (1 - p)
  }

  F_upp <- if (is.infinite(upp)) 1 else (plnorm(upp, meanlog = meanlog, sdlog = sdlog))
  F_low <- if (low <= 0) 0 else (plnorm(low, meanlog = meanlog, sdlog = sdlog))

  p <- p * (F_upp - F_low) + F_low

  qlnorm(p, meanlog = meanlog, sdlog = sdlog, lower.tail = TRUE, log.p = FALSE)

}

rlnorm_trunc <- function(n, meanlog = 0, sdlog = 1, low = 0, upp = Inf) {

  rands <- runif(n)

  qlnorm_trunc(rands, meanlog = meanlog, sdlog = sdlog, low = low, upp = upp)

}
