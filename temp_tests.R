
kLow <- 50000
kUpp <- 5e5

set.seed(486)
x <- sort(rlnorm(1000, 10, 2))
x_low <- x[which(x > kLow)]
x_upp <- x[which(x < kUpp)]
x_low_upp <- x[which(x > kLow & x < kUpp)]

## Density Testing
dlnorm_trunc(x, 10, 2)
dlnorm_trunc(x, 10, 2, log = TRUE)

dlnorm_trunc(x_low, 10, 2, low = kLow)
dlnorm_trunc(x_low, 10, 2, low = kLow, log = TRUE)
dlnorm_trunc(c(kLow - 10000, x_low), 10, 2, low = kLow)
dlnorm_trunc(c(kLow - 10000, x_low), 10, 2, low = kLow, log = TRUE)

dlnorm_trunc(x_upp, 10, 2, upp = kUpp)
dlnorm_trunc(x_upp, 10, 2, upp = kUpp, log = TRUE)
dlnorm_trunc(c(x_upp, kUpp + 1000), 10, 2, upp = kUpp)
dlnorm_trunc(c(x_upp, kUpp + 1000), 10, 2, upp = kUpp, log = TRUE)
dlnorm_trunc(c(kUpp - 1000, kUpp - 1, kUpp, kUpp + 1, kUpp + 1000), 10, 2, upp = kUpp, log = TRUE)

dlnorm_trunc(x_low_upp, 10, 2, low = kLow, upp = kUpp)
dlnorm_trunc(x_low_upp, 10, 2, low = kLow, upp = kUpp, log = TRUE)
dlnorm_trunc(c(kLow - 1000, x_low_upp, kUpp + 1000), 10, 2, low = kLow, upp = kUpp)
dlnorm_trunc(c(kLow - 1000, x_low_upp, kUpp + 1000), 10, 2, low = kLow, upp = kUpp, log = TRUE)

## Probability Testing
plnorm_trunc(x, meanlog = 10, sdlog = 2)
plnorm_trunc(x, meanlog = 10, sdlog = 2, lower.tail = FALSE)
plnorm_trunc(x, meanlog = 10, sdlog = 2, log.p = TRUE)
plnorm_trunc(x, meanlog = 10, sdlog = 2, lower.tail = FALSE, log.p = TRUE)
plnorm_trunc(x, meanlog = 10, sdlog = 2, low = kLow, upp = kUpp)

plnorm_trunc(x_low, 10, 2, low = kLow)
plnorm_trunc(x_low, 10, 2, low = kLow, lower.tail = FALSE)
plnorm_trunc(x_low, 10, 2, low = kLow, log.p = TRUE)
plnorm_trunc(x_low, 10, 2, low = kLow, lower.tail = FALSE, log.p = TRUE)

plnorm_trunc(x_upp, 10, 2, upp = kUpp)
plnorm_trunc(x_upp, 10, 2, upp = kUpp, lower.tail = FALSE)
plnorm_trunc(x_upp, 10, 2, upp = kUpp, log.p = TRUE)
plnorm_trunc(x_upp, 10, 2, upp = kUpp, lower.tail = FALSE, log.p = TRUE)

plnorm_trunc(x_low_upp, 10, 2, low = kLow, upp = kUpp)
plnorm_trunc(x_low_upp, 10, 2, low = kLow, upp = kUpp, lower.tail = FALSE)
plnorm_trunc(x_low_upp, 10, 2, low = kLow, upp = kUpp, log.p = TRUE)
plnorm_trunc(x_low_upp, 10, 2, low = kLow, upp = kUpp, lower.tail = FALSE, log.p = TRUE)


## Quantile Testing
ps <- seq(0.05, 0.95, by = 0.05)

qlnorm_trunc(ps, 10, 2)
qlnorm_trunc(ps, 10, 2, low = kLow)
qlnorm_trunc(c(ps, 0.9999), 10, 2, upp = kUpp)
qlnorm_trunc(ps, 10, 2, low = kLow, upp = kUpp)
qlnorm_trunc(log(ps), 10, 2, log.p = TRUE)
qlnorm_trunc(1-ps, 10, 2, lower.tail = FALSE)
qlnorm_trunc(log(1-ps), 10, 2, lower.tail = FALSE, log.p = TRUE)


## Random number testing
set.seed(65482)
sort(rlnorm_trunc(100, 10, 2))
sort(rlnorm_trunc(100, 10, 2, low = kLow))
sort(rlnorm_trunc(100, 10, 2, upp = kUpp))
sort(rlnorm_trunc(100, 10, 2, low = kLow, upp = kUpp))
