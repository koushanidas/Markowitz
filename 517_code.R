install.packages("gridExtra")
install.packages("data.table")
library(data.table)
library(scales)
library(ggplot2)
dt <- data.table(read.csv("D:\\ticker_list.csv"))

print(dt)
dt[, date := as.Date(date)]
# create indexed values

# create indexed values
dt[, idx_price := price/price[1], by = ticker]
print(dt)
# plot the indexed values
# plot the indexed values
ggplot(dt, aes(x = date, y = idx_price, color = ticker)) +
  geom_line() +
  # Miscellaneous Formatting
  theme_bw() + ggtitle("Price Developments") +
  xlab("Date") + ylab("Index price") +
  scale_color_discrete(name = "Company")


# calculate the arithmetic returns
dt[, ret := price / shift(price, 1) - 1, by = ticker]
# summary table
# take only non-na values
tab <- dt[!is.na(ret), .(ticker, ret)]
# calculate the expected returns (historical mean of returns) and volatility (standard deviation of returns)
tab <- tab[, .(er = round(mean(ret), 4),
               sd = round(sd(ret), 4)),
           by = "ticker"]
print(tab)
ggplot(tab, aes(x = sd, y = er, color = ticker)) +
  geom_point(size = 5) +
  # Miscellaneous Formatting
  theme_bw() + ggtitle("Risk-Return Tradeoff") +
  xlab("Volatility") + ylab("Expected Returns") +
  scale_y_continuous(label = percent, limits = c(0, 0.03)) +
  scale_x_continuous(label = percent, limits = c(0, 0.1))



# load the functions, libraries etc
#source("https://github.com/DavZim/Efficient_Frontier/blob/master/R/functions.R")
library(data.table)
library(ggplot2)
library(gridExtra)
library(magrittr)
library(scales)

#' Downloads Yahoo Stock Data
#' The function downloads adjusted stock prices for given stocks
#'
#' @param tickers a vector of tickers (yahoo-syntax)
#' @param long a boolean if the data should be returned in a wide or long format
#'
#' @return a data.table containing the date, ticker and prices (long) or date and one column with prices per ticker (wide)
#' @export
#'
#' @examples
#'
#' getData(tickers = c("IBM", "MSFT"), long = T)
#'
getData <- function(tickers, long = T) {
  # iterate through the tickers and get the last adjusted price in a data.table
  res <- lapply(tickers, function(x) {
    
    dat <- getSymbols(x, from = "2000-01-01", auto.assign = F)
    
    dt <- data.table(date = as.Date(index(dat)),
                     ticker = x,
                     price = as.numeric(Ad(dat)))
    return(dt)
  })
  
  # combine the list to one data.table
  res <- rbindlist(res)
  
  # cast the data if the user wants to get the data in a wide format
  if (!long) {
    res <- dcast(res, date ~ ticker)
  }
  
  return(res)
}


#' Calculate correlated Random Values
#'
#' The function calculates random values that are correlated to an input
#' @param x a vector of values
#' @param r the target correlation
#' @param y_mean the mean of the random values
#' @param y_sd the standard deviation of the random values
#'
#' @return a vector with the same length as x that contains random values
#' @export
#'
#' @examples
#' set.seed(123)
#' x <- rnorm(1000)
#' y <- rmultvar(x, r = 0.7, y_mean = 10.5, y_sd = 1)
#'
#' # test
#' cor(x, y) # 0.727
#' mean(y) # 10.5
#' sd(y) # 1.047
#'
rmultvar = function(x, r, y_mean, y_sd){
  # inspired by gung
  # http://stats.stackexchange.com/questions/38856/how-to-generate-correlated-random-numbers-given-means-variances-and-degree-of
  
  x2 <- (x -  mean(x)) / sd(x)
  r2 <- r ^ 2
  ve <- 1 - r2
  SD <- sqrt(ve)
  e <- rnorm(length(x2), mean = 0, sd = SD)
  y <- r*x2 + e
  
  y <- (y - mean(y)) * y_sd + y_mean
  return(y)
}


#' Calculate the abcd-list for the Efficient Frontier
#' The function calculates A, B, C, and Delta for a given set of returns
#' @param x a data.table with the columsn date, ticker, and returns
#'
#' @return a list of alpha, beta, gamma, and delta that can be used to compute the efficient frontier
#' @export
#'
#' @examples
#'
calcEFParamsLong <- function(x) {
  x <- x[is.finite(ret), .(date, ticker, ret)]
  
  rets <- dcast(x, formula = date ~ ticker, value.var = "ret")[, date := NULL]
  
  retbar <- colMeans(rets, na.rm = T)
  covs <- var(rets, na.rm = T) # calculates the covariance of the returns
  invS <- solve(covs)
  i <- matrix(1, nrow = length(retbar))
  
  alpha <- t(i) %*% invS %*% i
  beta <- t(i) %*% invS %*% retbar
  gamma <- t(retbar) %*% invS %*% retbar
  delta <- alpha * gamma - beta * beta
  
  retlist <- list(alpha = as.numeric(alpha),
                  beta = as.numeric(beta),
                  gamma = as.numeric(gamma),
                  delta = as.numeric(delta))
  
  return(retlist)
}


#' Calculate the Efficient Frontier Values
#' The function calculates the y-values for an efficient frontier for given x-values
#' @param xvals a vector of x-values 
#' @param abcd a list of the values for the efficient frontier as outputted by calcEFParamsLong
#' @param upper a boolean value if the upper (efficient) or lower (inefficient) frontier should be returned
#'
#' @return a vector of y-values for the efficient frontier
#' @export
#'
#' @examples
calcEFValues <- function(x, abcd, upper = T) {
  alpha <- abcd$alpha
  beta <- abcd$beta
  gamma <- abcd$gamma
  delta <- abcd$delta
  
  if (upper) {
    retval <- beta / alpha + sqrt((beta / alpha) ^ 2 - (gamma - delta * x ^ 2) / (alpha))
  } else {
    retval <- beta / alpha - sqrt((beta / alpha) ^ 2 - (gamma - delta * x ^ 2) / (alpha))
  }
  
  return(retval)
}


#' Creates a plot of different efficient frontiers given the tickers
#'
#' @param dat a data.table containing at least two columns ticker and ret (returns)
#' @param tickers a vector of tickers that are compared (length 2)
#'
#' @return a ggplot of the two points
#' @export
#'
#' @examples
plotCombinations <- function(dat, tickers) {
  dat <- dat[ticker %in% tickers]
  
  list1 <- calcABCDs(dat)
  tabs <- dat[, .(mean = mean(ret), sd = sd(ret)), by = "ticker"]
  
  dfUpper <- data.table(x = seq(from = 0, to = max(tabs$sd), length.out = 10000))
  dfLower <- data.table(x = seq(from = 0, to = min(tabs$sd), length.out = 10000))
  
  dfUpper[, y := calcEffPoints(x, list1, upper = T)]
  dfLower[, y := calcEffPoints(x, list1, upper = F)]
  
  # trim values below the lower point
  y_min <- dat[, mean(ret), by = ticker][, min(V1)]
  
  dfUpper <- dfUpper[y >= y_min]
  dfLower <- dfLower[y >= y_min]
  
  correl <- cor(dat[ticker == tickers[1], ret], dat[ticker == tickers[2], ret]) %>% round(2)
  
  ggplot() +
    geom_line(data = dfUpper, aes(x = x, y = y), linetype = "dashed") +
    geom_line(data = dfLower, aes(x = x, y = y), linetype = "dashed") +
    geom_point(data = tabs, aes(x = sd, y = mean), color = "red", shape = 16) +
    theme_bw() + geom_hline(yintercept = 0, color = "darkgrey") +
    geom_vline(xintercept = 0, color = "darkgrey") +
    ggtitle(paste0("Correlation: ", correl)) +
    xlab("Volatility") + ylab("Expected Returns") +
    scale_y_continuous(label = percent, limits = c(0, max(tabs$mean) * 1.2)) +
    scale_x_continuous(label = percent, limits = c(0, max(tabs$sd) * 1.2))
  
}
set.seed(12345)
df <- data.table(x = rnorm(10000, mean = 0.07, sd = 0.05))

y_mean <- 0.03
y_sd <- 0.02

z_mean <- 0.04
z_sd <- 0.03

df[, y := rmultvar(x, r = 0, y_mean, y_sd)]
df[, z := rmultvar(x, r = 0, z_mean, z_sd)]

write.csv(df, file = "D:\\mult_assets.csv", row.names = F)

link <- "D:\\mult_assets.csv"
df <- data.table(read.csv(link))
# calculate the necessary values:
# I) expected returns for the two assets
er_x <- mean(df$x)
er_y <- mean(df$y)
# II) risk (standard deviation) as a risk measure
sd_x <- sd(df$x)
sd_y <- sd(df$y)
# III) covariance
cov_xy <- cov(df$x, df$y)
# create 1000 portfolio weights (omegas)
x_weights <- seq(from = 0, to = 1, length.out = 1000)
# create a data.table that contains the weights for the two assets
two_assets <- data.table(wx = x_weights,
                         wy = 1 - x_weights)
# calculate the expected returns and standard deviations for the 1000 possible portfolios
two_assets[, ':=' (er_p = wx * er_x + wy * er_y,
                   sd_p = sqrt(wx^2 * sd_x^2 +
                                 wy^2 * sd_y^2 +
                                 2 * wx * (1 - wx) * cov_xy))]
two_assets

# lastly plot the values
ggplot() +
  geom_point(data = two_assets, aes(x = sd_p, y = er_p, color = wx)) +
  geom_point(data = data.table(sd = c(sd_x, sd_y), mean = c(er_x, er_y)),
             aes(x = sd, y = mean), color = "red", size = 3, shape = 18) +
  # Miscellaneous Formatting
  theme_bw() + ggtitle("Possible Portfolios with Two Risky Assets") +
  xlab("Volatility") + ylab("Expected Returns") +
  scale_y_continuous(label = percent, limits = c(0, max(two_assets$er_p) * 1.2)) +
  scale_x_continuous(label = percent, limits = c(0, max(two_assets$sd_p) * 1.2)) +
  scale_color_continuous(name = expression(omega[x]), labels = percent)






##########33 short selling
calcEFParams <- function(rets) {
  retbar <- colMeans(rets, na.rm = T)
  covs <- var(rets, na.rm = T) # calculates the covariance of the returns
  invS <- solve(covs)
  i <- matrix(1, nrow = length(retbar))
  alpha <- t(i) %*% invS %*% i
  beta <- t(i) %*% invS %*% retbar
  gamma <- t(retbar) %*% invS %*% retbar
  delta <- alpha * gamma - beta * beta
  retlist <- list(alpha = as.numeric(alpha),
                  beta = as.numeric(beta),
                  gamma = as.numeric(gamma),
                  delta = as.numeric(delta))
  return(retlist)
}
# load data
#link <- "https://raw.githubusercontent.com/DavZim/Efficient_Frontier/master/data/mult_assets.csv"
df <- data.table(read.csv(link))
abcds <- calcEFParams(df)
abcds
## $alpha
## [1] 4037.551
##
## $beta
## [1] 147.8334
##
## $gamma
## [1] 5.992395
##
## $delta
## [1] 2339.881
calcEFValues <- function(x, abcd, upper = T) {
  alpha <- abcd$alpha
  beta <- abcd$beta
  gamma <- abcd$gamma
  delta <- abcd$delta
  if (upper) {
    retval <- beta / alpha + sqrt((beta / alpha) ^ 2 - (gamma - delta * x ^ 2) / (alpha))
  } else {
    retval <- beta / alpha - sqrt((beta / alpha) ^ 2 - (gamma - delta * x ^ 2) / (alpha))
  }
  return(retval)
}
# calculate the risk-return tradeoff the two assets (for plotting the points)
df_table <- melt(df)[, .(er = mean(value),
                         sd = sd(value)), by = variable]
# plot the values
ggplot(df_table, aes(x = sd, y = er)) +
  # add the stocks
  geom_point(size = 4, color = "red", shape = 18) +
  # add the upper efficient frontier
  stat_function(fun = calcEFValues, args = list(abcd = abcds, upper = T), n = 10000,
                color = "red", size = 1) +
  # add the lower "efficient" frontier
  stat_function(fun = calcEFValues, args = list(abcd = abcds, upper = F), n = 10000,
                color = "blue", size = 1) +
  # Miscellaneous Formatting
  theme_bw() + ggtitle("Efficient Frontier with Short-Selling") +
  xlab("Volatility") + ylab("Expected Returns") +
  scale_y_continuous(label = percent, limits = c(0, max(df_table$er) * 1.2)) +
  scale_x_continuous(label = percent, limits = c(0, max(df_table$sd) * 1.2))



##### 3 assets

df <- data.table(read.csv(link))
# calculate the necessary values:
# I) expected returns for the two assets
er_x <- mean(df$x)
er_y <- mean(df$y)
er_z <- mean(df$z)
# II) risk (standard deviation) as a risk measure
sd_x <- sd(df$x)
sd_y <- sd(df$y)
sd_z <- sd(df$z)
# III) covariance
cov_xy <- cov(df$x, df$y)
cov_xz <- cov(df$x, df$z)
cov_yz <- cov(df$y, df$z)
# create portfolio weights (omegas)
x_weights <- seq(from = 0, to = 1, length.out = 1000)
# create a data.table that contains the weights for the three assets
three_assets <- data.table(wx = rep(x_weights, each = length(x_weights)),
                           wy = rep(x_weights, length(x_weights)))
three_assets[, wz := 1 - wx - wy]
# calculate the expected returns and standard deviations for the 1000 possible portfolios
three_assets[, ':=' (er_p = wx * er_x + wy * er_y + wz * er_z,
                     sd_p = sqrt(wx^2 * sd_x^2 +
                                   wy^2 * sd_y^2 +
                                   wz^2 * sd_z^2 +
                                   2 * wx * wy * cov_xy +
                                   2 * wx * wz * cov_xz +
                                   2 * wy * wz * cov_yz))]
# take out cases where we have negative weights (shortselling)
three_assets <- three_assets[wx >= 0 & wy >= 0 & wz >= 0]
three_assets

# lastly plot the values
ggplot() +
  geom_point(data = three_assets, aes(x = sd_p, y = er_p, color = wx - wz)) +
  geom_point(data = data.table(sd = c(sd_x, sd_y, sd_z), mean = c(er_x, er_y, er_z)),
             aes(x = sd, y = mean), color = "red", size = 3, shape = 18) +
  # Miscellaneous Formatting
  theme_bw() + ggtitle("Possible Portfolios with Three Risky Assets") +
  xlab("Volatility") + ylab("Expected Returns") +
  scale_y_continuous(label = percent, limits = c(0, max(three_assets$er_p) * 1.2)) +
  scale_x_continuous(label = percent, limits = c(0, max(three_assets$sd_p) * 1.2)) +
  scale_color_gradientn(colors = c("red", "blue", "yellow"),
                        name = expression(omega[x] - omega[z]), labels = percent)

install.packages("tseries")
library(tseries)

##################
calcEFParams <- function(rets) {
  retbar <- colMeans(rets, na.rm = T)
  covs <- var(rets, na.rm = T) # calculates the covariance of the returns
  invS <- solve(covs)
  i <- matrix(1, nrow = length(retbar))
  alpha <- t(i) %*% invS %*% i
  beta <- t(i) %*% invS %*% retbar
  gamma <- t(retbar) %*% invS %*% retbar
  delta <- alpha * gamma - beta * beta
  retlist <- list(alpha = as.numeric(alpha),
                  beta = as.numeric(beta),
                  gamma = as.numeric(gamma),
                  delta = as.numeric(delta))
  return(retlist)
}
# load data
link <- "https://raw.githubusercontent.com/DavZim/Efficient_Frontier/master/data/mult_assets.csv"
df <- data.table(read.csv(link))
abcds <- calcEFParams(df)
abcds
## $alpha
## [1] 4037.551
##
## $beta
## [1] 147.8334
##
## $gamma
## [1] 5.992395
##
## $delta
## [1] 2339.881
calcEFValues <- function(x, abcd, upper = T) {
  alpha <- abcd$alpha
  beta <- abcd$beta
  gamma <- abcd$gamma
  delta <- abcd$delta
  if (upper) {
    retval <- beta / alpha + sqrt((beta / alpha) ^ 2 - (gamma - delta * x ^ 2) / (alpha))
  } else {
    retval <- beta / alpha - sqrt((beta / alpha) ^ 2 - (gamma - delta * x ^ 2) / (alpha))
  }
  return(retval)
}
# calculate the risk-return tradeoff the two assets (for plotting the points)
df_table <- melt(df)[, .(er = mean(value),
                         sd = sd(value)), by = variable]
# plot the values
ggplot(df_table, aes(x = sd, y = er)) +
  # add the stocks
  geom_point(size = 4, color = "red", shape = 18) +
  # add the upper efficient frontier
  stat_function(fun = calcEFValues, args = list(abcd = abcds, upper = T), n = 10000,
                color = "red", size = 1) +
  # add the lower "efficient" frontier
  stat_function(fun = calcEFValues, args = list(abcd = abcds, upper = F), n = 10000,
                color = "blue", size = 1) +
  # Miscellaneous Formatting
  theme_bw() + ggtitle("Efficient Frontier with Short-Selling") +
  xlab("Volatility") + ylab("Expected Returns") +
  scale_y_continuous(label = percent, limits = c(0, max(df_table$er) * 1.2)) +
  scale_x_continuous(label = percent, limits = c(0, max(df_table$sd) * 1.2))



###############

library(tseries)
##
##     'tseries' version: 0.10-34
##
##     'tseries' is a package for time series analysis and
##     computational finance.
##
##     See 'library(help="tseries")' for details.
#link <- "https://raw.githubusercontent.com/DavZim/Efficient_Frontier/master/data/mult_assets.csv"
df <- data.table(read.csv(link))
print(df)
df_table <- melt(df)[, .(er = mean(value),
                         sd = sd(value)), by = variable]
er_vals <- seq(from = min(df_table$er), to = max(df_table$er), length.out = 1000)
# find an optimal portfolio for each possible possible expected return
# values are explicitly set between the minimum and maximum of the expected returns per asset)
sd_vals <- sapply(er_vals, function(er) {
  op <- portfolio.optim(as.matrix(df), er)
  return(op$ps)
})
plot_dt <- data.table(sd = sd_vals, er = er_vals)
# find the lower and the upper frontier
minsd <- min(plot_dt$sd)
minsd_er <- plot_dt[sd == minsd, er]
plot_dt[, efficient := er >= minsd_er]
plot_dt

ggplot() +
  geom_point(data = plot_dt[efficient == F], aes(x = sd, y = er), size = 0.5, color = "blue") +
  geom_point(data = plot_dt[efficient == T], aes(x = sd, y = er), size = 0.5, color = "red") +
  geom_point(data = df_table, aes(x = sd, y = er), size = 4, color = "red", shape = 18) +
  # Miscellaneous Formatting
  theme_bw() + ggtitle("Efficient Frontier without Short-Selling") +
  xlab("Volatility") + ylab("Expected Returns") +
  scale_y_continuous(label = percent, limits = c(0, max(df_table$er) * 1.2)) +
  scale_x_continuous(label = percent, limits = c(0, max(df_table$sd) * 1.2))