#
# Oscar Celma
# Implementation of the Long Tail model, F(x)
# http://ocelma.net/PhD/
# Article reference: http://firstmonday.org/htbin/cgiwrap/bin/ojs/index.php/fm/article/view/1832/1716
#

# Load functions: F(x), N50() and GetRank()
source("include/longtail-functions.R")

# Read long tail example data file
w1 = read.table("data/long_tail.data")
a = c(w1$V1)

# Plot the long-tail in log-lin
plot(a, xlab='Ranking', ylab='Number of plays', log="x", main="Last.fm Popularity", sub="The long-tail effect")

# Plot the long-tail in log-log (does it "look like" a power-law? log-normal? etc.)
plot(a, xlab='Ranking', ylab='Number of plays', log="xy", main="Last.fm Popularity", sub="The long-tail effect")

# Compute cumulative and cumulative percent from data in 'a'
cum = cumsum(as.numeric(a))
cum_pcnt = cum / cum[length(cum)] * 100

# Plot cumulative percentage of plays
plot(cum_pcnt, xlab='Ranking', ylab='Cumulative (percentatge) of plays', log="x", main="Last.fm Popularity", sub="The long-tail effect")

# F(x) model: Create the non-linear model for the observations
# Set initial values:
n50 = N50(cum_pcnt, 0.0001)
beta = 1
alfa = 0.75

# Set x (rank) and y (cumulative percentage) vector values
rank = 1:length(a)
dataset = data.frame(cum_pcnt)
# Fit the observations with the F(x) model
fit = nls( cum_pcnt ~ F(rank, n50, beta, alfa), data = dataset, start = list(n50=n50, beta=beta, alfa=alfa), trace = TRUE )

# Get the results of the non-linear regression
residuals = summary(fit)$residuals
params = summary(fit)$parameters

fit_params = summary(fit)$parameters[, 1]
n50 =  fit_params[1] # estimated N50
beta = fit_params[2] # estimated Beta
alfa = fit_params[3] # estimated Alfa

####################
# Article Appendix #
####################

# Get boundaries between Head/Mid, and Mid/Tail sections
x_bm = round( n50^(2/3) )
x_me = round( n50^(4/3) )

# This can be slow, depending on how much data you have!
f = c(0)
for (i in 1:length(a)) f[i] = F(i, n50, beta, alfa)

plot(f, log="x", type='l', xlab='Ranking', ylab='Cumulative percentage of plays', main="Last.fm Popularity", sub="F(x) model and the real points", col='blue')
lines(rank, cum_pcnt, col="red")
lines(x_bm, cum_pcnt[x_bm], type = "b", lty = 1, pch = 8, col = 'black')
lines(x_me, cum_pcnt[x_me], type = "b", lty = 1, pch = 8, col = 'black')

# Reconstruct the long-tail:
# This can be *very* slow, depending on how much data you have!
#f_orig = c(0)
#for (i in 1:length(a)) f_orig[i] = F(i, n50, beta, alfa) - F(i-1, n50, beta, alfa)
#plot(a, xlab='Ranking', ylab='Number of plays', log="x", main="Last.fm Popularity", sub="The long-tail effect")
#plot(f_orig, log="x", type='l', xlab='Ranking', ylab='F(x) - F(x-1)', main="Last.fm Popularity", sub="Reconstructing original long-tail", col='blue')
