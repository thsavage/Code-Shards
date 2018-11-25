# Session 1: 

# Getting Used to R and the R Studio IDE.

library(PASWR2)
library(MASS)
library(repmis)
library(latex2exp)
library(dplyr)
library(ggplot2)
library(tidyverse)

getwd() # Identify the working directory.

set.seed(1492) # Set seed makes results reproducible.
ruv <- runif(n = 20, min = 0, max = 1) # Generate a uniform[0, 1] RV with 20 draws.
round(ruv, 4) # Round answers to 4 decimals places. 

(7 * 3) + 12/2 - 7^2 + sqrt(4) # R can act as basic calculator.

x <- 1:5 # Generate a list from 1 to 5.
y <- x + rnorm(n = 5, mean = 0, sd = .5) # Add normal errors.
model <- lm(y ~ x) # Regress y onto x.
model # Print results

nv <- c(1, 3, 6, 8) # Numeric list
cv <- c("a", "d", "f", "p") # Character list
lv <- c(TRUE, FALSE, FALSE, TRUE) # Logical list
DF1 <- data.frame(nv, cv, lv) # Create an R dataframe
DF1 # Print out the dataframe.
str(DF1) # Describe its contents.

DF1$nv # Dollar sign prefix.

DF1$cv # Again.

attach(DF1) # Attach data in memory

nv

cv

detach(DF1) # Detach data from memory.

library(MASS) # Load in MASS library.
help(Animals) # Example data.
head(Animals) # List out first 6 observations.

library(fredr) # Another library to import data from FRED
fredr_set_key('30e6ecb242a73869e11cb35f6aa3afc3') # My key, please don't abuse.
gdp_pc = fredr('A229RX0A048NBEA') # Grab GDP per capita
attach(gdp_pc) # Attach and plot.
plot(date, value)

plot(date, value, col = 'red', pch=16)

plot(date, value, col = 'blue', pch=16)

detach(gdp_pc)


unrate <- fredr(series_id = "UNRATE", observation_start = as.Date("1990-01-01"))
plot(unrate$date, unrate$value, col = 'blue', pch=16, ylab = "Rate", xlab = "Date")


tenyear <- fredr(series_id = "DGS10", observation_start = as.Date("1990-01-01"))
plot(tenyear$date, tenyear$value, col = 'blue', pch=16, ylab = "Rate", xlab = "Date")


fredr_series_search_text(
  search_text = "federal funds",
  order_by = "popularity",
  sort_order = "desc",
  limit = 1) %>%
  pull(id) %>%
  fredr(series_id = .)


fedfunds <- fredr(series_id = "FEDFUNDS", observation_start = as.Date("1990-01-01"))
plot(fedfunds$date, fedfunds$value, col = 'blue', pch=16, ylab = "Rate", xlab = "Date")


ggplot(data = fedfunds, mapping = aes(x = date, y = value, color = series_id)) +
geom_line() + labs(x = "Date", y = "Rate", color = "Series")


griliches = read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/Ecdat/Griliches.csv")
attach(griliches)
str(griliches)
summary(griliches)
hist(lw80)
hist(lw80, freq = F)
hist(lw80, freq = F, breaks=40, col = 'blue')
griliches$age2 = griliches$age^2 # Generate an additional variable, the square of age.

attach(griliches)
plot(age, lw80, col='blue', pch=16, xlab="Age", ylab="Log of Hourly Wages", main = "A Scatterplot")
plot(age2, lw80, col='blue', pch=16, xlab="Age Squared", ylab="Log of Hourly Wages", main = "A Scatterplot")
detach(griliches)


d <- read_csv("https://stats.idre.ucla.edu/stat/data/hsbraw.csv")
d
mean(d$read)
median(d$read)
var(d$read)


# Some very useful code for plotting the parabola: y = x ^ 2.
par(mfrow=c(3, 3), pty = "m")  #3 by 3 layout
x <- -4:4
y <- x^2
plot(x, y, xlim=c(-8, 8), ylim = c(0, 20), main ="")
title(main = "Default values with limits \n for x and y axes altered")
plot(x, y, pch = "x", xlim=c(-8, 8), ylim = c(0, 20), main="")
title(main = "Default plotting character \n changed to x")
plot(x, y, type = "l", xlim = c(-8, 8), ylim = c(0, 20), main="")
title(main = "Lines connecting the data")
plot(x, y, type = "b", xlim = c(-8, 8), ylim = c(0, 20), main="")
title(main = "Both point and lines \n between data")
plot(x, y, type = "h", xlim = c(-8, 8), ylim = c(0, 20), main="")
title(main = "Vertical lines")
plot(x, y, type = "o", xlim = c(-8, 8), ylim = c(0, 20), main="")
title(main = "Overlaid points \n and connected lines")
plot(x, y, type = "s", xlim = c(-8, 8), ylim = c(0, 20), main="")
title(main = "Stairsteps")
plot(x, y, xlim = c(-8, 8), ylim = c(0, 20), main = "", xlab = "X Axis",
     ylab = "Y Axis")
title(main = "Basic plot with axes labeled")
plot(x, y, type = "n", xlim = c(-8, 8), ylim = c(0, 20), xlab = "",
     ylab = "", main = "")
title(main = "Empty Graph \n(No Plotted Points)")
text(0, 16, "Some Red Text", col = "red")
text(0, 10, expression(paste("Some Math: ", bar(x)==sum(frac(x[i],
                                                             n), i==1, n))))
Alpha <- round(mean(y), 3)
text(0, 3, bquote(paste("The Mean: ", bar(y)==.(Alpha))))
par(mfrow=c(1, 1))


# Colors and points.
# figure margins of 2.2, 2.2, 0.2, and 0.2 lines
par(mar=c(2, 2, 0, 0) + 0.2)
plot(x = 1, y = 1, xlim = c(1, 16), ylim = c(-1.5, 5), type = "n",
     xlab = "", ylab = "")  # create empty plot with x and y axes
COLORS <- c("black", "red", "green", "darkblue", "darkgreen",
            "magenta", "orange", "cyan")  # vector of colors
# symbols (pch = 0:7) placed at (1, 4), (3, 4), ...(15, 4) with
# character expansion 1:8 with color specified in COLORS
points(x = seq(1, 15, 2), y = rep(4, 8), cex = 1:8, col = COLORS,
       pch = 0:7, lwd = 2)
# labels 0:7 placed at (1, 2), (3, 2),..., (15, 2) with
# character expansion 1:8 with color specified in COLORS
text(x = seq(1, 15, 2), y = rep(2, 8), labels = paste(0:7), cex = 1:8,
     col = COLORS)
# symbols (pch = 8:15) placed at (1, 0), (3, 0),..., (15, 0)
# with character expansion of 2
points(x = seq(1, 15, 2), y = rep(0, 8), pch = 8:15, cex = 2)
# labels 8:15 placed 0.7 to the right of (1, 0), (3, 0),..., (15, 0)
# with character expansion of 2
text(x = seq(1, 15, 2) + 0.7, y = rep(0, 8), labels = paste(8:15),
     cex = 2)
# symbols (pch = 16:23) placed at (1, -1), (3, -1),..., (15, -1)
# with character expansion of 2
points(x = seq(1, 15, 2), y = rep(-1, 8), pch = 16:23, cex = 2)
# labels 16:23 placed 0.7 to the right of (1, -1), (3, -1),..., (15, -1)
# with character expansion of 2
text(x = seq(1, 15, 2) + 0.7, y = rep(-1, 8), labels = paste(16:23),
     cex = 2)


# In-class lab.  Import mtcars data and generate some plots.  Fit a linear model.
data("mtcars")
attach(mtcars)
plot(mpg, cyl)



# Foundational Concepts from Probability and Statistics.

# Birthday example.
m <- 1:60          # Number of students
p <- numeric(60)   # Initialize probability vector to 0's
for(i in m){       # index values for loop
  q = prod((365:(365 - i + 1))/365) # P(No Match) if i people in room
  p[i] = 1 - q}

plot(m, p, col = "blue", pch = 16,
     main = "A Simple Example",
     ylab = "Pr(At least 2 Students Have the Same Birthday)",
     xlab = "Number of Schack Students")
abline(h = 0.5, lty = 4, col = "red")      # Add horizontal line
abline(v = 23, lty = 4, col = "red")       # Add vertical line


# Conditional probability.
library(MASS)  # used for fractions function
Omega <- expand.grid(roll1 = 1:6, roll2 = 1:6)
F <- subset(Omega, roll1 + roll2 == 10)
F
E <- subset(Omega, roll1 == 5)
E
PE <- dim(E)[1]/dim(Omega)[1]  # Pr(G)
fractions(PE)

FaE <- subset(Omega, roll1 == 5 & roll2 ==5)  # Events E and F
FaE
PFaE <- dim(FaE)[1]/dim(Omega)[1]  # P(H and G)
fractions(PFaE)
PFgE <- PFaE/PE  # P(H|G)
fractions(PFgE)


# Monte Hall Problem
set.seed(1984)  # Done for reproducibility
size <- 10000000
actual <- sample(x = 1:3, size = size, replace = TRUE)
aguess <- sample(x = 1:3, size = size, replace = TRUE)
equals <- (actual == aguess)
PNoSwitch <- sum(equals)/size
not.eq <- (actual != aguess)
PSwitch <- sum(not.eq)/size
Probs <- c(PNoSwitch, PSwitch)
names(Probs) <- c("P(Win Given No Door Change)", "P(Win Given Door Change)")
Probs


set.seed(1929)
opar <- par(no.readonly = TRUE)
library(MASS)  # used for fractions function
par(mfrow=c(1, 2), pty = "s")
Omega <- expand.grid(coin1 = 0:1, coin2 = 0:1, coin3 = 0:1)
n.heads <- apply(Omega, 1, sum)
cbind(Omega, n.heads)
table <- table(n.heads)/length(n.heads)
fractions(table)


plot(table, xlab = "x", ylab="P(X = x)", yaxt = "n", main = "PDF for X")
axis(2, at = c(1/8, 3/8), labels = c("1/8", "3/8"), las = 1)
plot(ecdf(n.heads), main = "CDF for X", ylab = "F(x)", xlab = "x", 
     yaxt = "n")
axis(2, at = c(1/8, 4/8, 7/8, 1), labels = c("1/8", "4/8", "7/8", "1"), 
     las = 1)
segments(1, 1/8, 1, 4/8, lty = 2)
text(2.6, 2.5/8, "P(X = 1) =  F(1) - F(0)")
par(opar)


mean(n.heads)
var(n.heads)


library(ggplot2)
x <- seq(-1, 1, length = 500)
y <- dunif(x, -1, 1)
DF <- data.frame(fx = y)
previous_theme <- theme_set(theme_bw())  # set black and white theme
ggplot(data = DF, aes(x = x, y = fx)) +
  geom_area(fill = "blue") +
  labs(x = "Supprt", y = "f(x)\n") +
  ylim(c(0, 1)) +
  theme_set(previous_theme)                # Restore original theme


Uniform <- c(40, 60, 75, 100, 120)
n <- length(Uniform)
meanUniform <- (1/n)*sum(Uniform)
varUniform <- (1/n)*sum((Uniform - meanUniform)^2)
result <- c(meanUniform, varUniform)
result


opar <- par(no.readonly = TRUE)
par(mfrow=c(1, 2), pty = "s")
x <- 0:8
px <- dbinom(x, 8, 0.3)
plot(x, px, type = "h", xlab = "Heads", ylab="Probability",
     main = "PDF of X~Bin(8, 0.3)")
xs <- rep(0:8, round(dbinom(0:8, 8, .3)*100000, 0))
plot(ecdf(xs), main = "CDF of X~Bin(8, 0.3)",
     ylab = expression(Pr(Heads<=x)), xlab = "Heads")
par(opar)


x <- 0:10
n <- 10
DF <- stack(list('0.2' = dbinom(x, n, 0.2), 
                 '0.5' = dbinom(x, n, 0.5), 
                 '0.8' = dbinom(x, n, 0.8)))
names(DF) <- c("px", "pi")
DF$x <- x
previous_theme <- theme_set(theme_bw()) # set black and white theme

ggplot(data = DF, aes(x = x, y = px)) + facet_grid(pi~.) + 
  geom_linerange(aes(x = x, ymin = 0, ymax = px), size = 1) + 
  geom_point(color = "blue", size = 3) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.title.y=element_text(vjust = 0.2)) +
  labs(y = "Probability", x = TeX("$x$"), title = TeX("$X \\sim Bin(n = 10, \\pi)$\n"))
theme_set(previous_theme) # Restore original theme


bino.gen <- function (samples = 10000, n = 20, pi = 0.5)
{
  values <- sample(c(0, 1), samples * n, replace = TRUE,
                   prob = c(1 - pi, pi))
  value.mat <- matrix(values, ncol = n)
  Successes <- apply(value.mat, 1, sum)
  a1 <- round((table(Successes)/samples), 3)
  b1 <- round(dbinom(0:n, n, pi), 3)
  names(b1) <- 0:n
  hist(Successes, breaks = c((-0.5 + 0):(n + 0.5)), freq = FALSE,
       ylab = "", col = 13, ylim = c(0, max(a1, b1)),
       main = " Theoretical Values Superimposed
       Over Histogram of Simulated Values")
  x <- 0:n
  fx <- dbinom(x, n, pi)
  lines(x, fx, type = "h")
  lines(x, fx, type = "p", pch = 16)
  list(simulated.distribution = a1, theoretical.distribution = b1)
}


# Geometric PDF and CDF
opar <- par(no.readonly = TRUE)
par(mfrow=c(1, 2), pty = "s")
x <- 0:15
px <- dgeom(x, .3)
plot(x, px, type = "h", xlab = "Success", ylab="P(X = x)",
     main = "PDF of X ~ Geom(0.3)")
xs <- rep(0:15, round(dgeom(0:15, 0.3)*100000, 0))
plot(ecdf(xs), main = "CDF of X ~ Geom(0.3)",
     ylab = expression(P(X <= x)), xlab = "Success")
par(opar)


# Continuous Uniform
x <- seq(-1, 1, length = 500)
y <- dunif(x, -1, 1)
DF <- data.frame(fx = y)
previous_theme <- theme_set(theme_bw())  # set black and white theme
ggplot(data = DF, aes(x = x, y = fx)) +
  geom_area(fill = "blue") +
  labs(x = "Supprt", y = "f(x)\n") +
  ylim(c(0, 1)) +
  theme_set(previous_theme)                # Restore original theme


#Continuous Gamma
x <- seq(-1, 11, length = 500)
y1 <- dgamma(x, 2, 1)
y2 <- dgamma(x, 2, 2)
y3 <- dgamma(x, 2, 4)
y4 <- dgamma(x, 3, 1)
y5 <- dgamma(x, 3, 2)
y6 <- dgamma(x, 3, 4)
y7 <- dgamma(x, 4, 1)
y8 <- dgamma(x, 4, 2)
y9 <- dgamma(x, 4, 4)
DF <- data.frame(fx = c(y1, y2, y3, y4, y5, y6, y7, y8, y9))
DF$a[1:1500] = "alpha = 2"
DF$a[1501:3000] = "alpha = 3"
DF$a[3001:4500] = "alpha = 4"
DF$l[1:500] = "lambda = 1"
DF$l[501:1000] = "lambda = 2"
DF$l[1001:1500] = "lambda = 4"
DF$l[1501:2000] = "lambda = 1"
DF$l[2001:2500] = "lambda = 2"
DF$l[2501:3000] = "lambda = 4"
DF$l[3001:3500] = "lambda = 1"
DF$l[3501:4000] = "lambda = 2"
DF$l[4001:4500] = "lambda = 4"
DF$x = x


previous_theme <- theme_set(theme_bw()) # set black and white theme
ggplot(data = DF, aes(x = x, y = fx)) + geom_area(fill = "blue") + facet_grid(l~a) + 
  labs(x = TeX("$x$"), y = TeX("$f(x)$"), title = TeX("$X ~ \\Gamma(\\alpha, \\lambda)$")) + 
  theme(axis.title.y=element_text(vjust = 0.2)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
theme_set(previous_theme) # Restore original theme  


# The Nomral Distribution: Nature's Distribution
par(mfrow = c(1, 3))
# First
curve(dnorm(x, 0, 1/2), -3.3/2, 3.3/2, axes = FALSE, ann = FALSE, 
      xlim = c(-6.6, 6.6), ylim =c(0, .8))
segments(-3.3/2,0,3.3/2,0,lwd = 2)
segments(0, 0, 0, dnorm(0, 0, 1/2), lty="dotted")
mtext(side = 1, line = 0, "mu")
# Second
curve(dnorm(x, 0, 1), -3.3, 3.3, axes = FALSE, ann = FALSE, 
      xlim = c(-6.6, 6.6), ylim =c(0, .8))
segments(-3.3,0,3.3,0,lwd = 2)
segments(0, 0, 0, dnorm(0, 0, 1), lty="dotted")
mtext(side = 1, line = 0, "mu")
# Third
curve(dnorm(x, 0, 2), -3.3*2, 3.3*2, axes = FALSE, ann = FALSE, 
      xlim = c(-6.6, 6.6), ylim =c(0, .8))
segments(-3.3*2,0,3.3*2,0,lwd = 2)
segments(0, 0, 0, dnorm(0, 0, 2), lty="dotted")
segments(2, 0, 2, dnorm(2, 0, 2), lty="dotted")
mtext(side = 1, line = 0, "mu")
arrows(0,.05,2,.05,length=.05, code = 3)
text(1, .09, "sigma")
#
par(mfrow=c(1,1))


# Suppose we wanted to calcuate the probability that a value falls between 
# A and B for a Normal(0, 1).  Let A = -1.96 and B be 1.96.
curve(dnorm(x, 0, 1), xlim = c(-3, 3), ylab = "Normal(0,1)", xlab = "Range", main="We Will See This Alot")
abline(v=-1.96, col='red', lw=2)
abline(v=1.96, col='red', lw=2)
pnorm(1.96, 0, 1) - pnorm(-1.96, 0, 1)


# Exploratory Data Analysis Using Real-World Data
data("SCORE")
attach(SCORE)
hist(scores, freq = F)

eda(scores) # Incredibly powerful command in R for exploratory data analysis
detach(SCORE)

data("mtcars")
attach(mtcars)
eda(mpg)
eda(hp)


# Examine skewness in data.
set.seed(1776)
DF <- stack(list("Normal" = rnorm(500, 0, 1), "Short Tailed" = runif(500, -1, 1), "Long Tailed" = rt(500, 1)))
names(DF) <- c("values", "Shape")
previous_theme <- theme_set(theme_bw()) # set black and white theme
ggplot(data = DF, aes(sample = values, shape = Shape, color = Shape)) + 
  stat_qq(size = 1) + geom_abline(slope = 1, lty = "dashed") + xlim(-3, 3) + 
  ylim(-3, 3) + scale_color_grey() + 
  guides(colour = guide_legend(override.aes = list(size=4)))
set.seed(8)
DF <- stack(list("Normal" = rnorm(500, 0, 1), "Skew Right" = rexp(500, 1), "Skew Left" = (-1)*rexp(500, 1)))
names(DF) <- c("values", "Shape")
ggplot(data = DF, aes(sample = values, shape = Shape, color = Shape)) + 
  stat_qq(size = 1) + geom_abline(slope = 1, lty = "dashed") + xlim(-3, 3) + 
  ylim(-3, 3) + scale_color_grey() + 
  guides(colour = guide_legend(override.aes = list(size=4)))
theme_set(previous_theme) # Restore original theme 


# Conceptualization of Arbitrary Multivariate RVs.
x <- seq(0, 1, length.out = 25)
y <- x
f2 <- function(x, y){ifelse(x >= y, x*y, 0)}
persp(x, y, outer(x, y, f2), shade = 0.6, expand = 0.6,
      theta = 300, phi = 30, ticktype = "detailed", zlab = "z")


# Covariace and Correlation
opar <- par(no.readonly = TRUE) # copy of current settings
par(mfrow= c(1, 3), mar = c(4.1,4.1,5.1,1.1))
x1 <- c(58, 72, 72, 86, 86, 100, 100, 114, 114, 128)
y1 <- c(80, 80, 90, 90, 100, 100, 110, 110, 120, 120)
plot(x1, y1, las = 0, xlab = TeX("$X_1$"), ylab = TeX("$Y_1$"), main = TeX("$Covariance>0$"), pch = 19)
abline(h = mean(y1), lty = "dashed")
abline(v = mean(x1), lty = "dashed")
x2 <- c(58, 72, 72, 86, 86, 100, 100, 114, 114, 128)
y2 <- c(1200, 1200, 1100, 1100, 1000, 1000, 900, 900, 800, 800)
plot(x2, y2, las = 0, xlab = TeX("$X_1$"), ylab = TeX("$Y_1$"), main = TeX("$Covariance<0$"), pch = 19)
abline(h = mean(y2), lty = "dashed")
abline(v = mean(x2), lty = "dashed")
x3 <- c(25.5, 27.0, 30, 33, 34.5, 33, 30, 27, 28.8, 31.2)
y3 <- c(30, 33, 34.5, 33, 30, 27, 25.5, 27, 30, 30)
plot(x3, y3, las = 0, xlab = TeX("$X_1$"), ylab = TeX("$Y_1$"), main = TeX("$Covariance=0$"), pch = 19)
abline(h = mean(y3), lty = "dashed")
abline(v = mean(x3), lty = "dashed")
par(opar)  # reset settings


# Obviously there are an in-built formulas.
cov(x1, y1)
cov(x2, y2)
cov(x3, y3)

cor(x1, y1)
cor(x2, y2)
cor(x3, y3)

cov(x1, x1) - var(x1)


# Bivariate Normal
f1 <- function(x, y, p = 0){
  exp( (x^2 - 2*p*x*y + y^2) / (-2*(1 - p^2)) ) / (2*pi*sqrt(1 - p^2)) }
f2 <- function(x, y, p = 0.4){
  exp( (x^2 - 2*p*x*y + y^2) / (-2*(1 - p^2)) ) / (2*pi*sqrt(1 - p^2)) }
f3 <- function(x, y, p = 0.8){
  exp( (x^2 - 2*p*x*y + y^2) / (-2*(1 - p^2)) ) / (2*pi*sqrt(1 - p^2)) }
opar <- par(no.readonly = TRUE) # copy of current settings
par(mfrow = c(1, 3), mar = c(1.1, 1.1, 1.1, 1.1), pty = "s")
x <- seq(-3, 3, length = 40)
y <- x
persp(x, y, outer(x, y, f1), zlab = "z", main = expression(rho == 0),
      theta = -25, expand = 0.65, phi = 25, shade = 0.2)
persp(x, y, outer(x, y, f2), zlab = "z", main = expression(rho == 0.4),
      theta = -25, expand = 0.65, phi = 25, shade = 0.2)
persp(x, y, outer(x, y, f3), zlab = "z", main = expression(rho == 0.8),
      theta = -25, expand = 0.65, phi = 25, shade = 0.3)
par(opar)


f1 <- function(x, y, p = 0){
  exp( (x^2 - 2*p*x*y + y^2) / (-2*(1 - p^2)) ) / (2*pi*sqrt(1 - p^2)) }
f2 <- function(x, y, p = -0.4){
  exp( (x^2 - 2*p*x*y + y^2) / (-2*(1 - p^2)) ) / (2*pi*sqrt(1 - p^2)) }
f3 <- function(x, y, p = -0.8){
  exp( (x^2 - 2*p*x*y + y^2) / (-2*(1 - p^2)) ) / (2*pi*sqrt(1 - p^2)) }
opar <- par(no.readonly = TRUE) # copy of current settings
par(mfrow = c(1, 3), mar = c(1.1, 1.1, 1.1, 1.1), pty = "s")
x <- seq(-3, 3, length = 40)
y <- x
persp(x, y, outer(x, y, f1), zlab = "z", main = expression(rho == 0),
      theta = -25, expand = 0.65, phi = 25, shade = 0.2)
persp(x, y, outer(x, y, f2), zlab = "z", main = expression(rho == -0.4),
      theta = -25, expand = 0.65, phi = 25, shade = 0.2)
persp(x, y, outer(x, y, f3), zlab = "z", main = expression(rho == -0.8),
      theta = -25, expand = 0.65, phi = 25, shade = 0.2)
par(opar)


f1 <- function(x, y, p = 0){
  exp( (x^2 - 2*p*x*y + y^2) / (-2*(1 - p^2)) ) / (2*pi*sqrt(1 - p^2)) }
f2 <- function(x, y, p = 0.4){
  exp( (x^2 - 2*p*x*y + y^2) / (-2*(1 - p^2)) ) / (2*pi*sqrt(1 - p^2)) }
f3 <- function(x, y, p = 0.8){
  exp( (x^2 - 2*p*x*y + y^2) / (-2*(1 - p^2)) ) / (2*pi*sqrt(1 - p^2)) }
opar <- par(no.readonly = TRUE) # copy of current settings
par(mfrow = c(1, 3), mar = c(4.1, 4.1, 4.1, 1.1), pty = "s")
x <- seq(-3, 3, length = 50)
y <- x
contour(x, y, outer(x, y, f1), nlevels = 10, xlab = "x", ylab = "y",
        main = expression(rho == 0))
contour(x, y, outer(x, y, f2), nlevels = 10, xlab = "x", ylab = "y",
        main = expression(rho == 0.4))
contour(x, y, outer(x, y, f3), nlevels = 10, xlab = "x", ylab = "y",
        main = expression(rho == 0.8))
par(opar)


opar <- par(no.readonly = TRUE) # copy of current settings
par(mfrow = c(1, 3), mar = c(4.1, 4.1, 4.1, 1.1), pty = "s")
x <- seq(-3, 3, length = 50)
y <- x
image(x, y, outer(x, y, f1), col  = gray((0:100)/100), xlab = "x",
      ylab = "y", main = expression(rho == 0))
image(x, y, outer(x, y, f2), col  = gray((0:100)/100), xlab = "x",
      ylab = "y", main = expression(rho == 0.4))
image(x, y, outer(x, y, f3), col  = gray((0:100)/100), xlab = "x",
      ylab = "y", main = expression(rho == 0.8))


x <- seq(-3, 3, length = 50)
y <- x
z1 <- outer(x, y, f1)
z2 <- outer(x, y, f2)
z3 <- outer(x, y, f3)
Grid <- expand.grid(x = x, y = y)
zp <- c(expression(rho == 0.0), expression(rho == 0.4),
        expression(rho == 0.8))
DF1 <- data.frame(x = Grid$x, y = Grid$y, z = as.vector(z1))
DF2 <- data.frame(x = Grid$x, y = Grid$y, z = as.vector(z2))
DF3 <- data.frame(x = Grid$x, y = Grid$y, z = as.vector(z3))
DF1$r = "rho == 0.0"
DF2$r = "rho == 0.4"
DF3$r = "rho == 0.8"
BDF <- rbind(DF1, DF2, DF3)
p <- ggplot(data = BDF, aes(x = x, y = y, z = z))
p + stat_contour(aes(colour = ..level..)) + theme_bw() +
  scale_colour_gradient(low = "gray10", high = "gray90") +
  labs(colour = "Density", x = "x", y = "y") +
  facet_grid(. ~ r, labeller = label_parsed) +
  coord_fixed(ratio = 1)

p <- ggplot(data = BDF, aes(x = x, y = y, fill = z))
p + geom_raster() + theme_bw() +
  scale_fill_gradient(low = "gray10", high = "gray90") +
  labs(fill = "Density", x = "x", y = "y") + facet_grid(. ~ r) +
  coord_fixed(ratio = 1)

contourplot(z1 + z2 + z3 ~ x * y, data = Grid, xlab = "x", ylab = "y",
            outer = TRUE, layout = c(3, 1), aspect = "xy",
            cuts = 11, strip = strip.custom(factor.levels = zp))


levelplot(z1 + z2 + z3 ~ x * y, data = Grid, xlab = TeX("$x$"), ylab = TeX("$y$"), 
          outer = TRUE, layout = c(3, 1), aspect = "xy",
          strip = strip.custom(factor.levels = zp))


# Sampling: What We Really Observe.
set.seed(1066)
mu <- matrix(c(0,0), nrow = 2, ncol = 1, byrow = TRUE)
rho <- 0.0
Sigma <- matrix(c(1, rho, rho, 1), nrow=2, ncol=2, byrow = TRUE) 
data = data.frame(mvrnorm(n = 1000, mu, Sigma))
plot(data$X2, data$X1, xlab=TeX("$X_1$"), ylab=TeX("$X_2$"), 
     main="Scatterplot of Simulated Data", pch=16, col='blue',
     xlim=c(-4,4), ylim=c(-4,4))
grid(lw = 2)

# What Do We Do?
abline(lm(X2 ~ X1, data = data), col='red')


set.seed(1066)
mu <- matrix(c(0,0), nrow = 2, ncol = 1, byrow = TRUE)
rho <- 0.5
Sigma <- matrix(c(1, rho, rho, 1), nrow=2, ncol=2, byrow = TRUE) 
data = data.frame(mvrnorm(n = 1000, mu, Sigma))
plot(data$X2, data$X1, xlab=TeX("$X_1$"), ylab=TeX("$X_2$"), 
     main="Scatterplot of Simulated Data", pch=16, col='blue',
     xlim=c(-4,4), ylim=c(-4,4))
grid(lw = 2)
abline(lm(X2 ~ X1, data = data), col='red')


set.seed(1066)
rho <- -0.5
Sigma <- matrix(c(1, rho, rho, 1), nrow=2, ncol=2, byrow = TRUE) 
data = data.frame(mvrnorm(n = 1000, mu, Sigma))
plot(data$X2, data$X1, xlab=TeX("$X_1$"), ylab=TeX("$X_2$"), 
     main="Scatterplot of Simulated Data", pch=16, col='blue',
     xlim=c(-4,4), ylim=c(-4,4))
grid(lw = 2)
abline(lm(X2 ~ X1, data = data), col='red')


# Real Data: A Synthesis of the Above.
data("mtcars")
attach(mtcars)

sapply(mtcars, mean, na.rm=TRUE)
sapply(mtcars, var, na.rm=TRUE)
cor(mtcars)

plot(mpg, wt, xlab=TeX("$MPG$"), ylab=TeX("Weight$"), 
     main="Weight versus MPG", pch=16, col='blue')
grid(lw = 2)

abline(lm(wt ~ mpg, data = mtcars), col='red')


threemonth <- drop_na(fredr(series_id = "DGS3MO", observation_start = as.Date("2008-01-01")))
tenyear <- drop_na(fredr(series_id = "DGS10", observation_start = as.Date("2008-01-01")))
plot(threemonth$value, tenyear$value, 
     xlab=TeX("3 Month Yields"), ylab=TeX("10 Year Yields"), 
     main="Daily Interest Rates Since 2008", pch=16, col='blue')
grid(lw = 2)
abline(lm(tenyear$value ~ threemonth$value), col='red')
