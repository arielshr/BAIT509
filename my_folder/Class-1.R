###################################
#BAIT 509 Class 1 Exercise
###################################

##################
#Oracle Regression
##################

library(tidyverse)
library(ggplot2)

genreg <- function(n){
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  eps <- rnorm(n)
  y <- 5-x1+2*x2+eps
  tibble(x1 = x1, x2 = x2, y = y)
}

sample <- genreg(1000)
sample

dat <- mutate(sample,
       yhat = 5,
       yhat1 = 5 - x1,
       yhat2 = 5 + 2*x2,
       yhat12 = 5 - x1 + 2*x2
       )

mse <- mean((dat$yhat - dat$y)^2)
mse1 <- mean((dat$yhat1 - dat$y)^2)
mse2 <- mean((dat$yhat2 - dat$y)^2)
mse12 <- mean((dat$yhat12 - dat$y)^2)

######################
#Oracle Classification
######################
p.1 <- data.frame(matrix(ncol = 4, nrow = 1))
colnames(p.1) <- c("x", "pa", "pb", "pc")
p.1$x=1
p.1$pa = 0.2
p.1$pb = 0.8/(1+exp(-p.1$x))
p.1$pc = 1 - p.1$pa - p.1$pb
p.1

pa.minus2 = 0.2
pb.minus2 = 0.8/(1+exp(2))
pc.minus2 = 1 - pa - pb

gencla <- function(n) {
  x <- rnorm(n) 
  pB <- 0.8/(1+exp(-x))
  y <- map_chr(pB, function(x) 
    sample(LETTERS[1:3], size=1, replace=TRUE,
           prob=c(0.2, x, 1-0.2-x)))
  tibble(x=x, y=y)
}

dat2 <- gencla(1000)
dat2

dat2 <- mutate(dat2,
              yhat = sapply(x,function(x_)
              if (x_<0) "C" else "B"))

error <- 1 - mean(dat2$yhat == dat2$y)
error
