#This is an implementation of the following paper:
#http://papers.nips.cc/paper/4522-practical-bayesian-optimization-of-machine-learning-algorithms.pdf

library(GPfit)
library(purrr)

#set working directory
wd = "/home/nghia/Documents/Sample Code/sample code 2"
setwd(wd) 

#source model from another R file
source('model.R')

#"hyper-parameter" of the model
x = seq(from = 0.1,
        to = 0.5,
        by = 0.1)
y = seq(from = 1,
        to = 5,
        by = 0.5)
z = seq(from = 10,
        to = 50,
        by = 5)

#define search space
s_space = expand.grid(x,y,z)

#create variable t with range (0,1) and length is the length of the search space
interval = seq(from = 0, to = 1, by = 1/(nrow(s_space)-1)) 
interval = round(interval, 5)
s_space$t = interval
colnames(s_space) = c("x", "y", "z", "t")
#taking sample from search space 
sample = sample(interval, size = 50, replace = FALSE)
#
t = c()
value = c()
count = 1
repeat{
  t[count] = sample[count]
  value[count] = f(s_space[s_space$t == sample[count], ])
  count = count + 1
  if(count>50) break
} 
#initial value for bayesian optimization
seed = as.data.frame(cbind(t,value))

count = 1
count_max = 25 #define number of time you want this process to be repeated
repeat{
  #fit a gaussian process with initial value
  fit = GP_fit(
    X = seed[, "t"],
    Y = seed[, "value"],
    corr = list(type = "matern", nu = 5/2)
  )
  
  #prediction with gaussian process
  pred <- predict.GP(fit, xnew = interval)
  mu <- pred$Y_hat
  sigma <- sqrt(pred$MSE)
  y_best = min(seed[, "value"])
  
  #acquisition function, calculate probability of improvement
  expected_improvement <- purrr::map2_dbl(mu, sigma, 
                                          function(m, s){
                                            if (s == 0) return(0)
                                            gamma <- (y_best - m) / s
                                            phi <- pnorm(gamma)
                                            return(s * (gamma * phi + dnorm(gamma)))
                                            })
  
  #choose next point to evaluate, 
  x_new = interval[which.max(expected_improvement)]
  
  #update this point to the seed 
  seed[nrow(seed)+1, 1] = x_new
  seed[nrow(seed), 2]  = f(s_space[s_space$t == x_new, ])
  #
  count = count + 1
  if (count > count_max) break
}
#
plot(s_space$t, 
     f(s_space[c(1:nrow(s_space)), ]), 
     type = "l", 
     col = "green",
     xlab = "t",
     ylab = "value")
#
lines( s_space$t,
       pred$Y_hat, 
       type = "l", 
       col = "red",)

#minimum value found
min = seed[which(seed$value == min(seed$value)), ]
#t which minimizes the function
t_min = seed[which(seed$value == min(seed$value)), ][1]
#hyper-parameters which minimize the function
s_space[which(s_space$t == as.numeric(t_min)), ]