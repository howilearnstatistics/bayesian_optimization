#This R file contains a machine learning model.
#
#For purpose of computationally convenience, a simple function is provided to 
#simulate behavior of a machine learning model.
#
#To test Bayes Optimmization on a real machine learning model, define a function which takes hyper-parameters as input 
#and output a metric that you need to minimize


f = function(df){
  x = df[ ,1]
  y = df[ ,2]
  z = df[ ,3]
  return(1/(x+y) - 15/z + 0.1*y^2 + 10*x)
}