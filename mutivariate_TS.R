# Stationarity of mutivariate time series
#covariate time series

#eg 1 notes eqns used
A<-matrix(c(0.2,0.1,0.4,0.3),2,2)
# A is a matrix of coefficients
A
#using inbuilt function in R for eigen values
eigen(A)
eigen(A)$values
#both eigen values in output has mode<1, stat TS

#eg2
A1<-matrix(c(-0.6,-0.1,0.5,0.8),2,2)
A1

eigen(A1)
# dont see vector output, only values




