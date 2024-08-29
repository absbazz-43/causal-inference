“library(markovchain)set.seed(1)
States <- c("Rainy","Cloudy","Sunny")
TransMat <- matrix(c(0.30,0.50,0.20,0.25,0.4,0.35,0.1,0.2,0.70),nrow=3,byrow = TRUE)
TransMat
library(diagram)

plot(TransMat,package="diagram")

TransMat <- matrix(c(0.30,0.50,0.20,0.25,0.4,0.35,0.1,0.2,0.70),nrow = 3, byrow = TRUE)


df = seq(-10,10, by = .1)
?tanh()
plot(tanh(df))


h = -df*log10(df)
h
plot(df,h)
abline(v =df[which.max(h)] )

df[which.max(h)]


1/(1+exp(-.7))
