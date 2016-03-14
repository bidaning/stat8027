data <- c(158,143,106,57,97,80,109,109,350,224,109,214,84)
mean <- mean(data)
mean
var <- var(data)
var
set.seed(1000)
svar <- rep(0,5000)
for ( i in 1:5000){
    poir <- rpois(13,lambda = mean)
    svar[i] <- var(poir) 
}

hist(svar, freq = F)
prob <- length(svar[svar>var])/5000
prob

r = mean^2/(var-mean)
r
pr = 1/(1+mean/r)
pr
#rnbinom(1000, size =r,  prob = pr)
ssvar <- rep(0,5000)
for ( i in 1:5000){
    poir <- rnbinom(13, size =r,  prob = pr)
    ssvar[i] <- var(poir) 
}
hist(ssvar, freq = F)
abline(v = var, col = "red", lwd = 3)
prob2 <- length(ssvar[ssvar>var])/5000
prob2
