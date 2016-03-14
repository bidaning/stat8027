set.seed(1000)
binr <- function(n=1, p=.5, N=1000){
    ur <- runif(N)
    bn <- rep(n+1,N)
    x <- rep(0,n+1)
    for (i in 0:n){
        #x[i+1]<-pbinom(i,n,p,lower.tail = T)
        if (i == 0){
            x[i+1] <- choose(n,i)*p^i*(1-p)^(n-i)
        } else {
            x[i+1] <- x[i] + choose(n,i)*p^i*(1-p)^(n-i)
        }
    }

    for (i in 1:N){
        if (ur[i]<=x[1]){
            bn[i]=0
            next
        }
        for (j in 1:n){
            if (ur[i] > x[j] & ur[i]<=x[j+1]){
                bn[i]=j
                break 
            }
        }
    }
    max<-max(bn)
    min<-min(bn)
    mean <- mean(bn)
    var <- var(bn)
    bins = c(-0.5:(n+0.5))
    plot = hist(bn, breaks = bins, freq = F)
    y <- 0:n
    lines(x = y, y = dbinom(y,n,p), type = "h", col = "red", lwd = 3)
    return(list(max = max, min = min, bn = bn, hist = plot, mean = mean, var = var))
}

w <- binr(8,2/3,10000)
w$mean
w$var
