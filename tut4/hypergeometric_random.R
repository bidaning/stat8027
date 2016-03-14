set.seed(1000)
hyperr <- function(n=1, m=1, k=1, N=1000){
    ur <- runif(N)
    hyp <- rep(k+1,N)
    x <- rep(0,k+1)
    for (i in 0:k){
        #x[i+1]<-phyper(i,m,n-m,k,lower.tail = T)
        if (i == 0) {
            x[i+1] <- choose(m,i)*choose(n-m,k-i)/choose(n,k)
        } else {
            x[i+1] <- x[i] + choose(m,i)*choose(n-m,k-i)/choose(n,k)
        }
    }
    
    for (i in 1:N){
        if (ur[i]<=x[1]){
            hyp[i]=0
            next
        }
        for (j in 1:k){
            if (ur[i] > x[j] & ur[i]<=x[j+1]){
                hyp[i]=j
                break 
            }
        }
    }
    max<-max(hyp)
    min<-min(hyp)
    mean <- mean(hyp)
    var <- var(hyp)
    bins = c(-0.5:(k+0.5))
    plot = hist(hyp, breaks = bins, freq = F)
    y <- 0:k
    lines(x = y, y = dhyper(y,m,n-m,k), type = "h", col = "red", lwd = 3)
    return(list(max = max, min = min, hyp = hyp, hist = plot, mean = mean, var = var))
}

w <- hyperr(10,8,4,10000)
w$mean
w$var
dhyper(0:4,8,2,4)
