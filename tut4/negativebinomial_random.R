set.seed(1000)
nbinr <- function(n=1, p=.5, N=1000){
    ur <- runif(N)
    nbn <- rep(n+100,N)
    x <- rep(0,n+100)
    for (i in 0:(n+100)){
        x[i+1]<-pnbinom(q=i,size = n, prob = p,lower.tail = T)
    }
    
    for (i in 1:N){
        if (ur[i]<=x[1]){
            nbn[i]=0
            next
        }
        for (j in 1:(n+100)){
            if (ur[i] > x[j] & ur[i]<=x[j+1]){
                nbn[i]=j
                break 
            }
        }
    }
    max<-max(nbn)
    min<-min(nbn)
    mean <- mean(nbn)
    var <- var(nbn)
    bins = c(-0.5:(max(nbn)+0.5))
    plot = hist(nbn, breaks = bins, freq = F)
    y <- 0:max(nbn)
    lines(x = y, y = dnbinom(y,n,p), type = "h", col = "red", lwd = 3)
    return(list(max = max, min = min, nbn = nbn, hist = plot, mean = mean, var = var)) 
}

w <- nbinr(5,1/3,10000)
w$mean
w$var
