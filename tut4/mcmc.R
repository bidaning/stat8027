set.seed(1000)
mcmct <- function(df, N, delta, burn){
    out <- rep(0,N+burn)
    acc <- 0
    
    ##density
    #pt
    
    ##starting value
    y <- 0
    out[1] <- y
    
    ##tuning parament
    delta <- 1
    
    ##mcmc
    for (i in 2:N+burn){
        y.star <- rnorm(1, mean = y, sd = 1*delta)
        r <- dt(y.star, df)/dt(y, df)
        rho <- min(r,1)
        
        if(runif(1) <= rho){
            y <- y.star
            acc <- acc + 1
        }
        out[i] <- y
    }
    par(mfrow=c(1,2))
    bins <- (floor(min(out[(burn+1):(N+burn)]))-0.5):(ceiling(max(out[(burn+1):(N+burn)])+0.5))
    plot1 <- plot(out[(burn+1):(N+burn)], type = "l", ylab = "y",xlab = "scan")
    plot2 <- hist(out[(burn+1):(N+burn)], breaks = bins, prob = T, ylim =c(0,0.4))
    y <- seq(floor(min(out[(burn+1):(N+burn)])), ceiling(max(out[(burn+1):(N+burn)])), by = 0.01)
    lines(y, dt(y, df), type = "l", col = "blue", lwd = 3)
    #lines(y, dnorm(y,0,1), type = "l", col = "red", lwd = 3)
    return(list(out = out[(burn+1):(N+burn)], plot = plot1, hist = plot2, acc = acc))
}

w <- mcmct(4, 1000, 1, 100)
#y <- seq(-5, 5, by = 0.01)
#lines(y, dt(y, 4000), type = "l", col = "red", lwd = 3)
#hist()
#plot(y, dt(y, 4), type = "l", lwd = 3)
