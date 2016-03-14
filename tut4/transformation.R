set.seed(1000)
transt <- function(df ,N){
    out <- rep(0,N)
    for (i in 1:N){
        v <- rnorm(1,0,1)
        zsqr <- rep(0,df)
        for (j in 1:df){
            z <- rnorm(1,0,1)
            zsqr[j] <- z^2
        }
        out[i] <- v/sqrt(sum(zsqr)/df)
    }
    par(mfrow=c(1,1))
    bins <- (floor(min(out))-0.5):(ceiling(max(out))+0.5)
    plot <- hist(out, breaks = bins, prob = T, ylim = c(0,0.4))
    x <- seq(floor(min(out)), ceiling(max(out)), by = 0.01)
    lines(x, dt(x, df), type = "l", col = "blue", lwd = 3)
    return(list(out = out, plot = plot))
}
transt(2,1000)
