set.seed(1000)
art <- function(df, N){
    x <- seq(0,1,by = 0.001)
    M <- max(dt(x,df = df)/dcauchy(x,0,1))
    y <- NULL
    for (i in 1:N){
        u <- runif(1,0,1)
        v <- rcauchy(1,0,1)
        if (u*M*dcauchy(v)<dt(v,df = df)){
            y.i <- v
            y <- c(y,y.i)
        }
    }
    num <- length(y)
    bins <- (floor(min(y))-0.5):(ceiling(max(y))+0.5)
    plot <- hist(y, breaks = bins, prob = T, ylim =c(0,0.4))
    z <- seq(floor(min(y)), ceiling(max(y)), by = 0.01)
    lines(z, dt(z, df), type = "l", col = "blue", lwd = 3)
    return(list(out = y, num = num, plot = plot))
}

w <- art(2,1000)
w$num

