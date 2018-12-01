delta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)	# m (delta)
beta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)		# s (beta)
theta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)	# f (theta)


dhjorth = function (y, m, s, f, log = FALSE) 
{
    if (any(y <= 0)) 
        stop("y must contain positive values")
    if (any(m <= 0)) 
        stop("m must be positive")
    if (any(s <= 0)) 
        stop("s must be positive")
        
    tmp <- -(f/s) * log(1 + s * y) - (y/m)^2/2 + log(y/m^2 + f/(1 + s * y))
    
    if (!log) 
        tmp <- exp(tmp)
    tmp
}

qhjorth = function (p, m, s, f) 
{
    h <- function(y) 
    {
        1 - (1 + s[i] * y)^(-f[i]/s[i]) * exp(-(y/m[i])^2/2) - p[i]
    }
    
    if (any(p < 0 | p > 1)) 
        stop("p must lie between 0 and 1")
    if (any(m <= 0)) 
        stop("m must be positive")
    if (any(s < 0)) 
        stop("s must be positive")
    len <- max(length(p), length(m), length(s), length(f))
    if (length(p) != len) {
        if (length(p) == 1) 
            p <- rep(p, len)
        else stop("length of p incorrect")
    }
    if (length(m) != len) {
        if (length(m) == 1) 
            m <- rep(m, len)
        else stop("length of m incorrect")
    }
    if (length(s) != len) {
        if (length(s) == 1) 
            s <- rep(s, len)
        else stop("length of s incorrect")
    }
    if (length(f) != len) {
        if (length(f) == 1) 
            f <- rep(f, len)
        else stop("length of f incorrect")
    }
    tmp <- vector(mode = "numeric", len)
    for (i in 1:len) {
        interval <- c(.Machine$double.xmin, 20)
        while (h(interval[1]) * h(interval[2]) > 0) interval <- 2 * 
            interval
        tmp[i] <- uniroot(h, interval)$root
    }
    tmp
}



phjorth = function (q, m, s, f) 
{
    if (any(q <= 0)) 
        stop("q must contain positive values")
    if (any(m <= 0)) 
        stop("m must be positive")
    if (any(s <= 0)) 
        stop("s must be positive")
        
    1 - (1 + s * q)^(-f/s) * exp(-(q/m)^2/2)
}


rhjorth = function (n = 1, m, s, f) 
{
    qhjorth(runif(n), m = m, s = s, f = f)
}
