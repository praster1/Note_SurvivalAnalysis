##### Alpha Distribution
### parameter
alpha = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
beta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x <- seq(0, 10, length.out = 101)

color = rainbow(10)
par(mfrow = c(2, 2))


##### 알파 분포
dalpha = function(x, alpha = 1, beta = 0, log = FALSE) 
{
    fx <- (beta * exp(-(1/2) *(alpha - beta / x)^2)) / (sqrt(2 * pi) * pnorm(alpha) * x^2)
    
    if (log) 
        return(log(fx))
    else return(fx)
}


## 누적분포함수
palpha = function (q, alpha = 1, beta = 0, lower.tail = TRUE, log.p = FALSE) 
{
    Fx <- pnorm(alpha - beta/q) / pnorm(alpha)
    if (!lower.tail) 
        Fx <- 1 - Fx
    if (log.p) 
        Fx <- log(Fx)
    return(Fx)
}
X



### Life Distribution
plot(x, dalpha(x, alpha[1], beta[1]), xlim=c(0, 10), ylim=c(0, 3), col=color[1], lwd=2, type = 'l', main="Life Distribution")
for (i in 2:5)	{	lines(x, dalpha(x, alpha[i], beta[i]), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('lambda = 0.5', 'lambda = 1', 'lambda = 2', 'lambda = 4', 'lambda = 8'))
