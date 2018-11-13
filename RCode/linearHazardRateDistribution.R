##### 선형 증가 분포
dlinearFR = function (x, alpha = 0, beta = 1, log = FALSE) 
{
    fx <- (alpha * x + beta) * exp(- (1/2) * alpha * x^2 + beta * x)
    if (log) 
        return(log(fx))
    else return(fx)
}

plinearFR = function (q, alpha = 0, beta = 1, lower.tail = TRUE, log.p = FALSE) 
{
    Fx <- 1 - exp(- (1/2) * alpha * x^2 + beta * x)
    if (!lower.tail) 
        Fx <- 1 - Fx
    if (log.p) 
        Fx <- log(Fx)
    return(Fx)
}

# 점검 필요
# qlinearFR = function (p, scale = 1, location = 0, lower.tail = TRUE, log.p = FALSE) 
# {
    # if (log.p) 
        # p <- exp(p)
    # if (!lower.tail) 
        # p <- 1 - p
    # xF <- location - scale * log(-log(p))
    # return(xF)
# }

# rlinearFR = function (n, alpha = 0, beta = 1) 
# {
	# qlinearFR(runif(n), scale, location)
# }


par(mfrow = c(2, 4))

### parameter: alpha
alpha = c(0.5, 1, 2, 4, 8)	# shape

### Input Variable
x <- seq(0, 10, length.out = 101)

color = rainbow(10)

### Life Distribution
plot(x, dlinearFR(x, alpha=alpha[1], beta=1), xlim=c(0, 10), ylim=c(0, 6), col=color[1], lwd=2, type = 'l', main="Life Distribution")
for (i in 2:5)	{	lines(x, dlinearFR(x, alpha=alpha[i], beta=1), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('alpha = 0.5', 'alpha = 1', 'alpha = 2', 'alpha = 4', 'alpha = 8'))

### Cumulative Distribution
plot(x, plinearFR(x, alpha=alpha[1], beta=1), xlim=c(0, 10), ylim=c(0, 1), col=color[1], lwd=2, type = 'l', main="Cumulative Distribution")
for (i in 2:5)	{	lines(x, plinearFR(x, alpha=alpha[i], beta=1), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('alpha = 0.5', 'alpha = 1', 'alpha = 2', 'alpha = 4', 'alpha = 8'))

### Survival Function
plot(x, 1-plinearFR(x, alpha=alpha[1], beta=1), xlim=c(0, 10), ylim=c(0, 3), col=color[1], lwd=2, type = 'l', main="Survival Function")
for (i in 2:5)	{	lines(x, 1-plinearFR(x, alpha=alpha[i], beta=1), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('alpha = 0.5', 'alpha = 1', 'alpha = 2', 'alpha = 4', 'alpha = 8'))

### Hazard Function
plot(x, dlinearFR(x, alpha=alpha[1], beta=1)/(1-plinearFR(x, alpha=alpha[1], beta=1)), xlim=c(0, 10), ylim=c(0, 10), col=color[1], lwd=2, type = 'l', main="Hazard Function")
for (i in 2:5) {	lines(x, dlinearFR(x, alpha=alpha[i], beta=1)/(1-plinearFR(x, alpha=alpha[i], beta=1)), col=color[i], lwd=2);	}
legend('topright', bty = 'n', lwd=2, col=color[1:5], legend = c('alpha = 0.5', 'alpha = 1', 'alpha = 2', 'alpha = 4', 'alpha = 8'))



### parameter: beta_p
beta_p = c(0.5, 1, 2, 4, 8)	#beta

### Input Variable
x <- seq(-10, 10, length.out = 101)

color = rainbow(10)

### Life Distribution
plot(x, dlinearFR(x, alpha=0, beta=beta_p[1]), xlim=c(-5, 10), ylim=c(0, 6), col=color[1], lwd=2, type = 'l', main="Life Distribution")
for (i in 2:5)	{	lines(x, dlinearFR(x, alpha=0, beta=beta_p[i]), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('beta_p = 0.5', 'beta_p = 1', 'beta_p = 2', 'beta_p = 4', 'beta_p = 8'))

### Cumulative Distribution
plot(x, plinearFR(x, alpha=0, beta=beta_p[1]), xlim=c(-5, 10), ylim=c(0, 1), col=color[1], lwd=2, type = 'l', main="Cumulative Distribution")
for (i in 2:5)	{	lines(x, plinearFR(x, alpha=0, beta=beta_p[i]), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('beta_p = 0.5', 'beta_p = 1', 'beta_p = 2', 'beta_p = 4', 'beta_p = 8'))

### Survival Function
plot(x, 1-plinearFR(x, alpha=0, beta=beta_p[1]), xlim=c(-5, 10), ylim=c(0, 3), col=color[1], lwd=2, type = 'l', main="Survival Function")
for (i in 2:5)	{	lines(x, 1-plinearFR(x, alpha=0, beta=beta_p[i]), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('beta_p = 0.5', 'beta_p = 1', 'beta_p = 2', 'beta_p = 4', 'beta_p = 8'))

### Hazard Function
plot(x, dlinearFR(x, alpha=0, beta=beta_p[1])/(1-plinearFR(x, alpha=0, beta=beta_p[1])), xlim=c(-5, 10), ylim=c(0, 10), col=color[1], lwd=2, type = 'l', main="Hazard Function")
for (i in 2:5) {	lines(x, dlinearFR(x, alpha=0, beta=beta_p[i])/(1-plinearFR(x, alpha=0, beta=beta_p[i])), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('beta_p = 0.5', 'beta_p = 1', 'beta_p = 2', 'beta_p = 4', 'beta_p = 8'))
