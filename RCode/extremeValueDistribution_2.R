##### 극치 분포: Gumbel 최대값 분포
dgumbel_max = function (x, scale = 1, location = 0, log = FALSE) 
{
    fx <- 1/scale * exp(-(x - location)/scale - exp(-(x - location)/scale))
    if (log) 
        return(log(fx))
    else return(fx)
}

pgumbel_max = function (q, scale = 1, location = 0, lower.tail = TRUE, log.p = FALSE) 
{
    Fx <- exp(-exp(-(q - location)/scale))
    if (!lower.tail) 
        Fx <- 1 - Fx
    if (log.p) 
        Fx <- log(Fx)
    return(Fx)
}
qgumbel_max = function (p, scale = 1, location = 0, lower.tail = TRUE, log.p = FALSE) 
{
    if (log.p) 
        p <- exp(p)
    if (!lower.tail) 
        p <- 1 - p
    xF <- location - scale * log(-log(p))
    return(xF)
}

rgumbel_max = function (n, scale = 1, location = 0) 
{
	qgumbel(runif(n), scale, location)
}


par(mfrow = c(2, 4))

### parameter: mu
mu = c(0.5, 1, 2, 4, 8)	# shape

### Input Variable
x <- seq(0, 10, length.out = 101)

color = rainbow(10)

### Life Distribution
plot(x, dgumbel_max(x, location=mu[1], scale=1), xlim=c(0, 10), ylim=c(0, 0.5), col=color[1], lwd=2, type = 'l', main="Life Distribution")
for (i in 2:5)	{	lines(x, dgumbel_max(x, location=mu[i], scale=1), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('mu = 0.5', 'mu = 1', 'mu = 2', 'mu = 4', 'mu = 8'))

### Cumulative Distribution
plot(x, pgumbel_max(x, location=mu[1], scale=1), xlim=c(0, 10), ylim=c(0, 1), col=color[1], lwd=2, type = 'l', main="Cumulative Distribution")
for (i in 2:5)	{	lines(x, pgumbel_max(x, location=mu[i], scale=1), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('mu = 0.5', 'mu = 1', 'mu = 2', 'mu = 4', 'mu = 8'))

### Survival Function
plot(x, 1-pgumbel_max(x, location=mu[1], scale=1), xlim=c(0, 10), ylim=c(0, 1), col=color[1], lwd=2, type = 'l', main="Survival Function")
for (i in 2:5)	{	lines(x, 1-pgumbel_max(x, location=mu[i], scale=1), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('mu = 0.5', 'mu = 1', 'mu = 2', 'mu = 4', 'mu = 8'))

### Hazard Function
plot(x, dgumbel_max(x, location=mu[1], scale=1)/(1-pgumbel_max(x, location=mu[1], scale=1)), xlim=c(0, 10), ylim=c(0, 1), col=color[1], lwd=2, type = 'l', main="Hazard Function")
for (i in 2:5) {	lines(x, dgumbel_max(x, location=mu[i], scale=1)/(1-pgumbel_max(x, location=mu[i], scale=1)), col=color[i], lwd=2);	}
legend('topright', bty = 'n', lwd=2, col=color[1:5], legend = c('mu = 0.5', 'mu = 1', 'mu = 2', 'mu = 4', 'mu = 8'))



### parameter: beta_p
beta_p = c(0.5, 1, 2, 4, 8)	#scale

### Input Variable
x <- seq(-10, 10, length.out = 101)

color = rainbow(10)

### Life Distribution
plot(x, dgumbel_max(x, location=0, scale=beta_p[1]), xlim=c(-5, 10), ylim=c(0, 1), col=color[1], lwd=2, type = 'l', main="Life Distribution")
for (i in 2:5)	{	lines(x, dgumbel_max(x, location=0, scale=beta_p[i]), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('beta_p = 0.5', 'beta_p = 1', 'beta_p = 2', 'beta_p = 4', 'beta_p = 8'))

### Cumulative Distribution
plot(x, pgumbel_max(x, location=0, scale=beta_p[1]), xlim=c(-5, 10), ylim=c(0, 1), col=color[1], lwd=2, type = 'l', main="Cumulative Distribution")
for (i in 2:5)	{	lines(x, pgumbel_max(x, location=0, scale=beta_p[i]), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('beta_p = 0.5', 'beta_p = 1', 'beta_p = 2', 'beta_p = 4', 'beta_p = 8'))

### Survival Function
plot(x, 1-pgumbel_max(x, location=0, scale=beta_p[1]), xlim=c(-5, 10), ylim=c(0, 1), col=color[1], lwd=2, type = 'l', main="Survival Function")
for (i in 2:5)	{	lines(x, 1-pgumbel_max(x, location=0, scale=beta_p[i]), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('beta_p = 0.5', 'beta_p = 1', 'beta_p = 2', 'beta_p = 4', 'beta_p = 8'))

### Hazard Function
plot(x, dgumbel_max(x, location=0, scale=beta_p[1])/(1-pgumbel_max(x, location=0, scale=beta_p[1])), xlim=c(-5, 10), ylim=c(0, 2), col=color[1], lwd=2, type = 'l', main="Hazard Function")
for (i in 2:5) {	lines(x, dgumbel_max(x, location=0, scale=beta_p[i])/(1-pgumbel_max(x, location=0, scale=beta_p[i])), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('beta_p = 0.5', 'beta_p = 1', 'beta_p = 2', 'beta_p = 4', 'beta_p = 8'))
