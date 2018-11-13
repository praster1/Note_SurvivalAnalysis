##### 호스 분포
### f(y) = (1+s y)^(-f/s) exp(-(y/m)^2/2) (y/m^2+f/(1+s y))
### y = x or t, s = beta, f = theta, m = delta
require(rmutil)

par(mfrow = c(3, 4))

### parameter: delta
delta = c(0.5, 1, 2, 4, 8)	# delta

### Input Variable
x <- seq(0.1, 10, length.out = 101)

color = rainbow(10)

### Life Distribution
plot(x, dhjorth(x, m=delta[1], s=1, f=1), xlim=c(0, 10), ylim=c(0, 1), col=color[1], lwd=2, type = 'l', main="Life Distribution")
for (i in 2:5)	{	lines(x, dhjorth(x, m=delta[i], s=1, f=1), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('delta = 0.5', 'delta = 1', 'delta = 2', 'delta = 4', 'delta = 8'))

### Cumulative Distribution
plot(x, phjorth(x, m=delta[1], s=1, f=1), xlim=c(0, 10), ylim=c(0, 1), col=color[1], lwd=2, type = 'l', main="Cumulative Distribution")
for (i in 2:5)	{	lines(x, phjorth(x, m=delta[i], s=1, f=1), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('delta = 0.5', 'delta = 1', 'delta = 2', 'delta = 4', 'delta = 8'))

### Survival Function
plot(x, 1-phjorth(x, m=delta[1], s=1, f=1), xlim=c(0, 10), ylim=c(0, 1), col=color[1], lwd=2, type = 'l', main="Survival Function")
for (i in 2:5)	{	lines(x, 1-phjorth(x, m=delta[i], s=1, f=1), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('delta = 0.5', 'delta = 1', 'delta = 2', 'delta = 4', 'delta = 8'))

### Hazard Function
plot(x, dhjorth(x, m=delta[1], s=1, f=1)/(1-phjorth(x, m=delta[1], s=1, f=1)), xlim=c(0, 10), ylim=c(0, 10), col=color[1], lwd=2, type = 'l', main="Hazard Function")
for (i in 2:5) {	lines(x, dhjorth(x, m=delta[i], s=1, f=1)/(1-phjorth(x, m=delta[i], s=1, f=1)), col=color[i], lwd=2);	}
legend('topright', bty = 'n', lwd=2, col=color[1:5], legend = c('delta = 0.5', 'delta = 1', 'delta = 2', 'delta = 4', 'delta = 8'))



### parameter: beta
beta = c(0.5, 1, 2, 4, 8)	# beta

### Input Variable
x <- seq(0.1, 10, length.out = 101)

color = rainbow(10)

### Life Distribution
plot(x, dhjorth(x, m=1, s=beta[1], f=1), xlim=c(0, 10), ylim=c(0, 1), col=color[1], lwd=2, type = 'l', main="Life Distribution")
for (i in 2:5)	{	lines(x, dhjorth(x, m=1, s=beta[i], f=1), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('beta = 0.5', 'beta = 1', 'beta = 2', 'beta = 4', 'beta = 8'))

### Cumulative Distribution
plot(x, phjorth(x, m=1, s=beta[1], f=1), xlim=c(0, 10), ylim=c(0, 1), col=color[1], lwd=2, type = 'l', main="Cumulative Distribution")
for (i in 2:5)	{	lines(x, phjorth(x, m=1, s=beta[i], f=1), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('beta = 0.5', 'beta = 1', 'beta = 2', 'beta = 4', 'beta = 8'))

### Survival Function
plot(x, 1-phjorth(x, m=1, s=beta[1], f=1), xlim=c(0, 10), ylim=c(0, 1), col=color[1], lwd=2, type = 'l', main="Survival Function")
for (i in 2:5)	{	lines(x, 1-phjorth(x, m=1, s=beta[i], f=1), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('beta = 0.5', 'beta = 1', 'beta = 2', 'beta = 4', 'beta = 8'))

### Hazard Function
plot(x, dhjorth(x, m=1, s=beta[1], f=1)/(1-phjorth(x, m=1, s=beta[1], f=1)), xlim=c(0, 10), ylim=c(0, 10), col=color[1], lwd=2, type = 'l', main="Hazard Function")
for (i in 2:5) {	lines(x, dhjorth(x, m=1, s=beta[i], f=1)/(1-phjorth(x, m=1, s=beta[i], f=1)), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('beta = 0.5', 'beta = 1', 'beta = 2', 'beta = 4', 'beta = 8'))



### parameter: theta
theta = c(0.5, 1, 2, 4, 8)	#theta

### Input Variable
x <- seq(0.1, 10, length.out = 101)

color = rainbow(10)

### Life Distribution
plot(x, dhjorth(x, m=1, s=1, f=theta[1]), xlim=c(0, 10), ylim=c(0, 1), col=color[1], lwd=2, type = 'l', main="Life Distribution")
for (i in 2:5)	{	lines(x, dhjorth(x, m=1, s=1, f=theta[i]), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('theta = 0.5', 'theta = 1', 'theta = 2', 'theta = 4', 'theta = 8'))

### Cumulative Distribution
plot(x, phjorth(x, m=1, s=1, f=theta[1]), xlim=c(0, 10), ylim=c(0, 1), col=color[1], lwd=2, type = 'l', main="Cumulative Distribution")
for (i in 2:5)	{	lines(x, phjorth(x, m=1, s=1, f=theta[i]), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('theta = 0.5', 'theta = 1', 'theta = 2', 'theta = 4', 'theta = 8'))

### Survival Function
plot(x, 1-phjorth(x, m=1, s=1, f=theta[1]), xlim=c(0, 10), ylim=c(0, 1), col=color[1], lwd=2, type = 'l', main="Survival Function")
for (i in 2:5)	{	lines(x, 1-phjorth(x, m=1, s=1, f=theta[i]), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('theta = 0.5', 'theta = 1', 'theta = 2', 'theta = 4', 'theta = 8'))

### Hazard Function
plot(x, dhjorth(x, m=1, s=1, f=theta[1])/(1-phjorth(x, m=1, s=1, f=theta[1])), xlim=c(0, 10), ylim=c(0, 10), col=color[1], lwd=2, type = 'l', main="Hazard Function")
for (i in 2:5) {	lines(x, dhjorth(x, m=1, s=1, f=theta[i])/(1-phjorth(x, m=1, s=1, f=theta[i])), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('theta = 0.5', 'theta = 1', 'theta = 2', 'theta = 4', 'theta = 8'))