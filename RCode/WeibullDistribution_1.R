##### Weibull Distribution with 2 parameters
par(mfrow = c(2, 4))

### parameter: alpha
alpha = c(0.5, 1, 2, 4, 8)	# shape

### Input Variable
x = seq(0, 10, length.out = 101)

color = rainbow(10)

### Life Distribution
plot(x, dweibull(x, shape=alpha[1], scale=1), xlim=c(0, 10), ylim=c(0, 3), col=color[1], lwd=2, type = 'l', main="Life Distribution")
for (i in 2:5)	{	lines(x, dweibull(x, shape=alpha[i], scale=1), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('alpha = 0.5', 'alpha = 1', 'alpha = 2', 'alpha = 4', 'alpha = 8'))

### Cumulative Distribution
plot(x, pweibull(x, shape=alpha[1], scale=1), xlim=c(0, 10), ylim=c(0, 1), col=color[1], lwd=2, type = 'l', main="Cumulative Distribution")
for (i in 2:5)	{	lines(x, pweibull(x, shape=alpha[i], scale=1), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('alpha = 0.5', 'alpha = 1', 'alpha = 2', 'alpha = 4', 'alpha = 8'))

### Survival Function
plot(x, 1-pweibull(x, shape=alpha[1], scale=1), xlim=c(0, 10), ylim=c(0, 1), col=color[1], lwd=2, type = 'l', main="Survival Function")
for (i in 2:5)	{	lines(x, 1-pweibull(x, shape=alpha[i], scale=1), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('alpha = 0.5', 'alpha = 1', 'alpha = 2', 'alpha = 4', 'alpha = 8'))

### Hazard Function
plot(x, dweibull(x, shape=alpha[1], scale=1)/(1-pweibull(x, shape=alpha[1], scale=1)), xlim=c(0, 10), ylim=c(0, 20), col=color[1], lwd=2, type = 'l', main="Hazard Function")
for (i in 2:5) {	lines(x, dweibull(x, shape=alpha[i], scale=1)/(1-pweibull(x, shape=alpha[i], scale=1)), col=color[i], lwd=2);	}
legend('topright', bty = 'n', lwd=2, col=color[1:5], legend = c('alpha = 0.5', 'alpha = 1', 'alpha = 2', 'alpha = 4', 'alpha = 8'))



### parameter: theta
theta = c(0.5, 1, 2, 4, 8)	#scale

### Input Variable
x = seq(0, 10, length.out = 101)

color = rainbow(10)

### Life Distribution
plot(x, dweibull(x, shape=1, scale=theta[1]), xlim=c(0, 10), ylim=c(0, 3), col=color[1], lwd=2, type = 'l', main="Life Distribution")
for (i in 2:5)	{	lines(x, dweibull(x, shape=1, scale=theta[i]), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('theta = 0.5', 'theta = 1', 'theta = 2', 'theta = 4', 'theta = 8'))

### Cumulative Distribution
plot(x, pweibull(x, shape=1, scale=theta[1]), xlim=c(0, 10), ylim=c(0, 1), col=color[1], lwd=2, type = 'l', main="Cumulative Distribution")
for (i in 2:5)	{	lines(x, pweibull(x, shape=1, scale=theta[i]), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('theta = 0.5', 'theta = 1', 'theta = 2', 'theta = 4', 'theta = 8'))

### Survival Function
plot(x, 1-pweibull(x, shape=1, scale=theta[1]), xlim=c(0, 10), ylim=c(0, 1), col=color[1], lwd=2, type = 'l', main="Survival Function")
for (i in 2:5)	{	lines(x, 1-pweibull(x, shape=1, scale=theta[i]), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('theta = 0.5', 'theta = 1', 'theta = 2', 'theta = 4', 'theta = 8'))

### Hazard Function
plot(x, dweibull(x, shape=1, scale=theta[1])/(1-pweibull(x, shape=1, scale=theta[1])), xlim=c(0, 10), ylim=c(0, 5), col=color[1], lwd=2, type = 'l', main="Hazard Function")
for (i in 2:5) {	lines(x, dweibull(x, shape=1, scale=theta[i])/(1-pweibull(x, shape=1, scale=theta[i])), col=color[i], lwd=2);	}
legend('topright', bty = 'n', lwd=2, col=color[1:5], legend = c('theta = 0.5', 'theta = 1', 'theta = 2', 'theta = 4', 'theta = 8'))