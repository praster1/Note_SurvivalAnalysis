##### Weibull Distribution with 3 parameters
### parameter: alpha
gamma_p = c(0.5, 1, 2, 4, 8)	# location

### Input Variable
x = seq(0, 10, length.out = 101)

color = rainbow(10)
par(mfrow = c(2, 2))


### Life Distribution
plot(x, dweibull(x-gamma_p[1], shape=3, scale=1), xlim=c(0, 10), ylim=c(0, 1.5), col=color[1], lwd=2, type = 'l', main="Life Distribution")
for (i in 2:5)	{	lines(x, dweibull(x-gamma_p[i], shape=3, scale=1), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('theta = 0.5', 'theta = 1', 'theta = 2', 'theta = 4', 'theta = 8'))

### Cumulative Distribution
plot(x, pweibull(x-gamma_p[1], shape=3, scale=1), xlim=c(0, 10), ylim=c(0, 1), col=color[1], lwd=2, type = 'l', main="Cumulative Distribution")
for (i in 2:5)	{	lines(x, pweibull(x-gamma_p[i], shape=3, scale=1), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('theta = 0.5', 'theta = 1', 'theta = 2', 'theta = 4', 'theta = 8'))

### Survival Function
plot(x, 1-pweibull(x-gamma_p[1], shape=3, scale=1), xlim=c(0, 10), ylim=c(0, 1), col=color[1], lwd=2, type = 'l', main="Survival Function")
for (i in 2:5)	{	lines(x, 1-pweibull(x-gamma_p[i], shape=3, scale=1), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('theta = 0.5', 'theta = 1', 'theta = 2', 'theta = 4', 'theta = 8'))

### Hazard Function
plot(x, dweibull(x-gamma_p[1], shape=3, scale=1)/(1-pweibull(x-gamma_p[1], shape=3, scale=1)), xlim=c(0, 10), ylim=c(0, 10), col=color[1], lwd=2, type = 'l', main="Hazard Function")
for (i in 2:5) {	lines(x, dweibull(x-gamma_p[i], shape=3, scale=1)/(1-pweibull(x-gamma_p[i], shape=3, scale=1)), col=color[i], lwd=2);	}
legend('topright', bty = 'n', lwd=2, col=color[1:5], legend = c('theta = 0.5', 'theta = 1', 'theta = 2', 'theta = 4', 'theta = 8'))