##### Exponential Distribution
### parameter
lambda = c(0.5, 1, 2, 4, 8)	# lambda

### Input Variable
x <- seq(0, 10, length.out = 101)


color = rainbow(10)
par(mfrow = c(2, 2))

### Life Distribution
plot(x, dexp(x, lambda[1]), xlim=c(0, 10), ylim=c(0, 3), col=color[1], lwd=2, type = 'l', main="Life Distribution")
for (i in 2:5)	{	lines(x, dexp(x, lambda[i]), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('lambda = 0.5', 'lambda = 1', 'lambda = 2', 'lambda = 4', 'lambda = 8'))

### Cumulative Distribution
plot(x, pexp(x, lambda[1]), xlim=c(0, 10), ylim=c(0, 1), col=color[1], lwd=2, type = 'l', main="Cumulative Distribution")
for (i in 2:5)	{	lines(x, pexp(x, lambda[i]), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('lambda = 0.5', 'lambda = 1', 'lambda = 2', 'lambda = 4', 'lambda = 8'))

### Survival Function
plot(x, 1-pexp(x, lambda[1]), xlim=c(0, 10), ylim=c(0, 1), col=color[1], lwd=2, type = 'l', main="Survival Function")
for (i in 2:5)	{	lines(x, 1-pexp(x, lambda[i]), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('lambda = 0.5', 'lambda = 1', 'lambda = 2', 'lambda = 4', 'lambda = 8'))

### Hazard Function
plot(x, dexp(x, lambda[1])/(1-pexp(x, lambda[1])), xlim=c(0, 10), ylim=c(0, 10), col=color[1], lwd=2, type = 'l', main="Hazard Function")
for (i in 2:5) {	lines(x, dexp(x, lambda[i])/(1-pexp(x, lambda[i])), col=color[i], lwd=2);	}
legend('topright', bty = 'n', lwd=2, col=color[1:5], legend = c('lambda = 0.5', 'lambda = 1', 'lambda = 2', 'lambda = 4', 'lambda = 8'))
