##### 메이크햄 분포
library(VGAM) 

### parameter
theta = c(0.5, 1, 2, 4, 8)	# theta

### Input Variable
x <- seq(0, 10, length.out = 101)


color = rainbow(10)
par(mfrow = c(2, 2))

### Life Distribution
plot(x, dmakeham(x, shape=theta[1]), xlim=c(0, 10), ylim=c(0, 5), col=color[1], lwd=2, type = 'l', main="Life Distribution")
for (i in 2:5)	{	lines(x, dmakeham(x, shape=theta[i]), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('shape=theta = 0.5', 'shape=theta = 1', 'shape=theta = 2', 'shape=theta = 4', 'shape=theta = 8'))

### Cumulative Distribution
plot(x, pmakeham(x, shape=theta[1]), xlim=c(0, 10), ylim=c(0, 1), col=color[1], lwd=2, type = 'l', main="Cumulative Distribution")
for (i in 2:5)	{	lines(x, pmakeham(x, shape=theta[i]), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('shape=theta = 0.5', 'shape=theta = 1', 'shape=theta = 2', 'shape=theta = 4', 'shape=theta = 8'))

### Survival Function
plot(x, 1-pmakeham(x, shape=theta[1]), xlim=c(0, 10), ylim=c(0, 1), col=color[1], lwd=2, type = 'l', main="Survival Function")
for (i in 2:5)	{	lines(x, 1-pmakeham(x, shape=theta[i]), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('shape=theta = 0.5', 'shape=theta = 1', 'shape=theta = 2', 'shape=theta = 4', 'shape=theta = 8'))

### Hazard Function
plot(x, dmakeham(x, shape=theta[1])/(1-pmakeham(x, shape=theta[1])), xlim=c(0, 10), ylim=c(0, 20), col=color[1], lwd=2, type = 'l', main="Hazard Function")
for (i in 2:5) {	lines(x, dmakeham(x, shape=theta[i])/(1-pmakeham(x, shape=theta[i])), col=color[i], lwd=2);	}
legend('topright', bty = 'n', lwd=2, col=color[1:5], legend = c('shape=theta = 0.5', 'shape=theta = 1', 'shape=theta = 2', 'shape=theta = 4', 'shape=theta = 8'))
