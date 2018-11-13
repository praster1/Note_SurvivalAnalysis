##### Gamma Distribution with 2 Parameters
par(mfrow = c(2, 4))

### parameter: beta_p
beta_p = c(0.5, 1, 2, 4, 8)	# shape

### Input Variable
x <- seq(0, 10, length.out = 101)

color = rainbow(10)

### Life Distribution
plot(x, dgamma(x, shape=beta_p[1], scale=1), xlim=c(0, 10), ylim=c(0, 2), col=color[1], lwd=2, type = 'l', main="Life Distribution")
for (i in 2:5)	{	lines(x, dgamma(x, shape=beta_p[i], scale=1), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('beta_p = 0.5', 'beta_p = 1', 'beta_p = 2', 'beta_p = 4', 'beta_p = 8'))

### Cumulative Distribution
plot(x, pgamma(x, shape=beta_p[1], scale=1), xlim=c(0, 10), ylim=c(0, 1), col=color[1], lwd=2, type = 'l', main="Cumulative Distribution")
for (i in 2:5)	{	lines(x, pgamma(x, shape=beta_p[i], scale=1), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('beta_p = 0.5', 'beta_p = 1', 'beta_p = 2', 'beta_p = 4', 'beta_p = 8'))

### Survival Function
plot(x, 1-pgamma(x, shape=beta_p[1], scale=1), xlim=c(0, 10), ylim=c(0, 1), col=color[1], lwd=2, type = 'l', main="Survival Function")
for (i in 2:5)	{	lines(x, 1-pgamma(x, shape=beta_p[i], scale=1), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('beta_p = 0.5', 'beta_p = 1', 'beta_p = 2', 'beta_p = 4', 'beta_p = 8'))

### Hazard Function
plot(x, dgamma(x, shape=beta_p[1], scale=1)/(1-pgamma(x, shape=beta_p[1], scale=1)), xlim=c(0, 10), ylim=c(0, 3), col=color[1], lwd=2, type = 'l', main="Hazard Function")
for (i in 2:5) {	lines(x, dgamma(x, shape=beta_p[i], scale=1)/(1-pgamma(x, shape=beta_p[i], scale=1)), col=color[i], lwd=2);	}
legend('topright', bty = 'n', lwd=2, col=color[1:5], legend = c('beta_p = 0.5', 'beta_p = 1', 'beta_p = 2', 'beta_p = 4', 'beta_p = 8'))



### parameter: theta
theta = c(0.5, 1, 2, 4, 8)	#scale

### Input Variable
x <- seq(0, 10, length.out = 101)

color = rainbow(10)

### Life Distribution
plot(x, dgamma(x, shape=3, scale=theta[1]), xlim=c(0, 10), ylim=c(0, 0.6), col=color[1], lwd=2, type = 'l', main="Life Distribution")
for (i in 2:5)	{	lines(x, dgamma(x, shape=3, scale=theta[i]), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('theta = 0.5', 'theta = 1', 'theta = 2', 'theta = 4', 'theta = 8'))

### Cumulative Distribution
plot(x, pgamma(x, shape=3, scale=theta[1]), xlim=c(0, 10), ylim=c(0, 1), col=color[1], lwd=2, type = 'l', main="Cumulative Distribution")
for (i in 2:5)	{	lines(x, pgamma(x, shape=3, scale=theta[i]), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('theta = 0.5', 'theta = 1', 'theta = 2', 'theta = 4', 'theta = 8'))

### Survival Function
plot(x, 1-pgamma(x, shape=3, scale=theta[1]), xlim=c(0, 10), ylim=c(0, 1), col=color[1], lwd=2, type = 'l', main="Survival Function")
for (i in 2:5)	{	lines(x, 1-pgamma(x, shape=3, scale=theta[i]), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('theta = 0.5', 'theta = 1', 'theta = 2', 'theta = 4', 'theta = 8'))

### Hazard Function
plot(x, dgamma(x, shape=3, scale=theta[1])/(1-pgamma(x, shape=3, scale=theta[1])), xlim=c(0, 10), ylim=c(0, 2), col=color[1], lwd=2, type = 'l', main="Hazard Function")
for (i in 2:5) {	lines(x, dgamma(x, shape=3, scale=theta[i])/(1-pgamma(x, shape=3, scale=theta[i])), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('theta = 0.5', 'theta = 1', 'theta = 2', 'theta = 4', 'theta = 8'))
