##### 로그 정규 분포
par(mfrow = c(2, 4))

### parameter: mu
mu = c(0, 0.25, 0.5, 1, 2)	# meanlog

### Input Variable
x <- seq(0, 10, length.out = 101)

color = rainbow(10)

### Life Distribution
plot(x, dlnorm(x, meanlog=mu[1], sdlog=1), xlim=c(0, 10), ylim=c(0, 1), col=color[1], lwd=2, type = 'l', main="Life Distribution")
for (i in 2:5)	{	lines(x, dlnorm(x, meanlog=mu[i], sdlog=1), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('mu = 0', 'mu = 0.25', 'mu = 0.5', 'mu = 1', 'mu = 2'))

### Cumulative Distribution
plot(x, plnorm(x, meanlog=mu[1], sdlog=1), xlim=c(0, 10), ylim=c(0, 1), col=color[1], lwd=2, type = 'l', main="Cumulative Distribution")
for (i in 2:5)	{	lines(x, plnorm(x, meanlog=mu[i], sdlog=1), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('mu = 0', 'mu = 0.25', 'mu = 0.5', 'mu = 1', 'mu = 2'))

### Survival Function
plot(x, 1-plnorm(x, meanlog=mu[1], sdlog=1), xlim=c(0, 10), ylim=c(0, 1), col=color[1], lwd=2, type = 'l', main="Survival Function")
for (i in 2:5)	{	lines(x, 1-plnorm(x, meanlog=mu[i], sdlog=1), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('mu = 0', 'mu = 0.25', 'mu = 0.5', 'mu = 1', 'mu = 2'))

### Hazard Function
plot(x, dlnorm(x, meanlog=mu[1], sdlog=1)/(1-plnorm(x, meanlog=mu[1], sdlog=1)), xlim=c(0, 10), ylim=c(0, 1), col=color[1], lwd=2, type = 'l', main="Hazard Function")
for (i in 2:5) {	lines(x, dlnorm(x, meanlog=mu[i], sdlog=1)/(1-plnorm(x, meanlog=mu[i], sdlog=1)), col=color[i], lwd=2);	}
legend('topright', bty = 'n', lwd=2, col=color[1:5], legend = c('mu = 0', 'mu = 0.25', 'mu = 0.5', 'mu = 1', 'mu = 2'))



### parameter: sigma
sigma = c(0.25, 0.5, 1, 2, 4)	#sdlog

### Input Variable
x <- seq(0, 10, length.out = 101)

color = rainbow(10)

### Life Distribution
plot(x, dlnorm(x, meanlog=3, sdlog=sigma[1]), xlim=c(0, 10), ylim=c(0, 0.5), col=color[1], lwd=2, type = 'l', main="Life Distribution")
for (i in 2:5)	{	lines(x, dlnorm(x, meanlog=3, sdlog=sigma[i]), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('sigma = 0.25', 'sigma = 0.5', 'sigma = 1', 'sigma = 2', 'sigma = 4'))

### Cumulative Distribution
plot(x, plnorm(x, meanlog=3, sdlog=sigma[1]), xlim=c(0, 10), ylim=c(0, 0.5), col=color[1], lwd=2, type = 'l', main="Cumulative Distribution")
for (i in 2:5)	{	lines(x, plnorm(x, meanlog=3, sdlog=sigma[i]), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('sigma = 0.25', 'sigma = 0.5', 'sigma = 1', 'sigma = 2', 'sigma = 4'))

### Survival Function
plot(x, 1-plnorm(x, meanlog=3, sdlog=sigma[1]), xlim=c(0, 10), ylim=c(0, 1), col=color[1], lwd=2, type = 'l', main="Survival Function")
for (i in 2:5)	{	lines(x, 1-plnorm(x, meanlog=3, sdlog=sigma[i]), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('sigma = 0.25', 'sigma = 0.5', 'sigma = 1', 'sigma = 2', 'sigma = 4'))

### Hazard Function
plot(x, dlnorm(x, meanlog=3, sdlog=sigma[1])/(1-plnorm(x, meanlog=3, sdlog=sigma[1])), xlim=c(0, 10), ylim=c(0, 0.5), col=color[1], lwd=2, type = 'l', main="Hazard Function")
for (i in 2:5) {	lines(x, dlnorm(x, meanlog=3, sdlog=sigma[i])/(1-plnorm(x, meanlog=3, sdlog=sigma[i])), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('sigma = 0.25', 'sigma = 0.5', 'sigma = 1', 'sigma = 2', 'sigma = 4'))