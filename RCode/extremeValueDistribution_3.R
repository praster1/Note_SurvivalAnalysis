##### frechet 분포
require(VGAM)
par(mfrow = c(3, 4))

### parameter: mu
mu = c(0.5, 1, 2, 4, 8)	# location

### Input Variable
x <- seq(0, 10, length.out = 101)

color = rainbow(10)

### Life Distributio
plot(x, dfrechet(x, location=mu[1], shape=1, scale=1), xlim=c(0, 10), ylim=c(0, 1), col=color[1], lwd=2, type = 'l', main="Life Distribution")
for (i in 2:5)	{	lines(x, dfrechet(x, location=mu[i], shape=1, scale=1), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('mu = 0.5', 'mu = 1', 'mu = 2', 'mu = 4', 'mu = 8'))

### Cumulative Distribution
plot(x, pfrechet(x, location=mu[1], shape=1, scale=1), xlim=c(0, 10), ylim=c(0, 1), col=color[1], lwd=2, type = 'l', main="Cumulative Distribution")
for (i in 2:5)	{	lines(x, pfrechet(x, location=mu[i], shape=1, scale=1), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('mu = 0.5', 'mu = 1', 'mu = 2', 'mu = 4', 'mu = 8'))

### Survival Function
plot(x, 1-pfrechet(x, location=mu[1], shape=1, scale=1), xlim=c(0, 10), ylim=c(0, 1), col=color[1], lwd=2, type = 'l', main="Survival Function")
for (i in 2:5)	{	lines(x, 1-pfrechet(x, location=mu[i], shape=1, scale=1), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('mu = 0.5', 'mu = 1', 'mu = 2', 'mu = 4', 'mu = 8'))

### Hazard Function
plot(x, dfrechet(x, location=mu[1], shape=1, scale=1)/(1-pfrechet(x, location=mu[1], shape=1, scale=1)), xlim=c(0, 10), ylim=c(0, 1), col=color[1], lwd=2, type = 'l', main="Hazard Function")
for (i in 2:5) {	lines(x, dfrechet(x, location=mu[i], shape=1, scale=1)/(1-pfrechet(x, location=mu[i], shape=1, scale=1)), col=color[i], lwd=2);	}
legend('topright', bty = 'n', lwd=2, col=color[1:5], legend = c('mu = 0.5', 'mu = 1', 'mu = 2', 'mu = 4', 'mu = 8'))




### parameter: m_p
m_p = c(0.5, 1, 2, 4, 8)	# shape

### Input Variable
x <- seq(0, 10, length.out = 101)

color = rainbow(10)

### Life Distributio
plot(x, dfrechet(x, location=0, shape=m_p[1], scale=1), xlim=c(0, 10), ylim=c(0, 3), col=color[1], lwd=2, type = 'l', main="Life Distribution")
for (i in 2:5)	{	lines(x, dfrechet(x, location=0, shape=m_p[i], scale=1), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('m_p = 0.5', 'm_p = 1', 'm_p = 2', 'm_p = 4', 'm_p = 8'))

### Cumulative Distribution
plot(x, pfrechet(x, location=0, shape=m_p[1], scale=1), xlim=c(0, 10), ylim=c(0, 1), col=color[1], lwd=2, type = 'l', main="Cumulative Distribution")
for (i in 2:5)	{	lines(x, pfrechet(x, location=0, shape=m_p[i], scale=1), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('m_p = 0.5', 'm_p = 1', 'm_p = 2', 'm_p = 4', 'm_p = 8'))

### Survival Function
plot(x, 1-pfrechet(x, location=0, shape=m_p[1], scale=1), xlim=c(0, 10), ylim=c(0, 1), col=color[1], lwd=2, type = 'l', main="Survival Function")
for (i in 2:5)	{	lines(x, 1-pfrechet(x, location=0, shape=m_p[i], scale=1), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('m_p = 0.5', 'm_p = 1', 'm_p = 2', 'm_p = 4', 'm_p = 8'))

### Hazard Function
plot(x, dfrechet(x, location=0, shape=m_p[1], scale=1)/(1-pfrechet(x, location=0, shape=m_p[1], scale=1)), xlim=c(0, 10), ylim=c(0, 7), col=color[1], lwd=2, type = 'l', main="Hazard Function")
for (i in 2:5) {	lines(x, dfrechet(x, location=0, shape=m_p[i], scale=1)/(1-pfrechet(x, location=0, shape=m_p[i], scale=1)), col=color[i], lwd=2);	}
legend('topright', bty = 'n', lwd=2, col=color[1:5], legend = c('m_p = 0.5', 'm_p = 1', 'm_p = 2', 'm_p = 4', 'm_p = 8'))



### parameter: beta_p
beta_p = c(0.5, 1, 2, 4, 8)	#scale

### Input Variable
x <- seq(0, 10, length.out = 101)

color = rainbow(10)

### Life Distributio
plot(x, dfrechet(x, location=0, shape=1, scale=beta_p[1]), xlim=c(0, 10), ylim=c(0, 1.5), col=color[1], lwd=2, type = 'l', main="Life Distribution")
for (i in 2:5)	{	lines(x, dfrechet(x, location=0, shape=1, scale=beta_p[i]), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('beta_p = 0.5', 'beta_p = 1', 'beta_p = 2', 'beta_p = 4', 'beta_p = 8'))

### Cumulative Distribution
plot(x, pfrechet(x, location=0, shape=1, scale=beta_p[1]), xlim=c(0, 10), ylim=c(0, 1), col=color[1], lwd=2, type = 'l', main="Cumulative Distribution")
for (i in 2:5)	{	lines(x, pfrechet(x, location=0, shape=1, scale=beta_p[i]), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('beta_p = 0.5', 'beta_p = 1', 'beta_p = 2', 'beta_p = 4', 'beta_p = 8'))

### Survival Function
plot(x, 1-pfrechet(x, location=0, shape=1, scale=beta_p[1]), xlim=c(0, 10), ylim=c(0, 1), col=color[1], lwd=2, type = 'l', main="Survival Function")
for (i in 2:5)	{	lines(x, 1-pfrechet(x, location=0, shape=1, scale=beta_p[i]), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('beta_p = 0.5', 'beta_p = 1', 'beta_p = 2', 'beta_p = 4', 'beta_p = 8'))

### Hazard Function
plot(x, dfrechet(x, location=0, shape=1, scale=beta_p[1])/(1-pfrechet(x, location=0, shape=1, scale=beta_p[1])), xlim=c(0, 10), ylim=c(0, 1.5), col=color[1], lwd=2, type = 'l', main="Hazard Function")
for (i in 2:5) {	lines(x, dfrechet(x, location=0, shape=1, scale=beta_p[i])/(1-pfrechet(x, location=0, shape=1, scale=beta_p[i])), col=color[i], lwd=2);	}
legend('topright', bty = 'n', lwd=2, col=color[1:5], legend = c('beta_p = 0.5', 'beta_p = 1', 'beta_p = 2', 'beta_p = 4', 'beta_p = 8'))