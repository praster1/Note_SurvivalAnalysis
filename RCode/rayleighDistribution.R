##### 레일리 분포
library(VGAM) 

### parameter
k_p = c(0.5, 1, 2, 4, 8)	# k_p

### Input Variable
x <- seq(0, 10, length.out = 101)


color = rainbow(10)
par(mfrow = c(2, 2))

### Life Distribution
plot(x, drayleigh(x, k_p[1]), xlim=c(0, 10), ylim=c(0, 1.5), col=color[1], lwd=2, type = 'l', main="Life Distribution")
for (i in 2:5)	{	lines(x, drayleigh(x, k_p[i]), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('k_p = 0.5', 'k_p = 1', 'k_p = 2', 'k_p = 4', 'k_p = 8'))

### Cumulative Distribution
plot(x, prayleigh(x, k_p[1]), xlim=c(0, 10), ylim=c(0, 1), col=color[1], lwd=2, type = 'l', main="Cumulative Distribution")
for (i in 2:5)	{	lines(x, prayleigh(x, k_p[i]), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('k_p = 0.5', 'k_p = 1', 'k_p = 2', 'k_p = 4', 'k_p = 8'))

### Survival Function
plot(x, 1-prayleigh(x, k_p[1]), xlim=c(0, 10), ylim=c(0, 1), col=color[1], lwd=2, type = 'l', main="Survival Function")
for (i in 2:5)	{	lines(x, 1-prayleigh(x, k_p[i]), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('k_p = 0.5', 'k_p = 1', 'k_p = 2', 'k_p = 4', 'k_p = 8'))

### Hazard Function
plot(x, drayleigh(x, k_p[1])/(1-prayleigh(x, k_p[1])), xlim=c(0, 10), ylim=c(0, 10), col=color[1], lwd=2, type = 'l', main="Hazard Function")
for (i in 2:5) {	lines(x, drayleigh(x, k_p[i])/(1-prayleigh(x, k_p[i])), col=color[i], lwd=2);	}
legend('topright', bty = 'n', lwd=2, col=color[1:5], legend = c('k_p = 0.5', 'k_p = 1', 'k_p = 2', 'k_p = 4', 'k_p = 8'))