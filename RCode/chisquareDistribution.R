##### Chi-square Distribution
### parameter
nu = c(1, 3, 5, 7, 10)	# nu

### Input Variable
x <- seq(0, 10, length.out = 101)


color = rainbow(10)
par(mfrow = c(2, 2))

### Life Distribution
plot(x, dchisq(x, nu[1]), xlim=c(0, 10), ylim=c(0, 1.5), col=color[1], lwd=2, type = 'l', main="Life Distribution")
for (i in 2:5)	{	lines(x, dchisq(x, nu[i]), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('nu = 1', 'nu = 3', 'nu = 5', 'nu = 7', 'nu = 10'))

### Cumulative Distribution
plot(x, pchisq(x, nu[1]), xlim=c(0, 10), ylim=c(0, 1), col=color[1], lwd=2, type = 'l', main="Cumulative Distribution")
for (i in 2:5)	{	lines(x, pchisq(x, nu[i]), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('nu = 1', 'nu = 3', 'nu = 5', 'nu = 7', 'nu = 10'))

### Survival Function
plot(x, 1-pchisq(x, nu[1]), xlim=c(0, 10), ylim=c(0, 1), col=color[1], lwd=2, type = 'l', main="Survival Function")
for (i in 2:5)	{	lines(x, 1-pchisq(x, nu[i]), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('nu = 1', 'nu = 3', 'nu = 5', 'nu = 7', 'nu = 10'))

### Hazard Function
plot(x, dchisq(x, nu[1])/(1-pchisq(x, nu[1])), xlim=c(0, 10), ylim=c(0, 2), col=color[1], lwd=2, type = 'l', main="Hazard Function")
for (i in 2:5) {	lines(x, dchisq(x, nu[i])/(1-pchisq(x, nu[i])), col=color[i], lwd=2);	}
legend('topright', bty = 'n', lwd=2, col=color[1:5], legend = c('nu = 1', 'nu = 3', 'nu = 5', 'nu = 7', 'nu = 10'))
