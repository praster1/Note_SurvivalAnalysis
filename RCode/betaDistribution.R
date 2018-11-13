##### Beta Distribution
par(mfrow = c(2, 4))

### parameter: p_p
p_p = c(0.25,  0.5, 1, 2, 4)	# shape1

### Input Variable
x <- seq(0, 10, length.out = 101)

color = rainbow(10)

### Life Distribution
plot(x, dbeta(x, shape=p_p[1], shape2=1), xlim=c(0, 1), ylim=c(0, 5), col=color[1], lwd=2, type = 'l', main="Life Distribution")
for (i in 2:5)	{	lines(x, dbeta(x, shape=p_p[i], shape2=1), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('p_p = 0.5', 'p_p = 1', 'p_p = 2', 'p_p = 4', 'p_p = 8'))

### Cumulative Distribution
plot(x, pbeta(x, shape=p_p[1], shape2=1), xlim=c(0, 1), ylim=c(0, 1), col=color[1], lwd=2, type = 'l', main="Cumulative Distribution")
for (i in 2:5)	{	lines(x, pbeta(x, shape=p_p[i], shape2=1), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('p_p = 0.5', 'p_p = 1', 'p_p = 2', 'p_p = 4', 'p_p = 8'))

### Survival Function
plot(x, 1-pbeta(x, shape=p_p[1], shape2=1), xlim=c(0, 1), ylim=c(0, 1), col=color[1], lwd=2, type = 'l', main="Survival Function")
for (i in 2:5)	{	lines(x, 1-pbeta(x, shape=p_p[i], shape2=1), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('p_p = 0.5', 'p_p = 1', 'p_p = 2', 'p_p = 4', 'p_p = 8'))

### Hazard Function
plot(x, dbeta(x, shape=p_p[1], shape2=1)/(1-pbeta(x, shape=p_p[1], shape2=1)), xlim=c(0, 1), ylim=c(0, 10), col=color[1], lwd=2, type = 'l', main="Hazard Function")
for (i in 2:5) {	lines(x, dbeta(x, shape=p_p[i], shape2=1)/(1-pbeta(x, shape=p_p[i], shape2=1)), col=color[i], lwd=2);	}
legend('topright', bty = 'n', lwd=2, col=color[1:5], legend = c('p_p = 0.5', 'p_p = 1', 'p_p = 2', 'p_p = 4', 'p_p = 8'))



### parameter: q_p
q_p = c(0.25,  0.5, 1, 2, 4)	# shape2

### Input Variable
x <- seq(0, 10, length.out = 101)

color = rainbow(10)

### Life Distribution
plot(x, dbeta(x, shape=3, shape2=q_p[1]), xlim=c(0, 1), ylim=c(0, 5), col=color[1], lwd=2, type = 'l', main="Life Distribution")
for (i in 2:5)	{	lines(x, dbeta(x, shape=3, shape2=q_p[i]), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('q_p = 0.5', 'q_p = 1', 'q_p = 2', 'q_p = 4', 'q_p = 8'))

### Cumulative Distribution
plot(x, pbeta(x, shape=3, shape2=q_p[1]), xlim=c(0, 1), ylim=c(0, 1), col=color[1], lwd=2, type = 'l', main="Cumulative Distribution")
for (i in 2:5)	{	lines(x, pbeta(x, shape=3, shape2=q_p[i]), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('q_p = 0.5', 'q_p = 1', 'q_p = 2', 'q_p = 4', 'q_p = 8'))

### Survival Function
plot(x, 1-pbeta(x, shape=3, shape2=q_p[1]), xlim=c(0, 1), ylim=c(0, 1), col=color[1], lwd=2, type = 'l', main="Survival Function")
for (i in 2:5)	{	lines(x, 1-pbeta(x, shape=3, shape2=q_p[i]), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('q_p = 0.5', 'q_p = 1', 'q_p = 2', 'q_p = 4', 'q_p = 8'))

### Hazard Function
plot(x, dbeta(x, shape=3, shape2=q_p[1])/(1-pbeta(x, shape=3, shape2=q_p[1])), xlim=c(0, 1), ylim=c(0, 10), col=color[1], lwd=2, type = 'l', main="Hazard Function")
for (i in 2:5) {	lines(x, dbeta(x, shape=3, shape2=q_p[i])/(1-pbeta(x, shape=3, shape2=q_p[i])), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('q_p = 0.5', 'q_p = 1', 'q_p = 2', 'q_p = 4', 'q_p = 8'))
