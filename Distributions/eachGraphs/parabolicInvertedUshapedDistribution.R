
source("colorPalette.R")


##### parabolicInvertedUshaped Distribution
### parameter
alpha = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
beta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x = seq(-10, 10, length.out = 1000)


### 수명 분포
dparabolicInvertedUshaped = function(x, alpha = 0, beta = 1)
{
    fx = (3/(4*beta)) * (1 - ((x - alpha)/beta)^2)
    return(fx)
}


### 난수 함수
rparabolicInvertedUshaped = function (n, min=-10, max=10, alpha = 0, beta = 1) 
{
    normalization = function(x)	{	(x-min(x))/(max(x)-min(x));	}

	xseq = seq(min, max, length=1000000)
	res = sample(xseq, size=n, prob=normalization(dparabolicInvertedUshaped(xseq, alpha = alpha, beta = beta)), replace=TRUE)
	return(res)
}


### 누적분포함수
pparabolicInvertedUshaped = function(x, alpha = 0, beta = 1)
{
    fx = -(sparabolicInvertedUshaped(x, alpha = alpha, beta = beta) - 1)
    return(fx)
}


### 생존함수
sparabolicInvertedUshaped = function (x, alpha = 0, beta = 1) 
{
    fx = (1/2) - (1/4) * (3*((x-alpha)/beta) - ((x-alpha)/beta)^3)
    return(fx)
}


### 위험함수
hparabolicInvertedUshaped = function (x, alpha = 0, beta = 1)
{
    fx = dparabolicInvertedUshaped(x, alpha, beta) / sparabolicInvertedUshaped(x, alpha, beta)
    return(fx)
}





##### Plot
plot.parabolicInvertedUshaped_seq = function(x, alpha = 0, beta = 1, xlim=c(0, 10), ylim=c(0, 5), func="dparabolicInvertedUshaped")
{
    color=colorPalette(300)

    len_alpha = length(alpha)       # alpha 파라메터의 길이
    len_beta = length(beta)          # beta 파라메터의 길이
    
    color_counter = 1
    for (i in 1:len_alpha)  ### 파라메터: alpha
    {
        color_counter_init = color_counter
        legend_name = NULL;
        
        if (func=="dparabolicInvertedUshaped")     # 수명분포
        {
            plot(x, dparabolicInvertedUshaped(x, alpha=alpha[1], beta=beta[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Life Distribution Function")
            for (j in 1:len_beta)   ### 파라메터: beta
            {
                lines(x, dparabolicInvertedUshaped(x, alpha=alpha[i], beta=beta[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("alpha = ", alpha[i], " / beta = ", beta[j], sep=""))
            }
        }
        else if (func == "pparabolicInvertedUshaped")  # 누적분포함수
        {
            plot(x, pparabolicInvertedUshaped(x, alpha=alpha[1], beta=beta[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Cumulative Distribution Function")
            for (j in 1:len_beta)   ### 파라메터: beta
            {
                lines(x, pparabolicInvertedUshaped(x, alpha=alpha[i], beta=beta[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("alpha = ", alpha[i], " / beta = ", beta[j], sep=""))
            }
        }
        else if (func == "sparabolicInvertedUshaped")  # 생존함수
        {
            plot(x, sparabolicInvertedUshaped(x, alpha=alpha[1], beta=beta[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Survival Function")
            for (j in 1:len_beta)   ### 파라메터: beta
            {
                lines(x, sparabolicInvertedUshaped(x, alpha=alpha[i], beta=beta[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("alpha = ", alpha[i], " / beta = ", beta[j], sep=""))
            }
        }
        else if (func == "hparabolicInvertedUshaped")  # 위험함수
        {
            plot(x, hparabolicInvertedUshaped(x, alpha=alpha[1], beta=beta[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Hazard Function")
            for (j in 1:len_beta)   ### 파라메터: beta
            {
                lines(x, hparabolicInvertedUshaped(x, alpha=alpha[i], beta=beta[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("alpha = ", alpha[i], " / beta = ", beta[j], sep=""))
            }
        }
        legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
    }
}

par(mfrow = c(3, 3))
plot.parabolicInvertedUshaped_seq(x, alpha, beta, xlim=c(min(x), max(x)), ylim=c(0, 5), func="dparabolicInvertedUshaped")

par(mfrow = c(3, 3))
plot.parabolicInvertedUshaped_seq(x, alpha, beta, xlim=c(min(x), max(x)), ylim=c(0, 1), func="pparabolicInvertedUshaped")

par(mfrow = c(3, 3))
plot.parabolicInvertedUshaped_seq(x, alpha, beta, xlim=c(min(x), max(x)), ylim=c(0, 1), func="sparabolicInvertedUshaped")

par(mfrow = c(3, 3))
plot.parabolicInvertedUshaped_seq(x, alpha, beta, xlim=c(min(x), max(x)), ylim=c(0, 10), func="hparabolicInvertedUshaped")