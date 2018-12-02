setwd("/home/lv999/Dropbox/Github/SurvivalAnalysis/Distributions")
source("colorPalette.R")


##### weibull Distribution with gamma Parameters
### parameter
alpha = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
beta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)
gamma = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x = seq(-10, 10, length.out = 1000)


### 수명 분포
dinverseweibull = function(x, alpha=0, beta=1, gamma=1)
{
	temp = (x-alpha)/beta
    fx = (gamma/beta) * temp^(-gamma-1) * exp(-temp^(-gamma))
    return(fx)
}


### 난수 함수
rinverseweibull = function (n, min=0.1, max=10, alpha = 0, beta = 1, gamma = 1) 
{
    normalization = function(x)	{	(x-min(x))/(max(x)-min(x));	}

	xseq = seq(min, max, length=1000000)
	res = sample(xseq, size=n, prob=normalization(dinverseweibull(xseq, alpha = alpha, beta = beta, gamma = gamma)), replace=TRUE)
	return(res)
}



### 누적분포함수
pinverseweibull = function(x, alpha=0, beta=1, gamma=1)
{
    fx = -(sinverseweibull(x, alpha=alpha, beta=beta, gamma=gamma) - 1)
    return(fx)
}


### 생존함수
sinverseweibull = function(x, alpha=0, beta=1, gamma=1)
{
	temp = (x-alpha)/beta
    fx = 1 - exp(-temp^(-gamma))
    return(fx)
}


### 위험함수
hinverseweibull = function(x, alpha=0, beta=1, gamma=1)
{
    fx = dinverseweibull(x, alpha=alpha, beta=beta, gamma=gamma) / sinverseweibull(x, alpha=alpha, beta=beta, gamma=gamma)
    return(fx)
}





##### Plot
plot.inverseweibull_seq = function(x, alpha = 0, beta = 1, gamma = 1, xlim=c(0, 10), ylim=c(0, 5), func="dinverseweibull")
{
    color=colorPalette(300)

    len_gamma = length(gamma)       # gamma 파라메터의 길이
    len_alpha = length(alpha)          # alpha 파라메터의 길이
    len_beta = length(beta)  # beta 파라메터의 길이
    
    color_counter = 1
    for (i in 1:len_gamma)  ### 파라메터: gamma
    {
        if (func=="dinverseweibull")     # 수명분포
        {
            for (j in 1:len_alpha)   ### 파라메터: alpha
            {
                color_counter_init = color_counter
                legend_name = NULL;
                plot(x, dinverseweibull(x, gamma=gamma[1], alpha=alpha[1], beta=beta[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Life Distribution Function")
                for (k in 1:len_beta)   ### 파라메터: beta
                {
                    lines(x, dinverseweibull(x, gamma=gamma[i], alpha=alpha[j], beta=beta[k]), col=color[color_counter], lwd=2);
                    color_counter = color_counter + 1;
                    legend_name = c(legend_name, paste("gamma = ", gamma[i], " / alpha = ", alpha[j], " / beta = ", beta[k], sep=""))
                }
                legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
            }
        }
        else if (func == "pinverseweibull")  # 누적분포함수
        {
            for (j in 1:len_alpha)   ### 파라메터: alpha
            {
                color_counter_init = color_counter
                legend_name = NULL;
                plot(x, pinverseweibull(x, gamma=gamma[1], alpha=alpha[1], beta=beta[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Cumulative Distribution Function")
                for (k in 1:len_beta)   ### 파라메터: beta
                {
                    lines(x, pinverseweibull(x, gamma=gamma[i], alpha=alpha[j], beta=beta[k]), col=color[color_counter], lwd=2);
                    color_counter = color_counter + 1;
                    legend_name = c(legend_name, paste("gamma = ", gamma[i], " / alpha = ", alpha[j], " / beta = ", beta[k], sep=""))
                }
                legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
            }
        }
        else if (func == "sinverseweibull")  # 생존함수
        {
            for (j in 1:len_alpha)   ### 파라메터: alpha
            {
                color_counter_init = color_counter
                legend_name = NULL;
                plot(x, sinverseweibull(x, gamma=gamma[1], alpha=alpha[1], beta=beta[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Survival Function")
                for (k in 1:len_beta)   ### 파라메터: beta
                {
                    lines(x, sinverseweibull(x, gamma=gamma[i], alpha=alpha[j], beta=beta[k]), col=color[color_counter], lwd=2);
                    color_counter = color_counter + 1;
                    legend_name = c(legend_name, paste("gamma = ", gamma[i], " / alpha = ", alpha[j], " / beta = ", beta[k], sep=""))
                }
                legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
            }
        }
        else if (func == "hinverseweibull")  # 위험함수
        {
            for (j in 1:len_alpha)   ### 파라메터: alpha
            {
                color_counter_init = color_counter
                legend_name = NULL;
                plot(x, hinverseweibull(x, gamma=gamma[1], alpha=alpha[1], beta=beta[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Hazard Function")
                for (k in 1:len_beta)   ### 파라메터: beta
                {
                    lines(x, hinverseweibull(x, gamma=gamma[i], alpha=alpha[j], beta=beta[k]), col=color[color_counter], lwd=2);
                    color_counter = color_counter + 1;
                    legend_name = c(legend_name, paste("gamma = ", gamma[i], " / alpha = ", alpha[j], " / beta = ", beta[k], sep=""))
                }
                legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
            }
        }
    }
}

par(mfrow = c(9, 8))
plot.inverseweibull_seq(x, alpha, beta, gamma, xlim=c(min(x), max(x)), ylim=c(-10, 10), func="dinverseweibull")

par(mfrow = c(9, 8))
plot.inverseweibull_seq(x, alpha, beta, gamma, xlim=c(min(x), max(x)), ylim=c(-5, 5), func="pinverseweibull")

par(mfrow = c(9, 8))
plot.inverseweibull_seq(x, alpha, beta, gamma, xlim=c(min(x), max(x)), ylim=c(-5, 5), func="sinverseweibull")

par(mfrow = c(9, 8))
plot.inverseweibull_seq(x, alpha, beta, gamma, xlim=c(min(x), max(x)), ylim=c(-30, 30), func="hinverseweibull")