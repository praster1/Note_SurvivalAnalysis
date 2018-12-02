
source("colorPalette.R")


##### logweibull Distribution
### parameter
alpha = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
beta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x = seq(0.1, 10, length.out = 1000)


### 수명 분포
dlogweibull = function(x, alpha = 0, beta = 1)
{
	temp = (x-alpha)/beta
    fx = (1/beta) * exp(temp - exp(temp))
    return(fx)
}


### 난수 함수
rlogweibull = function (n, min=0.0001, max=1, alpha = 1, beta = 1) 
{
    normalization = function(x)	{	(x-min(x))/(max(x)-min(x));	}

	xseq = seq(min, max, length=1000000)
	res = sample(xseq, size=n, prob=normalization(dlogweibull(xseq, alpha = alpha, beta = beta)), replace=TRUE)
	return(res)
}


### 누적분포함수
plogweibull = function(x, alpha = 0, beta = 1)
{
    fx = -(slogweibull(x, alpha = alpha, beta = beta) - 1)
    return(fx)
}


### 생존함수
slogweibull = function (x, alpha = 0, beta = 1) 
{
	temp = (x-alpha)/beta
    fx = exp(- exp(temp))
    return(fx)
}


### 위험함수
hlogweibull = function (x, alpha = 0, beta = 1)
{
    fx = dlogweibull(x, alpha, beta) / slogweibull(x, alpha, beta)
    return(fx)
}





##### Plot
plot.logweibull_seq = function(x, alpha = 0, beta = 1, xlim=c(0, 10), ylim=c(0, 5), func="dlogweibull")
{
    color=colorPalette(300)

    len_alpha = length(alpha)       # alpha 파라메터의 길이
    len_beta = length(beta)          # beta 파라메터의 길이
    
    color_counter = 1
    for (i in 1:len_alpha)  ### 파라메터: alpha
    {
        color_counter_init = color_counter
        legend_name = NULL;
        
        if (func=="dlogweibull")     # 수명분포
        {
            plot(x, dlogweibull(x, alpha=alpha[1], beta=beta[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Life Distribution Function")
            for (j in 1:len_beta)   ### 파라메터: beta
            {
                lines(x, dlogweibull(x, alpha=alpha[i], beta=beta[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("alpha = ", alpha[i], " / beta = ", beta[j], sep=""))
            }
        }
        else if (func == "plogweibull")  # 누적분포함수
        {
            plot(x, plogweibull(x, alpha=alpha[1], beta=beta[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Cumulative Distribution Function")
            for (j in 1:len_beta)   ### 파라메터: beta
            {
                lines(x, plogweibull(x, alpha=alpha[i], beta=beta[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("alpha = ", alpha[i], " / beta = ", beta[j], sep=""))
            }
        }
        else if (func == "slogweibull")  # 생존함수
        {
            plot(x, slogweibull(x, alpha=alpha[1], beta=beta[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Survival Function")
            for (j in 1:len_beta)   ### 파라메터: beta
            {
                lines(x, slogweibull(x, alpha=alpha[i], beta=beta[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("alpha = ", alpha[i], " / beta = ", beta[j], sep=""))
            }
        }
        else if (func == "hlogweibull")  # 위험함수
        {
            plot(x, hlogweibull(x, alpha=alpha[1], beta=beta[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Hazard Function")
            for (j in 1:len_beta)   ### 파라메터: beta
            {
                lines(x, hlogweibull(x, alpha=alpha[i], beta=beta[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("alpha = ", alpha[i], " / beta = ", beta[j], sep=""))
            }
        }
        legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
    }
}

par(mfrow = c(3, 3))
plot.logweibull_seq(x, alpha, beta, xlim=c(min(x), max(x)), ylim=c(0, 5), func="dlogweibull")

par(mfrow = c(3, 3))
plot.logweibull_seq(x, alpha, beta, xlim=c(min(x), max(x)), ylim=c(0, 1), func="plogweibull")

par(mfrow = c(3, 3))
plot.logweibull_seq(x, alpha, beta, xlim=c(min(x), max(x)), ylim=c(0, 1), func="slogweibull")

par(mfrow = c(3, 3))
plot.logweibull_seq(x, alpha, beta, xlim=c(min(x), max(x)), ylim=c(0, 10), func="hlogweibull")

