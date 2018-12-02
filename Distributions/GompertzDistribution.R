setwd("/home/lv999/Dropbox/Github/SurvivalAnalysis/Distributions")
source("colorPalette.R")


##### Gompertz Distribution
### parameter
alpha = c(0, 0.25, 0.5, 0.75, 1, 2, 4, 8)
beta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x = seq(0.1, 10, length.out = 1000)


### 수명 분포
dgompertz = function (x, alpha = 0, beta = 1) 
{
    fx = alpha * exp(beta * x) * exp(alpha/beta * (1 - exp(beta * x)))
    return(fx)
}


### 난수 함수
rgompertz = function (n, min=0.0001, max=1, alpha = 1, beta = 1) 
{
    normalization = function(x)	{	(x-min(x))/(max(x)-min(x));	}

	xseq = seq(min, max, length=1000000)
	res = sample(xseq, size=n, prob=normalization(dgompertz(xseq, alpha = alpha, beta = beta)), replace=TRUE)
	return(res)
}


### 누적분포함수
pgompertz = function (x, alpha = 0, beta = 1) 
{
    fx = -(sgompertz(x, alpha, beta) - 1)
    return(fx)
}


### 생존함수
sgompertz = function (x, alpha = 0, beta = 1) 
{
    fx = exp(alpha/beta * (1 - exp(beta * x)))
    return(fx)
}


### 위험함수
hgompertz = function (x, alpha = 0, beta = 1) 
{
    fx = dgompertz(x, alpha, beta) / sgompertz(x, alpha, beta)
    return(fx)
}





##### Plot
plot.gompertz_seq = function(x, alpha = 0, beta = 1, xlim=c(0, 10), ylim=c(0, 5), func="dgompertz")
{
    color=colorPalette(300)

    len_alpha = length(alpha)       # alpha 파라메터의 길이
    len_beta = length(beta)          # beta 파라메터의 길이
    
    color_counter = 1
    for (i in 1:len_alpha)  ### 파라메터: alpha
    {
        color_counter_init = color_counter
        legend_name = NULL;
        
        if (func=="dgompertz")     # 수명분포
        {
            plot(x, dgompertz(x, alpha=alpha[1], beta=beta[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Life Distribution Function")
            for (j in 1:len_beta)   ### 파라메터: beta
            {
                lines(x, dgompertz(x, alpha=alpha[i], beta=beta[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("alpha = ", alpha[i], " / beta = ", beta[j], sep=""))
            }
        }
        else if (func == "pgompertz")  # 누적분포함수
        {
            plot(x, pgompertz(x, alpha=alpha[1], beta=beta[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Cumulative Distribution Function")
            for (j in 1:len_beta)   ### 파라메터: beta
            {
                lines(x, pgompertz(x, alpha=alpha[i], beta=beta[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("alpha = ", alpha[i], " / beta = ", beta[j], sep=""))
            }
        }
        else if (func == "sgompertz")  # 생존함수
        {
            plot(x, sgompertz(x, alpha=alpha[1], beta=beta[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Survival Function")
            for (j in 1:len_beta)   ### 파라메터: beta
            {
                lines(x, sgompertz(x, alpha=alpha[i], beta=beta[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("alpha = ", alpha[i], " / beta = ", beta[j], sep=""))
            }
        }
        else if (func == "hgompertz")  # 위험함수
        {
            plot(x, hgompertz(x, alpha=alpha[1], beta=beta[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Hazard Function")
            for (j in 1:len_beta)   ### 파라메터: beta
            {
                lines(x, hgompertz(x, alpha=alpha[i], beta=beta[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("alpha = ", alpha[i], " / beta = ", beta[j], sep=""))
            }
        }
        legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
    }
}

par(mfrow = c(3, 3))
plot.gompertz_seq(x, alpha, beta, xlim=c(min(x), max(x)), ylim=c(0, 1), func="dgompertz")

par(mfrow = c(3, 3))
plot.gompertz_seq(x, alpha, beta, xlim=c(min(x), max(x)), ylim=c(0, 1), func="pgompertz")

par(mfrow = c(3, 3))
plot.gompertz_seq(x, alpha, beta, xlim=c(min(x), max(x)), ylim=c(0, 1), func="sgompertz")

par(mfrow = c(3, 3))
plot.gompertz_seq(x, alpha, beta, xlim=c(min(x), max(x)), ylim=c(0, 20), func="hgompertz")
