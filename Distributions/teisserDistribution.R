setwd("/home/lv999/Dropbox/Github/SurvivalAnalysis/Distributions")
source("colorPalette.R")
require(pracma)


##### teisser Distribution
### parameter
alpha = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
beta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x = seq(-10, 10, length.out = 1000)


### 수명 분포
dteisser = function(x, alpha = 0, beta = 1)
{
	temp = (x-alpha)/beta
    fx = (1/beta) * (exp(temp) - 1) * exp(1 + temp - exp(temp))
    return(fx)
}


### 난수 함수
rteisser = function (n, min=-10, max=10, alpha = 0, beta = 1)
{
    normalization = function(x)	{	(x-min(x))/(max(x)-min(x));	}

	xseq = seq(min, max, length=1000000)
	res = sample(xseq, size=n, prob=normalization(dteisser(xseq, alpha = alpha, beta = beta)), replace=TRUE)
	return(res)
}



### 누적분포함수
pteisser = function(x, alpha = 0, beta = 1)
{
    fx = -(steisser(x, alpha = alpha, beta = beta) - 1)
    return(fx)
}


### 생존함수
steisser = function (x, alpha = 0, beta = 1) 
{
	temp = (x - alpha)/beta
    fx = (1/beta) * (exp(temp) - 1)
    return(fx)
}


### 위험함수
hteisser = function (x, alpha = 0, beta = 1)
{
    fx = dteisser(x, alpha, beta) / steisser(x, alpha, beta)
    return(fx)
}





##### Plot
plot.teisser_seq = function(x, alpha = 0, beta = 1, xlim=c(0, 10), ylim=c(0, 5), func="dteisser")
{
    color=colorPalette(300)

    len_alpha = length(alpha)       # alpha 파라메터의 길이
    len_beta = length(beta)          # beta 파라메터의 길이
    
    color_counter = 1
    for (i in 1:len_alpha)  ### 파라메터: alpha
    {
        color_counter_init = color_counter
        legend_name = NULL;
        
        if (func=="dteisser")     # 수명분포
        {
            plot(x, dteisser(x, alpha=alpha[1], beta=beta[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Life Distribution Function")
            for (j in 1:len_beta)   ### 파라메터: beta
            {
                lines(x, dteisser(x, alpha=alpha[i], beta=beta[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("alpha = ", alpha[i], " / beta = ", beta[j], sep=""))
            }
        }
        else if (func == "pteisser")  # 누적분포함수
        {
            plot(x, pteisser(x, alpha=alpha[1], beta=beta[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Cumulative Distribution Function")
            for (j in 1:len_beta)   ### 파라메터: beta
            {
                lines(x, pteisser(x, alpha=alpha[i], beta=beta[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("alpha = ", alpha[i], " / beta = ", beta[j], sep=""))
            }
        }
        else if (func == "steisser")  # 생존함수
        {
            plot(x, steisser(x, alpha=alpha[1], beta=beta[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Survival Function")
            for (j in 1:len_beta)   ### 파라메터: beta
            {
                lines(x, steisser(x, alpha=alpha[i], beta=beta[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("alpha = ", alpha[i], " / beta = ", beta[j], sep=""))
            }
        }
        else if (func == "hteisser")  # 위험함수
        {
            plot(x, hteisser(x, alpha=alpha[1], beta=beta[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Hazard Function")
            for (j in 1:len_beta)   ### 파라메터: beta
            {
                lines(x, hteisser(x, alpha=alpha[i], beta=beta[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("alpha = ", alpha[i], " / beta = ", beta[j], sep=""))
            }
        }
        legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
    }
}

par(mfrow = c(3, 3))
plot.teisser_seq(x, alpha, beta, xlim=c(min(x), max(x)), ylim=c(0, 5), func="dteisser")

par(mfrow = c(3, 3))
plot.teisser_seq(x, alpha, beta, xlim=c(min(x), max(x)), ylim=c(0, 2), func="pteisser")

par(mfrow = c(3, 3))
plot.teisser_seq(x, alpha, beta, xlim=c(min(x), max(x)), ylim=c(-1, 1), func="steisser")

par(mfrow = c(3, 3))
plot.teisser_seq(x, alpha, beta, xlim=c(min(x), max(x)), ylim=c(-1, 1), func="hteisser")

