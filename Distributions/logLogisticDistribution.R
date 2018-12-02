# https://www.rdocumentation.org/packages/actuar/versions/2.3-1/topics/Loglogistic

setwd("/home/lv999/Dropbox/Github/SurvivalAnalysis/Distributions")
source("colorPalette.R")


##### loglogistic Distribution
### parameter
alpha = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
beta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)
gamma = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x = seq(0.1, 10, length.out = 1000)


### 수명 분포
dloglogistic = function(x, alpha = 1, beta = 1, gamma = 1)
{
    fx = (gamma/beta) * ((x - alpha)/beta)^(gamma - 1) * (1 + ((x - alpha) / beta)^gamma)^(-2)
    return(fx)
}


### 난수 함수
rloglogistic = function (n, min=0.0001, max=10, alpha = 1, beta = 1, gamma = 1) 
{
    normalization = function(x)	{	(x-min(x))/(max(x)-min(x));	}

	xseq = seq(min, max, length=1000000)
	res = sample(xseq, size=n, prob=normalization(dloglogistic(xseq, alpha = alpha, beta = beta, gamma = gamma)), replace=TRUE)
	return(res)
}


### 누적분포함수
ploglogistic = function(x, alpha = 1, beta = 1, gamma = 1)
{
    fx = -(sloglogistic(x, alpha, beta) - 1)
    return(fx)
}


### 생존함수
sloglogistic = function (x, alpha = 1, beta = 1, gamma = 1)
{
    fx = (gamma/beta) * ((x - alpha) / beta)^(gamma - 1) * (1 + ((x - alpha) / beta)^gamma)^(-1)
    return(fx)
}


### 위험함수
hloglogistic = function (x, alpha = 1, beta = 1, gamma = 1)
{
    fx = dloglogistic(x, alpha, beta, gamma) / sloglogistic(x, alpha, beta, gamma)
    return(fx)
}





##### Plot
plot.loglogistic_seq = function(x, alpha = 1, beta = 1, gamma = 1, xlim=c(0, 10), ylim=c(0, 5), func="dloglogistic")
{
    color=colorPalette(300)

    len_alpha = length(alpha)       # alpha 파라메터의 길이
    len_beta = length(beta)          # beta 파라메터의 길이
    len_gamma = length(gamma)  # gamma 파라메터의 길이
    
    color_counter = 1
    for (i in 1:len_alpha)  ### 파라메터: alpha
    {
        if (func=="dloglogistic")     # 수명분포
        {
            for (j in 1:len_beta)   ### 파라메터: beta
            {
                color_counter_init = color_counter
                legend_name = NULL;
                plot(x, dloglogistic(x, alpha=alpha[1], beta=beta[1], gamma=gamma[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Life Distribution Function")
                for (k in 1:len_gamma)   ### 파라메터: gamma
                {
                    lines(x, dloglogistic(x, alpha=alpha[i], beta=beta[j], gamma=gamma[k]), col=color[color_counter], lwd=2);
                    color_counter = color_counter + 1;
                    legend_name = c(legend_name, paste("alpha = ", alpha[i], " / beta = ", beta[j], " / gamma = ", gamma[k], sep=""))
                }
                legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
            }
        }
        else if (func == "ploglogistic")  # 누적분포함수
        {
            for (j in 1:len_beta)   ### 파라메터: beta
            {
                color_counter_init = color_counter
                legend_name = NULL;
                plot(x, ploglogistic(x, alpha=alpha[1], beta=beta[1], gamma=gamma[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Cumulative Distribution Function")
                for (k in 1:len_gamma)   ### 파라메터: gamma
                {
                    lines(x, ploglogistic(x, alpha=alpha[i], beta=beta[j], gamma=gamma[k]), col=color[color_counter], lwd=2);
                    color_counter = color_counter + 1;
                    legend_name = c(legend_name, paste("alpha = ", alpha[i], " / beta = ", beta[j], " / gamma = ", gamma[k], sep=""))
                }
                legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
            }
        }
        else if (func == "sloglogistic")  # 생존함수
        {
            for (j in 1:len_beta)   ### 파라메터: beta
            {
                color_counter_init = color_counter
                legend_name = NULL;
                plot(x, sloglogistic(x, alpha=alpha[1], beta=beta[1], gamma=gamma[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Survival Function")
                for (k in 1:len_gamma)   ### 파라메터: gamma
                {
                    lines(x, sloglogistic(x, alpha=alpha[i], beta=beta[j], gamma=gamma[k]), col=color[color_counter], lwd=2);
                    color_counter = color_counter + 1;
                    legend_name = c(legend_name, paste("alpha = ", alpha[i], " / beta = ", beta[j], " / gamma = ", gamma[k], sep=""))
                }
                legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
            }
        }
        else if (func == "hloglogistic")  # 위험함수
        {
            for (j in 1:len_beta)   ### 파라메터: beta
            {
                color_counter_init = color_counter
                legend_name = NULL;
                plot(x, hloglogistic(x, alpha=alpha[1], beta=beta[1], gamma=gamma[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Hazard Function")
                for (k in 1:len_gamma)   ### 파라메터: gamma
                {
                    lines(x, hloglogistic(x, alpha=alpha[i], beta=beta[j], gamma=gamma[k]), col=color[color_counter], lwd=2);
                    color_counter = color_counter + 1;
                    legend_name = c(legend_name, paste("alpha = ", alpha[i], " / beta = ", beta[j], " / gamma = ", gamma[k], sep=""))
                }
                legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
            }
        }
    }
}

par(mfrow = c(8, 8))
plot.loglogistic_seq(x, alpha, beta, gamma, xlim=c(min(x), max(x)), ylim=c(-10, 10), func="dloglogistic")

par(mfrow = c(8, 8))
plot.loglogistic_seq(x, alpha, beta, gamma, xlim=c(min(x), max(x)), ylim=c(-5, 5), func="ploglogistic")

par(mfrow = c(8, 8))
plot.loglogistic_seq(x, alpha, beta, gamma, xlim=c(min(x), max(x)), ylim=c(-5, 5), func="sloglogistic")

par(mfrow = c(8, 8))
plot.loglogistic_seq(x, alpha, beta, gamma, xlim=c(min(x), max(x)), ylim=c(-30, 30), func="hloglogistic")