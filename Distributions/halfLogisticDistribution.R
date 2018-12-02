setwd("/home/lv999/Dropbox/Github/SurvivalAnalysis/Distributions")
source("colorPalette.R")


##### Half-logistic Distribution
### parameter
alpha = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
beta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)		# s (beta)

### input varialbe
x = seq(0.1, 10, length.out = 1000)


### 수명 분포
dhalflogistic = function(x, alpha = 0, beta = 1)
{
    fx = (2 * exp((x-alpha)/beta)) / (beta * (1 + exp((x-alpha)/beta))^2)
    return(fx)
}


### 난수 함수
rhalflogistic = function (n, min=0.0001, max=1, alpha = 1, beta = 1) 
{
    normalization = function(x)	{	(x-min(x))/(max(x)-min(x));	}

	xseq = seq(min, max, length=1000000)
	res = sample(xseq, size=n, prob=normalization(dhalflogistic(xseq, alpha = alpha, beta = beta)), replace=TRUE)
	return(res)
}

### 누적분포함수
phalflogistic = function(x, alpha = 0, beta = 1)
{
    Fx = -(shalflogistic(x, alpha = alpha, beta = beta) - 1)
    return(Fx)
}


### 생존함수
shalflogistic = function(x, alpha = 0, beta = 1)
{
    fx = 2 * (1 + exp((x-alpha)/beta))^(-1)
    return(fx)
}


### 위험함수
hhalflogistic = function(x, alpha = 0, beta = 1)
{
    fx = dhalflogistic(x, alpha = alpha, beta = beta) / shalflogistic(x, alpha = alpha, beta = beta)
    return(fx)
}





##### Plot
plot.halflogistic_seq = function(x, alpha = 0, beta = 1, xlim=c(0, 10), ylim=c(0, 5), func="dhalflogistic")
{
    color=colorPalette(300)

    len_alpha = length(alpha)       # m 파라메터의 길이
    len_beta = length(beta)          # s 파라메터의 길이
    
    color_counter = 1
    for (i in 1:len_alpha)  ### 파라메터: m
    {
        color_counter_init = color_counter
        legend_name = NULL;
        
        if (func=="dhalflogistic")     # 수명분포
        {
            plot(x, dhalflogistic(x, alpha=alpha[1], beta=beta[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Life Distribution Function")
            for (j in 1:len_beta)   ### 파라메터: s
            {
                lines(x, dhalflogistic(x, alpha=alpha[i], beta=beta[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("alpha = ", m[i], " / beta = ", s[j], sep=""))
            }
        }
        else if (func == "phalflogistic")  # 누적분포함수
        {
            plot(x, phalflogistic(x, alpha=alpha[1], beta=beta[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Cumulative Distribution Function")
            for (j in 1:len_beta)   ### 파라메터: s
            {
                lines(x, phalflogistic(x, alpha=alpha[i], beta=beta[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("alpha = ", m[i], " / beta = ", s[j], sep=""))
            }
        }
        else if (func == "shalflogistic")  # 생존함수
        {
            plot(x, shalflogistic(x, alpha=alpha[1], beta=beta[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Survival Function")
            for (j in 1:len_beta)   ### 파라메터: s
            {
                lines(x, shalflogistic(x, alpha=alpha[i], beta=beta[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("alpha = ", m[i], " / beta = ", s[j], sep=""))
            }
        }
        else if (func == "hhalflogistic")  # 위험함수
        {
            plot(x, hhalflogistic(x, alpha=alpha[1], beta=beta[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Hazard Function")
            for (j in 1:len_beta)   ### 파라메터: s
            {
                lines(x, hhalflogistic(x, alpha=alpha[i], beta=beta[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("alpha = ", m[i], " / beta = ", s[j], sep=""))
            }
        }
        legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
    }
}

par(mfrow = c(3, 3))
plot.halflogistic_seq(x, alpha, beta, xlim=c(min(x), max(x)), ylim=c(0, 1), func="dhalflogistic")

par(mfrow = c(3, 3))
plot.halflogistic_seq(x, alpha, beta, xlim=c(min(x), max(x)), ylim=c(0, 1), func="phalflogistic")

par(mfrow = c(3, 3))
plot.halflogistic_seq(x, alpha, beta, xlim=c(min(x), max(x)), ylim=c(0, 1), func="shalflogistic")

par(mfrow = c(3, 3))
plot.halflogistic_seq(x, alpha, beta, xlim=c(min(x), max(x)), ylim=c(0, 5), func="hhalflogistic")

