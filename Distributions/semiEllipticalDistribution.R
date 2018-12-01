setwd("/home/lv999/Dropbox/Github/SurvivalAnalysis/RCode")
source("colorPalette.R")
require(pracma)


##### semielliptical Distribution
### parameter
alpha = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
beta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x = seq(-10, 10, length.out = 1000)


### 수명 분포
dsemielliptical = function(x, alpha = 0, beta = 1)
{
    fx = (2/(beta * pi)) * sqrt(1 - ((x-alpha)/beta)^2)
    return(fx)
}


### 누적분포함수
psemielliptical = function(x, alpha = 0, beta = 1)
{
    fx = -(ssemielliptical(x, alpha = alpha, beta = beta) - 1)
    return(fx)
}


### 생존함수
ssemielliptical = function (x, alpha = 0, beta = 1) 
{
	temp = (x - alpha)/beta
    fx = (1/2) * (1/pi) * (temp * sqrt(1-temp^2) + asin(temp))
    return(fx)
}


### 위험함수
hsemielliptical = function (x, alpha = 0, beta = 1)
{
    fx = dsemielliptical(x, alpha, beta) / ssemielliptical(x, alpha, beta)
    return(fx)
}





##### Plot
plot.semielliptical_seq = function(x, alpha = 0, beta = 1, xlim=c(0, 10), ylim=c(0, 5), func="dsemielliptical")
{
    color=colorPalette(300)

    len_alpha = length(alpha)       # alpha 파라메터의 길이
    len_beta = length(beta)          # beta 파라메터의 길이
    
    color_counter = 1
    for (i in 1:len_alpha)  ### 파라메터: alpha
    {
        color_counter_init = color_counter
        legend_name = NULL;
        
        if (func=="dsemielliptical")     # 수명분포
        {
            plot(x, dsemielliptical(x, alpha=alpha[1], beta=beta[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Life Distribution Function")
            for (j in 1:len_beta)   ### 파라메터: beta
            {
                lines(x, dsemielliptical(x, alpha=alpha[i], beta=beta[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("alpha = ", alpha[i], " / beta = ", beta[j], sep=""))
            }
        }
        else if (func == "psemielliptical")  # 누적분포함수
        {
            plot(x, psemielliptical(x, alpha=alpha[1], beta=beta[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Cumulative Distribution Function")
            for (j in 1:len_beta)   ### 파라메터: beta
            {
                lines(x, psemielliptical(x, alpha=alpha[i], beta=beta[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("alpha = ", alpha[i], " / beta = ", beta[j], sep=""))
            }
        }
        else if (func == "ssemielliptical")  # 생존함수
        {
            plot(x, ssemielliptical(x, alpha=alpha[1], beta=beta[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Survival Function")
            for (j in 1:len_beta)   ### 파라메터: beta
            {
                lines(x, ssemielliptical(x, alpha=alpha[i], beta=beta[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("alpha = ", alpha[i], " / beta = ", beta[j], sep=""))
            }
        }
        else if (func == "hsemielliptical")  # 위험함수
        {
            plot(x, hsemielliptical(x, alpha=alpha[1], beta=beta[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Hazard Function")
            for (j in 1:len_beta)   ### 파라메터: beta
            {
                lines(x, hsemielliptical(x, alpha=alpha[i], beta=beta[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("alpha = ", alpha[i], " / beta = ", beta[j], sep=""))
            }
        }
        legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
    }
}

par(mfrow = c(3, 3))
plot.semielliptical_seq(x, alpha, beta, xlim=c(min(x), max(x)), ylim=c(0, 5), func="dsemielliptical")

par(mfrow = c(3, 3))
plot.semielliptical_seq(x, alpha, beta, xlim=c(min(x), max(x)), ylim=c(0, 2), func="psemielliptical")

par(mfrow = c(3, 3))
plot.semielliptical_seq(x, alpha, beta, xlim=c(min(x), max(x)), ylim=c(-1, 1), func="ssemielliptical")

par(mfrow = c(3, 3))
plot.semielliptical_seq(x, alpha, beta, xlim=c(min(x), max(x)), ylim=c(-10, 10), func="hsemielliptical")

