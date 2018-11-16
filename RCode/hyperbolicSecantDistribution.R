# https://www.rdocumentation.org/packages/VaRES/versions/1.0/topics/secant

setwd("/home/lv999/Dropbox/Github/SurvivalAnalysis/RCode")
source("colorPalette.R")
require(pracma)


##### hyperbolicsecant Distribution
### parameter
alpha = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
beta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x <- seq(-10, 10, length.out = 1000)


### 수명 분포
dhyperbolicsecant = function(x, alpha = 1, beta = 1)
{
    fx = (1 / (beta * pi)) * sech((x - alpha)/beta)
    return(fx)
}


### 누적분포함수
phyperbolicsecant = function(x, alpha = 1, beta = 1)
{
    fx = -(shyperbolicsecant(x, alpha, beta) - 1)
    return(fx)
}


### 생존함수
shyperbolicsecant = function (x, alpha = 1, beta = 0) 
{
    fx <- 1 - (2/pi) * atan(exp((x - alpha)/beta))
    return(fx)
}


### 위험함수
hhyperbolicsecant = function (x, alpha = 1, beta = 0)
{
    fx <- dhyperbolicsecant(x, alpha, beta) / shyperbolicsecant(x, alpha, beta)
    return(fx)
}





##### Plot
plot.hyperbolicsecant_seq = function(x, alpha = 1, beta = 0, xlim=c(0, 10), ylim=c(0, 5), func="dhyperbolicsecant")
{
    color=colorPalette(300)

    len_alpha = length(alpha)       # alpha 파라메터의 길이
    len_beta = length(beta)          # beta 파라메터의 길이
    
    color_counter = 1
    for (i in 1:len_alpha)  ### 파라메터: alpha
    {
        color_counter_init = color_counter
        legend_name = NULL;
        
        if (func=="dhyperbolicsecant")     # 수명분포
        {
            plot(x, dhyperbolicsecant(x, alpha=alpha[1], beta=beta[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Life Distribution Function")
            for (j in 1:len_beta)   ### 파라메터: beta
            {
                lines(x, dhyperbolicsecant(x, alpha=alpha[i], beta=beta[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("alpha = ", alpha[i], " / beta = ", beta[j], sep=""))
            }
        }
        else if (func == "phyperbolicsecant")  # 누적분포함수
        {
            plot(x, phyperbolicsecant(x, alpha=alpha[1], beta=beta[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Cumulative Distribution Function")
            for (j in 1:len_beta)   ### 파라메터: beta
            {
                lines(x, phyperbolicsecant(x, alpha=alpha[i], beta=beta[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("alpha = ", alpha[i], " / beta = ", beta[j], sep=""))
            }
        }
        else if (func == "shyperbolicsecant")  # 생존함수
        {
            plot(x, shyperbolicsecant(x, alpha=alpha[1], beta=beta[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Survival Function")
            for (j in 1:len_beta)   ### 파라메터: beta
            {
                lines(x, shyperbolicsecant(x, alpha=alpha[i], beta=beta[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("alpha = ", alpha[i], " / beta = ", beta[j], sep=""))
            }
        }
        else if (func == "hhyperbolicsecant")  # 위험함수
        {
            plot(x, hhyperbolicsecant(x, alpha=alpha[1], beta=beta[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Hazard Function")
            for (j in 1:len_beta)   ### 파라메터: beta
            {
                lines(x, hhyperbolicsecant(x, alpha=alpha[i], beta=beta[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("alpha = ", alpha[i], " / beta = ", beta[j], sep=""))
            }
        }
        legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
    }
}

par(mfrow = c(3, 3))
plot.hyperbolicsecant_seq(x, alpha, beta, xlim=c(min(x), max(x)), ylim=c(0, 5), func="dhyperbolicsecant")

par(mfrow = c(3, 3))
plot.hyperbolicsecant_seq(x, alpha, beta, xlim=c(min(x), max(x)), ylim=c(0, 1), func="phyperbolicsecant")

par(mfrow = c(3, 3))
plot.hyperbolicsecant_seq(x, alpha, beta, xlim=c(min(x), max(x)), ylim=c(0, 1), func="shyperbolicsecant")

par(mfrow = c(3, 3))
plot.hyperbolicsecant_seq(x, alpha, beta, xlim=c(min(x), max(x)), ylim=c(0, 10), func="hhyperbolicsecant")
