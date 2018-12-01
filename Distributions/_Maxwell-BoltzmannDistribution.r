# https://rdrr.io/cran/shotGroups/man/maxwell.html


setwd("/home/lv999/Dropbox/Github/SurvivalAnalysis/RCode")
source("colorPalette.R")
require(pracma)


##### maxwellboltzmann Distribution
### parameter
alpha = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
beta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x = seq(-10, 10, length.out = 1000)


### 수명 분포
dmaxwellboltzmann = function(x, alpha = 1, beta = 1)
{
    fx = (1/beta) * sqrt(2/pi) * (((x - alpha)/beta)^2) * exp(-(1/2)*(((x - alpha)/beta)^2))
    return(fx)
}


### 누적분포함수
pmaxwellboltzmann = function(x, alpha = 1, beta = 1)
{
    fx = -(smaxwellboltzmann(x, alpha, beta) - 1)
    return(fx)
}


### 생존함수
smaxwellboltzmann = function (x, alpha = 1, beta = 0) 
{
    #fx = 1 - (2/pi) * atan(exp((x - alpha)/beta))
    return(fx)
}


### 위험함수
hmaxwellboltzmann = function (x, alpha = 1, beta = 0)
{
    fx = dmaxwellboltzmann(x, alpha, beta) / smaxwellboltzmann(x, alpha, beta)
    return(fx)
}





##### Plot
plot.maxwellboltzmann_seq = function(x, alpha = 1, beta = 0, xlim=c(0, 10), ylim=c(0, 5), func="dmaxwellboltzmann")
{
    color=colorPalette(300)

    len_alpha = length(alpha)       # alpha 파라메터의 길이
    len_beta = length(beta)          # beta 파라메터의 길이
    
    color_counter = 1
    for (i in 1:len_alpha)  ### 파라메터: alpha
    {
        color_counter_init = color_counter
        legend_name = NULL;
        
        if (func=="dmaxwellboltzmann")     # 수명분포
        {
            plot(x, dmaxwellboltzmann(x, alpha=alpha[1], beta=beta[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Life Distribution Function")
            for (j in 1:len_beta)   ### 파라메터: beta
            {
                lines(x, dmaxwellboltzmann(x, alpha=alpha[i], beta=beta[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("alpha = ", alpha[i], " / beta = ", beta[j], sep=""))
            }
        }
        else if (func == "pmaxwellboltzmann")  # 누적분포함수
        {
            plot(x, pmaxwellboltzmann(x, alpha=alpha[1], beta=beta[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Cumulative Distribution Function")
            for (j in 1:len_beta)   ### 파라메터: beta
            {
                lines(x, pmaxwellboltzmann(x, alpha=alpha[i], beta=beta[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("alpha = ", alpha[i], " / beta = ", beta[j], sep=""))
            }
        }
        else if (func == "smaxwellboltzmann")  # 생존함수
        {
            plot(x, smaxwellboltzmann(x, alpha=alpha[1], beta=beta[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Survival Function")
            for (j in 1:len_beta)   ### 파라메터: beta
            {
                lines(x, smaxwellboltzmann(x, alpha=alpha[i], beta=beta[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("alpha = ", alpha[i], " / beta = ", beta[j], sep=""))
            }
        }
        else if (func == "hmaxwellboltzmann")  # 위험함수
        {
            plot(x, hmaxwellboltzmann(x, alpha=alpha[1], beta=beta[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Hazard Function")
            for (j in 1:len_beta)   ### 파라메터: beta
            {
                lines(x, hmaxwellboltzmann(x, alpha=alpha[i], beta=beta[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("alpha = ", alpha[i], " / beta = ", beta[j], sep=""))
            }
        }
        legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
    }
}

par(mfrow = c(3, 3))
plot.maxwellboltzmann_seq(x, alpha, beta, xlim=c(min(x), max(x)), ylim=c(0, 5), func="dmaxwellboltzmann")

par(mfrow = c(3, 3))
plot.maxwellboltzmann_seq(x, alpha, beta, xlim=c(min(x), max(x)), ylim=c(0, 1), func="pmaxwellboltzmann")

par(mfrow = c(3, 3))
plot.maxwellboltzmann_seq(x, alpha, beta, xlim=c(min(x), max(x)), ylim=c(0, 1), func="smaxwellboltzmann")

par(mfrow = c(3, 3))
plot.maxwellboltzmann_seq(x, alpha, beta, xlim=c(min(x), max(x)), ylim=c(0, 10), func="hmaxwellboltzmann")
