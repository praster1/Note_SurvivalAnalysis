setwd("/home/lv999/Dropbox/Github/SurvivalAnalysis/RCode")
source("colorPalette.R")


##### Arcsine Distribution
### parameter
alpha = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
beta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x = seq(-1, 10, length.out = 1000)


### 수명 분포
darcsine = function(x, alpha = 1, beta = 0)
{
    fx = dbeta((x-alpha)/(beta-alpha), 0.5, 0.5)
    return(fx)
}


### 분위수 함수
qarcsine = function(x, alpha = 1, beta = 0)
{
    fx = alpha + (beta-alpha) * qbeta(x, 0.5, 0.5)
    return(fx)
}


### 난수 함수
rarcsine = function(x, alpha = 1, beta = 0)
{
    fx = alpha + (beta-alpha) * rbeta(x, 0.5, 0.5)
    return(fx)
}


### 누적분포함수
parcsine = function(x, alpha = 1, beta = 0)
{
    fx = pbeta((x-alpha)/(beta-alpha), 0.5, 0.5)
    return(fx)
}


### 생존함수
sarcsine = function (x, alpha = 1, beta = 0) 
{
    fx = 1 - parcsine(x, alpha, beta)
    return(fx)
}


### 위험함수
harcsine = function (x, alpha = 1, beta = 0)
{
    fx = darcsine(x, alpha, beta) / sarcsine(x, alpha, beta)
    return(fx)
}





##### Plot
plot.arcsine_seq = function(x, alpha = 1, beta = 0, xlim=c(0, 10), ylim=c(0, 5), func="darcsine")
{
    color=colorPalette(300)

    len_alpha = length(alpha)       # alpha 파라메터의 길이
    len_beta = length(beta)          # beta 파라메터의 길이
    
    color_counter = 1
    for (i in 1:len_alpha)  ### 파라메터: alpha
    {
        color_counter_init = color_counter
        legend_name = NULL;
        
        if (func=="darcsine")     # 수명분포
        {
            plot(x, darcsine(x, alpha=alpha[1], beta=beta[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Life Distribution Function")
            for (j in 1:len_beta)   ### 파라메터: beta
            {
                lines(x, darcsine(x, alpha=alpha[i], beta=beta[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("alpha = ", alpha[i], " / beta = ", beta[j], sep=""))
            }
        }
        else if (func == "parcsine")  # 누적분포함수
        {
            plot(x, parcsine(x, alpha=alpha[1], beta=beta[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Cumulative Distribution Function")
            for (j in 1:len_beta)   ### 파라메터: beta
            {
                lines(x, parcsine(x, alpha=alpha[i], beta=beta[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("alpha = ", alpha[i], " / beta = ", beta[j], sep=""))
            }
        }
        else if (func == "sarcsine")  # 생존함수
        {
            plot(x, sarcsine(x, alpha=alpha[1], beta=beta[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Survival Function")
            for (j in 1:len_beta)   ### 파라메터: beta
            {
                lines(x, sarcsine(x, alpha=alpha[i], beta=beta[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("alpha = ", alpha[i], " / beta = ", beta[j], sep=""))
            }
        }
        else if (func == "harcsine")  # 위험함수
        {
            plot(x, harcsine(x, alpha=alpha[1], beta=beta[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Hazard Function")
            for (j in 1:len_beta)   ### 파라메터: beta
            {
                lines(x, harcsine(x, alpha=alpha[i], beta=beta[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("alpha = ", alpha[i], " / beta = ", beta[j], sep=""))
            }
        }
        legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
    }
}

par(mfrow = c(3, 3))
plot.arcsine_seq(x, alpha, beta, xlim=c(min(x), max(x)), ylim=c(0, 5), func="darcsine")

par(mfrow = c(3, 3))
plot.arcsine_seq(x, alpha, beta, xlim=c(min(x), max(x)), ylim=c(0, 1), func="parcsine")

par(mfrow = c(3, 3))
plot.arcsine_seq(x, alpha, beta, xlim=c(min(x), max(x)), ylim=c(0, 1), func="sarcsine")

par(mfrow = c(3, 3))
plot.arcsine_seq(x, alpha, beta, xlim=c(min(x), max(x)), ylim=c(0, 10), func="harcsine")
