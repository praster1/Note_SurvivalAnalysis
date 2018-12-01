setwd("/home/lv999/Dropbox/Github/SurvivalAnalysis/RCode")
source("colorPalette.R")


##### Reflexted Exponential Distribution
### parameter
alpha = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
beta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x = seq(0, 10, length.out = 1000)


### 수명 분포
drexponential = function(x, alpha=alpha, beta=beta)
{
    fx = (1/beta) * exp((x-alpha)/beta)
    return(fx)
}


### 누적분포함수
prexponential = function(x, alpha=alpha, beta=beta)
{
    fx = -(srexponential(x, alpha, beta) - 1)
    return(fx)
}


### 생존함수
srexponential = function (x, alpha=alpha, beta=beta)
{
    fx = 1 - exp((x-alpha)/beta)
    return(fx)
}


### 위험함수
hrexponential = function (x, alpha=alpha, beta=beta)
{
    fx = drexponential(x, alpha, beta) / srexponential(x, alpha, beta)
    return(fx)
}





##### Plot
plot.rexponential_seq = function(x, alpha=alpha, beta=beta, xlim=c(0, 10), ylim=c(0, 5), func="drexponential")
{
    color=colorPalette(300)

    len_alpha = length(alpha)       # alpha 파라메터의 길이
    len_beta = length(beta)          # beta 파라메터의 길이
    
    color_counter = 1
    for (i in 1:len_alpha)  ### 파라메터: alpha
    {
        color_counter_init = color_counter
        legend_name = NULL;
        
        if (func=="drexponential")     # 수명분포
        {
            plot(x, drexponential(x, alpha=alpha[1], beta=beta[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Life Distribution Function")
            for (j in 1:len_beta)   ### 파라메터: beta
            {
                lines(x, drexponential(x, alpha=alpha[i], beta=beta[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("alpha = ", alpha[i], " / beta = ", beta[j], sep=""))
            }
        }
        else if (func == "prexponential")  # 누적분포함수
        {
            plot(x, prexponential(x, alpha=alpha[1], beta=beta[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Cumulative Distribution Function")
            for (j in 1:len_beta)   ### 파라메터: beta
            {
                lines(x, prexponential(x, alpha=alpha[i], beta=beta[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("alpha = ", alpha[i], " / beta = ", beta[j], sep=""))
            }
        }
        else if (func == "srexponential")  # 생존함수
        {
            plot(x, srexponential(x, alpha=alpha[1], beta=beta[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Survival Function")
            for (j in 1:len_beta)   ### 파라메터: beta
            {
                lines(x, srexponential(x, alpha=alpha[i], beta=beta[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("alpha = ", alpha[i], " / beta = ", beta[j], sep=""))
            }
        }
        else if (func == "hrexponential")  # 위험함수
        {
            plot(x, hrexponential(x, alpha=alpha[1], beta=beta[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Hazard Function")
            for (j in 1:len_beta)   ### 파라메터: beta
            {
                lines(x, hrexponential(x, alpha=alpha[i], beta=beta[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("alpha = ", alpha[i], " / beta = ", beta[j], sep=""))
            }
        }
        legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
    }
}

par(mfrow = c(3, 3))
plot.rexponential_seq(x, alpha, beta, xlim=c(min(x), max(x)), ylim=c(0, 10), func="drexponential")

par(mfrow = c(3, 3))
plot.rexponential_seq(x, alpha, beta, xlim=c(min(x), max(x)), ylim=c(0, 10), func="prexponential")

par(mfrow = c(3, 3))
plot.rexponential_seq(x, alpha, beta, xlim=c(min(x), max(x)), ylim=c(-10, 10), func="srexponential")

par(mfrow = c(3, 3))
plot.rexponential_seq(x, alpha, beta, xlim=c(min(x), max(x)), ylim=c(-10, 10), func="hrexponential")