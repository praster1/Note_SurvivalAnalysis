# https://www.rdocumentation.org/packages/VGAM/versions/0.7-7/topics/Lomax

setwd("/home/lv999/Dropbox/Github/SurvivalAnalysis/RCode")
source("colorPalette.R")


##### lomax Distribution
### parameter
alpha = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
beta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)
gamma = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x <- seq(0, 1, length.out = 1000)


### 수명 분포
dlomax <- function(x, alpha = 1, beta = 1, gamma = 1)
{
    fx = (gamma / beta) * (1 + ((x - alpha)/beta))^(-(gamma+1))
    return(fx)
}


### 누적분포함수
plomax = function(x, alpha = 1, beta = 1, gamma = 1)
{
    fx = -(slomax(x, alpha, beta) - 1)
    return(fx)
}


### 생존함수
slomax = function (x, alpha = 1, beta = 1, gamma = 1)
{
    fx <- (1 + ((x - alpha)/beta))^(-gamma)
    return(fx)
}


### 위험함수
hlomax = function (x, alpha = 1, beta = 1, gamma = 1)
{
    fx <- dlomax(x, alpha, beta, gamma) / slomax(x, alpha, beta, gamma)
    return(fx)
}





##### Plot
plot.lomax_seq = function(x, alpha = 1, beta = 1, gamma = 1, xlim=c(0, 10), ylim=c(0, 5), func="dlomax")
{
    color=colorPalette(300)

    len_alpha = length(alpha)       # alpha 파라메터의 길이
    len_beta = length(beta)          # beta 파라메터의 길이
    len_gamma = length(gamma)  # gamma 파라메터의 길이
    
    color_counter = 1
    for (i in 1:len_alpha)  ### 파라메터: alpha
    {
        if (func=="dlomax")     # 수명분포
        {
            for (j in 1:len_beta)   ### 파라메터: beta
            {
                color_counter_init = color_counter
                legend_name = NULL;
                plot(x, dlomax(x, alpha=alpha[1], beta=beta[1], gamma=gamma[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Life Distribution Function")
                for (k in 1:len_gamma)   ### 파라메터: gamma
                {
                    lines(x, dlomax(x, alpha=alpha[i], beta=beta[j], gamma=gamma[k]), col=color[color_counter], lwd=2);
                    color_counter = color_counter + 1;
                    legend_name = c(legend_name, paste("alpha = ", alpha[i], " / beta = ", beta[j], " / gamma = ", gamma[k], sep=""))
                }
                legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
            }
        }
        else if (func == "plomax")  # 누적분포함수
        {
            for (j in 1:len_beta)   ### 파라메터: beta
            {
                color_counter_init = color_counter
                legend_name = NULL;
                plot(x, plomax(x, alpha=alpha[1], beta=beta[1], gamma=gamma[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Cumulative Distribution Function")
                for (k in 1:len_gamma)   ### 파라메터: gamma
                {
                    lines(x, plomax(x, alpha=alpha[i], beta=beta[j], gamma=gamma[k]), col=color[color_counter], lwd=2);
                    color_counter = color_counter + 1;
                    legend_name = c(legend_name, paste("alpha = ", alpha[i], " / beta = ", beta[j], " / gamma = ", gamma[k], sep=""))
                }
                legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
            }
        }
        else if (func == "slomax")  # 생존함수
        {
            for (j in 1:len_beta)   ### 파라메터: beta
            {
                color_counter_init = color_counter
                legend_name = NULL;
                plot(x, slomax(x, alpha=alpha[1], beta=beta[1], gamma=gamma[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Survival Function")
                for (k in 1:len_gamma)   ### 파라메터: gamma
                {
                    lines(x, slomax(x, alpha=alpha[i], beta=beta[j], gamma=gamma[k]), col=color[color_counter], lwd=2);
                    color_counter = color_counter + 1;
                    legend_name = c(legend_name, paste("alpha = ", alpha[i], " / beta = ", beta[j], " / gamma = ", gamma[k], sep=""))
                }
                legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
            }
        }
        else if (func == "hlomax")  # 위험함수
        {
            for (j in 1:len_beta)   ### 파라메터: beta
            {
                color_counter_init = color_counter
                legend_name = NULL;
                plot(x, hlomax(x, alpha=alpha[1], beta=beta[1], gamma=gamma[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Hazard Function")
                for (k in 1:len_gamma)   ### 파라메터: gamma
                {
                    lines(x, hlomax(x, alpha=alpha[i], beta=beta[j], gamma=gamma[k]), col=color[color_counter], lwd=2);
                    color_counter = color_counter + 1;
                    legend_name = c(legend_name, paste("alpha = ", alpha[i], " / beta = ", beta[j], " / gamma = ", gamma[k], sep=""))
                }
                legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
            }
        }
    }
}

par(mfrow = c(8, 8))
plot.lomax_seq(x, alpha, beta, gamma, xlim=c(min(x), max(x)), ylim=c(-10, 10), func="dlomax")

par(mfrow = c(8, 8))
plot.lomax_seq(x, alpha, beta, gamma, xlim=c(min(x), max(x)), ylim=c(-5, 5), func="plomax")

par(mfrow = c(8, 8))
plot.lomax_seq(x, alpha, beta, gamma, xlim=c(min(x), max(x)), ylim=c(-5, 5), func="slomax")

par(mfrow = c(8, 8))
plot.lomax_seq(x, alpha, beta, gamma, xlim=c(min(x), max(x)), ylim=c(-30, 30), func="hlomax")