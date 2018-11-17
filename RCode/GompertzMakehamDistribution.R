setwd("/home/lv999/Dropbox/Github/SurvivalAnalysis/RCode")
source("colorPalette.R")


##### Gompertz-Makeham Distribution
### parameter
alpha = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
beta = c(0, 0.25, 0.5, 0.75, 1, 2, 4, 8)
gamma = c(0, 0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x = seq(0, 1, length.out = 1000)


### 수명 분포
dgompertzmakeham = function(x, alpha = 1, beta = 1, gamma = 1)
{
    fx = (gamma * ((alpha / beta) * (1 - exp(beta * x)) - gamma * x)) + (alpha * exp(beta * x) * exp((alpha/beta) * (1-exp(beta * x)) - gamma * x))
    return(fx)
}


### 누적분포함수
pgompertzmakeham = function(x, alpha = 1, beta = 1, gamma = 1)
{
    fx = -(sgompertzmakeham(x, alpha, beta, gamma) - 1)
    return(fx)
}


### 생존함수
sgompertzmakeham = function (x, alpha = 1, beta = 1, gamma = 1)
{
    fx = exp((alpha/beta) * (1-exp(beta * x)) - gamma * x)
    return(fx)
}


### 위험함수
hgompertzmakeham = function (x, alpha = 1, beta = 1, gamma = 1)
{
    fx = dgompertzmakeham(x, alpha, beta, gamma) / sgompertzmakeham(x, alpha, beta, gamma)
    return(fx)
}





##### Plot
plot.gompertzmakeham_seq = function(x, alpha = 1, beta = 1, gamma = 1, xlim=c(0, 10), ylim=c(0, 5), func="dgompertzmakeham")
{
    color=colorPalette(300)

    len_alpha = length(alpha)       # alpha 파라메터의 길이
    len_beta = length(beta)          # beta 파라메터의 길이
    len_gamma = length(gamma)  # gamma 파라메터의 길이
    
    color_counter = 1
    for (i in 1:len_alpha)  ### 파라메터: alpha
    {
        if (func=="dgompertzmakeham")     # 수명분포
        {
            for (j in 1:len_beta)   ### 파라메터: beta
            {
                color_counter_init = color_counter
                legend_name = NULL;
                plot(x, dgompertzmakeham(x, alpha=alpha[1], beta=beta[1], gamma=gamma[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Life Distribution Function")
                for (k in 1:len_gamma)   ### 파라메터: gamma
                {
                    lines(x, dgompertzmakeham(x, alpha=alpha[i], beta=beta[j], gamma=gamma[k]), col=color[color_counter], lwd=2);
                    color_counter = color_counter + 1;
                    legend_name = c(legend_name, paste("alpha = ", alpha[i], " / beta = ", beta[j], " / gamma = ", gamma[k], sep=""))
                }
                legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
            }
        }
        else if (func == "pgompertzmakeham")  # 누적분포함수
        {
            for (j in 1:len_beta)   ### 파라메터: beta
            {
                color_counter_init = color_counter
                legend_name = NULL;
                plot(x, pgompertzmakeham(x, alpha=alpha[1], beta=beta[1], gamma=gamma[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Cumulative Distribution Function")
                for (k in 1:len_gamma)   ### 파라메터: gamma
                {
                    lines(x, pgompertzmakeham(x, alpha=alpha[i], beta=beta[j], gamma=gamma[k]), col=color[color_counter], lwd=2);
                    color_counter = color_counter + 1;
                    legend_name = c(legend_name, paste("alpha = ", alpha[i], " / beta = ", beta[j], " / gamma = ", gamma[k], sep=""))
                }
                legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
            }
        }
        else if (func == "sgompertzmakeham")  # 생존함수
        {
            for (j in 1:len_beta)   ### 파라메터: beta
            {
                color_counter_init = color_counter
                legend_name = NULL;
                plot(x, sgompertzmakeham(x, alpha=alpha[1], beta=beta[1], gamma=gamma[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Survival Function")
                for (k in 1:len_gamma)   ### 파라메터: gamma
                {
                    lines(x, sgompertzmakeham(x, alpha=alpha[i], beta=beta[j], gamma=gamma[k]), col=color[color_counter], lwd=2);
                    color_counter = color_counter + 1;
                    legend_name = c(legend_name, paste("alpha = ", alpha[i], " / beta = ", beta[j], " / gamma = ", gamma[k], sep=""))
                }
                legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
            }
        }
        else if (func == "hgompertzmakeham")  # 위험함수
        {
            for (j in 1:len_beta)   ### 파라메터: beta
            {
                color_counter_init = color_counter
                legend_name = NULL;
                plot(x, hgompertzmakeham(x, alpha=alpha[1], beta=beta[1], gamma=gamma[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Hazard Function")
                for (k in 1:len_gamma)   ### 파라메터: gamma
                {
                    lines(x, hgompertzmakeham(x, alpha=alpha[i], beta=beta[j], gamma=gamma[k]), col=color[color_counter], lwd=2);
                    color_counter = color_counter + 1;
                    legend_name = c(legend_name, paste("alpha = ", alpha[i], " / beta = ", beta[j], " / gamma = ", gamma[k], sep=""))
                }
                legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
            }
        }
    }
}

par(mfrow = c(9, 8))
plot.gompertzmakeham_seq(x, alpha, beta, gamma, xlim=c(min(x), max(x)), ylim=c(-10, 10), func="dgompertzmakeham")

par(mfrow = c(9, 8))
plot.gompertzmakeham_seq(x, alpha, beta, gamma, xlim=c(min(x), max(x)), ylim=c(-5, 5), func="pgompertzmakeham")

par(mfrow = c(9, 8))
plot.gompertzmakeham_seq(x, alpha, beta, gamma, xlim=c(min(x), max(x)), ylim=c(-5, 5), func="sgompertzmakeham")

par(mfrow = c(9, 8))
plot.gompertzmakeham_seq(x, alpha, beta, gamma, xlim=c(min(x), max(x)), ylim=c(-30, 30), func="hgompertzmakeham")