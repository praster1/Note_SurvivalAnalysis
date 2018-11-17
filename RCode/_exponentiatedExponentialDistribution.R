##### Not Completed 

setwd("/home/lv999/Dropbox/Github/SurvivalAnalysis/RCode")
source("colorPalette.R")


##### exponentiatedExponential Distribution
### parameter
alpha = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
beta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)
gamma = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x = seq(0, 10, length.out = 1000)


### 수명 분포
dexponentiatedExponential = function(x, alpha = 1, beta = 1, gamma = 1)
{
    fx = (gamma/beta) * exp(-((x - alpha) / beta)) * (1 - exp(((x - alpha) / beta)))^(gamma-1)
    return(fx)
}


### 누적분포함수
pexponentiatedExponential = function(x, alpha = 1, beta = 1, gamma = 1)
{
    fx = -(sexponentiatedExponential(x, alpha, beta) - 1)
    return(fx)
}


### 생존함수
sexponentiatedExponential = function (x, alpha = 1, beta = 1, gamma = 1)
{
    fx = 1 - (1 - exp(((x - alpha) / beta)))^gamma
    return(fx)
}


### 위험함수
hexponentiatedExponential = function (x, alpha = 1, beta = 1, gamma = 1)
{
    fx = dexponentiatedExponential(x, alpha, beta, gamma) / sexponentiatedExponential(x, alpha, beta, gamma)
    return(fx)
}





##### Plot
plot.exponentiatedExponential_seq = function(x, alpha = 1, beta = 1, gamma = 1, xlim=c(0, 10), ylim=c(0, 5), func="dexponentiatedExponential")
{
    color=colorPalette(300)

    len_alpha = length(alpha)       # alpha 파라메터의 길이
    len_beta = length(beta)          # beta 파라메터의 길이
    len_gamma = length(gamma)  # gamma 파라메터의 길이
    
    color_counter = 1
    for (i in 1:len_alpha)  ### 파라메터: alpha
    {
        if (func=="dexponentiatedExponential")     # 수명분포
        {
            for (j in 1:len_beta)   ### 파라메터: beta
            {
                color_counter_init = color_counter
                legend_name = NULL;
                plot(x, dexponentiatedExponential(x, alpha=alpha[1], beta=beta[1], gamma=gamma[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Life Distribution Function")
                for (k in 1:len_gamma)   ### 파라메터: gamma
                {
                    lines(x, dexponentiatedExponential(x, alpha=alpha[i], beta=beta[j], gamma=gamma[k]), col=color[color_counter], lwd=2);
                    color_counter = color_counter + 1;
                    legend_name = c(legend_name, paste("alpha = ", alpha[i], " / beta = ", beta[j], " / gamma = ", gamma[k], sep=""))
                }
                legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
            }
        }
        else if (func == "pexponentiatedExponential")  # 누적분포함수
        {
            for (j in 1:len_beta)   ### 파라메터: beta
            {
                color_counter_init = color_counter
                legend_name = NULL;
                plot(x, pexponentiatedExponential(x, alpha=alpha[1], beta=beta[1], gamma=gamma[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Cumulative Distribution Function")
                for (k in 1:len_gamma)   ### 파라메터: gamma
                {
                    lines(x, pexponentiatedExponential(x, alpha=alpha[i], beta=beta[j], gamma=gamma[k]), col=color[color_counter], lwd=2);
                    color_counter = color_counter + 1;
                    legend_name = c(legend_name, paste("alpha = ", alpha[i], " / beta = ", beta[j], " / gamma = ", gamma[k], sep=""))
                }
                legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
            }
        }
        else if (func == "sexponentiatedExponential")  # 생존함수
        {
            for (j in 1:len_beta)   ### 파라메터: beta
            {
                color_counter_init = color_counter
                legend_name = NULL;
                plot(x, sexponentiatedExponential(x, alpha=alpha[1], beta=beta[1], gamma=gamma[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Survival Function")
                for (k in 1:len_gamma)   ### 파라메터: gamma
                {
                    lines(x, sexponentiatedExponential(x, alpha=alpha[i], beta=beta[j], gamma=gamma[k]), col=color[color_counter], lwd=2);
                    color_counter = color_counter + 1;
                    legend_name = c(legend_name, paste("alpha = ", alpha[i], " / beta = ", beta[j], " / gamma = ", gamma[k], sep=""))
                }
                legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
            }
        }
        else if (func == "hexponentiatedExponential")  # 위험함수
        {
            for (j in 1:len_beta)   ### 파라메터: beta
            {
                color_counter_init = color_counter
                legend_name = NULL;
                plot(x, hexponentiatedExponential(x, alpha=alpha[1], beta=beta[1], gamma=gamma[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Hazard Function")
                for (k in 1:len_gamma)   ### 파라메터: gamma
                {
                    lines(x, hexponentiatedExponential(x, alpha=alpha[i], beta=beta[j], gamma=gamma[k]), col=color[color_counter], lwd=2);
                    color_counter = color_counter + 1;
                    legend_name = c(legend_name, paste("alpha = ", alpha[i], " / beta = ", beta[j], " / gamma = ", gamma[k], sep=""))
                }
                legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
            }
        }
    }
}

par(mfrow = c(9, 8))
plot.exponentiatedExponential_seq(x, alpha, beta, gamma, xlim=c(min(x), max(x)), ylim=c(-10, 10), func="dexponentiatedExponential")

par(mfrow = c(9, 8))
plot.exponentiatedExponential_seq(x, alpha, beta, gamma, xlim=c(min(x), max(x)), ylim=c(-10, 10), func="pexponentiatedExponential")

par(mfrow = c(9, 8))
plot.exponentiatedExponential_seq(x, alpha, beta, gamma, xlim=c(min(x), max(x)), ylim=c(-5, 5), func="sexponentiatedExponential")

par(mfrow = c(9, 8))
plot.exponentiatedExponential_seq(x, alpha, beta, gamma, xlim=c(min(x), max(x)), ylim=c(-30, 30), func="hexponentiatedExponential")