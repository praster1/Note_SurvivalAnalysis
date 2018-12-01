setwd("/home/lv999/Dropbox/Github/SurvivalAnalysis/Distributions")
source("colorPalette.R")
require(VaRES)

##### weibull Distribution with c Parameters
### parameter
mu = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
sigma = c(0.25, 0.5, 0.75, 1, 2, 4, 8)
c = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x = seq(-10, 10, length.out = 1000)


### 수명 분포
ddweibull(x, c=1, mu=0, sigma=1)


### 누적분포함수
pdweibull(x, c=1, mu=0, sigma=1)


### 생존함수
sdweibull = function(x, mu=1, sigma=1, c=0)
{
    fx = 1 - pdweibull(x, mu=mu, sigma=sigma, c=c)
    return(fx)
}


### 위험함수
hdweibull = function (x, mu=mu, sigma=sigma, c=0)
{
    fx = ddweibull(x, mu=mu, sigma=sigma, c=c) / sdweibull(x, mu=mu, sigma=sigma, c=c)
    return(fx)
}





##### Plot
plot.dweibull_seq = function(x, mu = 1, sigma = 1, c = 1, xlim=c(0, 10), ylim=c(0, 5), func="ddweibull")
{
    color=colorPalette(300)

    len_c = length(c)       # c 파라메터의 길이
    len_mu = length(mu)          # mu 파라메터의 길이
    len_sigma = length(sigma)  # sigma 파라메터의 길이
    
    color_counter = 1
    for (i in 1:len_c)  ### 파라메터: c
    {
        if (func=="ddweibull")     # 수명분포
        {
            for (j in 1:len_mu)   ### 파라메터: mu
            {
                color_counter_init = color_counter
                legend_name = NULL;
                plot(x, ddweibull(x, c=c[1], mu=mu[1], sigma=sigma[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Life Distribution Function")
                for (k in 1:len_sigma)   ### 파라메터: sigma
                {
                    lines(x, ddweibull(x, c=c[i], mu=mu[j], sigma=sigma[k]), col=color[color_counter], lwd=2);
                    color_counter = color_counter + 1;
                    legend_name = c(legend_name, paste("c = ", c[i], " / mu = ", mu[j], " / sigma = ", sigma[k], sep=""))
                }
                legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
            }
        }
        else if (func == "pdweibull")  # 누적분포함수
        {
            for (j in 1:len_mu)   ### 파라메터: mu
            {
                color_counter_init = color_counter
                legend_name = NULL;
                plot(x, pdweibull(x, c=c[1], mu=mu[1], sigma=sigma[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Cumulative Distribution Function")
                for (k in 1:len_sigma)   ### 파라메터: sigma
                {
                    lines(x, pdweibull(x, c=c[i], mu=mu[j], sigma=sigma[k]), col=color[color_counter], lwd=2);
                    color_counter = color_counter + 1;
                    legend_name = c(legend_name, paste("c = ", c[i], " / mu = ", mu[j], " / sigma = ", sigma[k], sep=""))
                }
                legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
            }
        }
        else if (func == "sdweibull")  # 생존함수
        {
            for (j in 1:len_mu)   ### 파라메터: mu
            {
                color_counter_init = color_counter
                legend_name = NULL;
                plot(x, sdweibull(x, c=c[1], mu=mu[1], sigma=sigma[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Survival Function")
                for (k in 1:len_sigma)   ### 파라메터: sigma
                {
                    lines(x, sdweibull(x, c=c[i], mu=mu[j], sigma=sigma[k]), col=color[color_counter], lwd=2);
                    color_counter = color_counter + 1;
                    legend_name = c(legend_name, paste("c = ", c[i], " / mu = ", mu[j], " / sigma = ", sigma[k], sep=""))
                }
                legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
            }
        }
        else if (func == "hdweibull")  # 위험함수
        {
            for (j in 1:len_mu)   ### 파라메터: mu
            {
                color_counter_init = color_counter
                legend_name = NULL;
                plot(x, hdweibull(x, c=c[1], mu=mu[1], sigma=sigma[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Hazard Function")
                for (k in 1:len_sigma)   ### 파라메터: sigma
                {
                    lines(x, hdweibull(x, c=c[i], mu=mu[j], sigma=sigma[k]), col=color[color_counter], lwd=2);
                    color_counter = color_counter + 1;
                    legend_name = c(legend_name, paste("c = ", c[i], " / mu = ", mu[j], " / sigma = ", sigma[k], sep=""))
                }
                legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
            }
        }
    }
}

par(mfrow = c(9, 8))
plot.dweibull_seq(x, mu, sigma, c, xlim=c(min(x), max(x)), ylim=c(-10, 10), func="ddweibull")

par(mfrow = c(9, 8))
plot.dweibull_seq(x, mu, sigma, c, xlim=c(min(x), max(x)), ylim=c(-5, 5), func="pdweibull")

par(mfrow = c(9, 8))
plot.dweibull_seq(x, mu, sigma, c, xlim=c(min(x), max(x)), ylim=c(-5, 5), func="sdweibull")

par(mfrow = c(9, 8))
plot.dweibull_seq(x, mu, sigma, c, xlim=c(min(x), max(x)), ylim=c(-30, 30), func="hdweibull")