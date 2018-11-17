setwd("/home/lv999/Dropbox/Github/SurvivalAnalysis/RCode")
source("colorPalette.R")
require(extraDistr)


##### Chi-square Distribution
### parameter
sigma = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x = seq(0, 10, length.out = 1000)


### 수명 분포
dhnorm(x, sigma = 1)


### 분위수 함수
qhnorm(x, sigma = 1)


### 난수 함수
rhnorm(x, sigma = 1)


### 누적분포함수
phnorm(x, sigma = 1)


### 생존함수
shnorm = function (x, sigma = 1)
{
    fx = 1 - phnorm(x, sigma = sigma)
    return(fx)
}


### 위험함수
hhnorm = function (x, sigma = 1)
{
    fx = dhnorm(x, sigma = sigma) / shnorm(x, sigma = sigma)
    return(fx)
}





##### Plot
plot.hnorm_seq = function(x, sigma = 1, xlim=c(0, 10), ylim=c(0, 5), func="dhnorm")
{
    color=colorPalette(300)

    len_sigma = length(sigma)       # sigma 파라메터의 길이
    
    color_counter = 1
    color_counter_init = color_counter
    legend_name = NULL;


    if (func=="dhnorm")     # 수명분포
    {    
        plot(x, dhnorm(x, sigma=sigma[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Life Distribution Function")
        for (i in 1:len_sigma)  ### 파라메터: sigma
        {
            lines(x, dhnorm(x, sigma=sigma[i]), col=color[color_counter], lwd=2);
            color_counter = color_counter + 1;
            legend_name = c(legend_name, paste("sigma = ", sigma[i], sep=""))
        }
    }
    else if (func == "phnorm")  # 누적분포함수
    {
        plot(x, phnorm(x, sigma=sigma[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Cumulative Distribution Function")
        for (i in 1:len_sigma)  ### 파라메터: sigma
        {
            lines(x, phnorm(x, sigma=sigma[i]), col=color[color_counter], lwd=2);
            color_counter = color_counter + 1;
            legend_name = c(legend_name, paste("sigma = ", sigma[i], sep=""))
        }
    }
    else if (func == "shnorm")  # 생존함수
    {
        plot(x, shnorm(x, sigma=sigma[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Survival Function")
        for (i in 1:len_sigma)  ### 파라메터: sigma
        {
            lines(x, shnorm(x, sigma=sigma[i]), col=color[color_counter], lwd=2);
            color_counter = color_counter + 1;
            legend_name = c(legend_name, paste("sigma = ", sigma[i], sep=""))
        }
    }
    else if (func == "hhnorm")  # 위험함수
    {
        plot(x, hhnorm(x, sigma=sigma[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Hazard Function")
        for (i in 1:len_sigma)  ### 파라메터: sigma
        {
            lines(x, hhnorm(x, sigma=sigma[i]), col=color[color_counter], lwd=2);
            color_counter = color_counter + 1;
            legend_name = c(legend_name, paste("sigma = ", sigma[i], sep=""))
        }
    }
    legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
}

par(mfrow = c(2, 2))
plot.hnorm_seq(x, sigma, xlim=c(min(x), max(x)), ylim=c(0, 2), func="dhnorm")
plot.hnorm_seq(x, sigma, xlim=c(min(x), max(x)), ylim=c(0, 1), func="phnorm")
plot.hnorm_seq(x, sigma, xlim=c(min(x), max(x)), ylim=c(0, 1), func="shnorm")
plot.hnorm_seq(x, sigma, xlim=c(min(x), max(x)), ylim=c(0, 2), func="hhnorm")