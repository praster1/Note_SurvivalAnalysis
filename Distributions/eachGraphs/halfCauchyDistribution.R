
source("colorPalette.R")
require(extraDistr)


##### half-Cahchy Distribution
### parameter
sigma = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x = seq(0.1, 10, length.out = 1000)


### 수명 분포
dhcauchy(x, sigma = sigma)


### 분위수 함수
qhcauchy(x, sigma = sigma)


### 난수 함수
rhcauchy(x, sigma = sigma)


### 누적분포함수
phcauchy(x, sigma = sigma)


### 생존함수
shcauchy = function (x, sigma = 1)
{
    fx = 1 - phcauchy(x, sigma = sigma)
    return(fx)
}


### 위험함수
hhcauchy = function (x, sigma = 1)
{
    fx = dhcauchy(x, sigma = sigma) / shcauchy(x, sigma = sigma)
    return(fx)
}





##### Plot
plot.hcauchy_seq = function(x, sigma = 1, xlim=c(0, 10), ylim=c(0, 5), func="dhcauchy")
{
    color=colorPalette(300)

    len_sigma = length(sigma)       # sigma 파라메터의 길이
    
    color_counter = 1
    color_counter_init = color_counter
    legend_name = NULL;


    if (func=="dhcauchy")     # 수명분포
    {    
        plot(x, dhcauchy(x, sigma=sigma[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Life Distribution Function")
        for (i in 1:len_sigma)  ### 파라메터: sigma
        {
            lines(x, dhcauchy(x, sigma=sigma[i]), col=color[color_counter], lwd=2);
            color_counter = color_counter + 1;
            legend_name = c(legend_name, paste("sigma = ", i, sep=""))
        }
    }
    else if (func == "phcauchy")  # 누적분포함수
    {
        plot(x, phcauchy(x, sigma=sigma[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Cumulative Distribution Function")
        for (i in 1:len_sigma)  ### 파라메터: sigma
        {
            lines(x, phcauchy(x, sigma=sigma[i]), col=color[color_counter], lwd=2);
            color_counter = color_counter + 1;
            legend_name = c(legend_name, paste("sigma = ", i, sep=""))
        }
    }
    else if (func == "shcauchy")  # 생존함수
    {
        plot(x, shcauchy(x, sigma=sigma[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Survival Function")
        for (i in 1:len_sigma)  ### 파라메터: sigma
        {
            lines(x, shcauchy(x, sigma=sigma[i]), col=color[color_counter], lwd=2);
            color_counter = color_counter + 1;
            legend_name = c(legend_name, paste("sigma = ", i, sep=""))
        }
    }
    else if (func == "hhcauchy")  # 위험함수
    {
        plot(x, hhcauchy(x, sigma=sigma[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Hazard Function")
        for (i in 1:len_sigma)  ### 파라메터: sigma
        {
            lines(x, hhcauchy(x, sigma=sigma[i]), col=color[color_counter], lwd=2);
            color_counter = color_counter + 1;
            legend_name = c(legend_name, paste("sigma = ", i, sep=""))
        }
    }
    legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
}

par(mfrow = c(2, 2))
plot.hcauchy_seq(x, sigma, xlim=c(min(x), max(x)), ylim=c(0, 2), func="dhcauchy")
plot.hcauchy_seq(x, sigma, xlim=c(min(x), max(x)), ylim=c(0, 1), func="phcauchy")
plot.hcauchy_seq(x, sigma, xlim=c(min(x), max(x)), ylim=c(0, 1), func="shcauchy")
plot.hcauchy_seq(x, sigma, xlim=c(min(x), max(x)), ylim=c(0, 2), func="hhcauchy")
