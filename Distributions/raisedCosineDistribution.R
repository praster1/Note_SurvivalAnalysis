setwd("/home/lv999/Dropbox/Github/SurvivalAnalysis/RCode")
source("colorPalette.R")


##### rcosine Distribution
### parameter
mu = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
sigma = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x = seq(-10, 10, length.out = 1000)


### 수명 분포
drcosine = function(x, mu, sigma) 
{
    fx = (1/(2*sigma))*(1 + cos( pi * (x-mu)/sigma ))
    return(fx)
}


### 누적분포함수
prcosine = function(x, mu = 1, sigma = 0)
{
    fx = -(srcosine(x, mu, sigma) - 1)
    return(fx)
}


### 생존함수
srcosine = function (x, mu = 1, sigma = 0) 
{
    fx = (1/2) * (1 - ((x-mu)/sigma) - (1/pi)*sin(pi * ((x-mu)/sigma)))
    return(fx)
}


### 위험함수
hrcosine = function (x, mu = 1, sigma = 0)
{
    fx = drcosine(x, mu, sigma) / srcosine(x, mu, sigma)
    return(fx)
}





##### Plot
plot.rcosine_seq = function(x, mu = 1, sigma = 0, xlim=c(0, 10), ylim=c(0, 5), func="drcosine")
{
    color=colorPalette(300)

    len_mu = length(mu)       # mu 파라메터의 길이
    len_sigma = length(sigma)          # sigma 파라메터의 길이
    
    color_counter = 1
    for (i in 1:len_mu)  ### 파라메터: mu
    {
        color_counter_init = color_counter
        legend_name = NULL;
        
        if (func=="drcosine")     # 수명분포
        {
            plot(x, drcosine(x, mu=mu[1], sigma=sigma[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Life Distribution Function")
            for (j in 1:len_sigma)   ### 파라메터: sigma
            {
                lines(x, drcosine(x, mu=mu[i], sigma=sigma[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("mu = ", i, " / sigma = ", j, sep=""))
            }
        }
        else if (func == "prcosine")  # 누적분포함수
        {
            plot(x, prcosine(x, mu=mu[1], sigma=sigma[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Cumulative Distribution Function")
            for (j in 1:len_sigma)   ### 파라메터: sigma
            {
                lines(x, prcosine(x, mu=mu[i], sigma=sigma[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("mu = ", i, " / sigma = ", j, sep=""))
            }
        }
        else if (func == "srcosine")  # 생존함수
        {
            plot(x, srcosine(x, mu=mu[1], sigma=sigma[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Survival Function")
            for (j in 1:len_sigma)   ### 파라메터: sigma
            {
                lines(x, srcosine(x, mu=mu[i], sigma=sigma[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("mu = ", i, " / sigma = ", j, sep=""))
            }
        }
        else if (func == "hrcosine")  # 위험함수
        {
            plot(x, hrcosine(x, mu=mu[1], sigma=sigma[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Hazard Function")
            for (j in 1:len_sigma)   ### 파라메터: sigma
            {
                lines(x, hrcosine(x, mu=mu[i], sigma=sigma[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("mu = ", i, " / sigma = ", j, sep=""))
            }
        }
        legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
    }
}

par(mfrow = c(3, 3))
plot.rcosine_seq(x, mu, sigma, xlim=c(min(x), max(x)), ylim=c(0, 5), func="drcosine")

par(mfrow = c(3, 3))
plot.rcosine_seq(x, mu, sigma, xlim=c(min(x), max(x)), ylim=c(-5, 5), func="prcosine")

par(mfrow = c(3, 3))
plot.rcosine_seq(x, mu, sigma, xlim=c(min(x), max(x)), ylim=c(-5, 5), func="srcosine")

par(mfrow = c(3, 3))
plot.rcosine_seq(x, mu, sigma, xlim=c(min(x), max(x)), ylim=c(-20, 20), func="hrcosine")