setwd("/home/lv999/Dropbox/Github/SurvivalAnalysis/Distributions")
source("colorPalette.R")


##### Cosine Distribution
### parameter
mu = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
sigma = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x = seq(-10, 10, length.out = 1000)


### 수명 분포
dcosine = function(x, mu, sigma) 
{
    fx = (1/(2*sigma))*(cos( (x-mu)/sigma))
    return(fx)
}


### 누적분포함수
pcosine = function(x, mu = 1, sigma = 0)
{
    fx = -(scosine(x, mu, sigma) - 1)
    return(fx)
}


### 생존함수
scosine = function (x, mu = 1, sigma = 0) 
{
    fx = (1/2) * (1 - sin((x-mu)/sigma))
    return(fx)
}


### 위험함수
hcosine = function (x, mu = 1, sigma = 0)
{
    fx = dcosine(x, mu, sigma) / scosine(x, mu, sigma)
    return(fx)
}





##### Plot
plot.cosine_seq = function(x, mu = 1, sigma = 0, xlim=c(0, 10), ylim=c(0, 5), func="dcosine")
{
    color=colorPalette(300)

    len_mu = length(mu)       # mu 파라메터의 길이
    len_sigma = length(sigma)          # sigma 파라메터의 길이
    
    color_counter = 1
    for (i in 1:len_mu)  ### 파라메터: mu
    {
        color_counter_init = color_counter
        legend_name = NULL;
        
        if (func=="dcosine")     # 수명분포
        {
            plot(x, dcosine(x, mu=mu[1], sigma=sigma[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Life Distribution Function")
            for (j in 1:len_sigma)   ### 파라메터: sigma
            {
                lines(x, dcosine(x, mu=mu[i], sigma=sigma[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("mu = ", mu[i], " / sigma = ", sigma[j], sep=""))
            }
        }
        else if (func == "pcosine")  # 누적분포함수
        {
            plot(x, pcosine(x, mu=mu[1], sigma=sigma[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Cumulative Distribution Function")
            for (j in 1:len_sigma)   ### 파라메터: sigma
            {
                lines(x, pcosine(x, mu=mu[i], sigma=sigma[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("mu = ", mu[i], " / sigma = ", sigma[j], sep=""))
            }
        }
        else if (func == "scosine")  # 생존함수
        {
            plot(x, scosine(x, mu=mu[1], sigma=sigma[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Survival Function")
            for (j in 1:len_sigma)   ### 파라메터: sigma
            {
                lines(x, scosine(x, mu=mu[i], sigma=sigma[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("mu = ", mu[i], " / sigma = ", sigma[j], sep=""))
            }
        }
        else if (func == "hcosine")  # 위험함수
        {
            plot(x, hcosine(x, mu=mu[1], sigma=sigma[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Hazard Function")
            for (j in 1:len_sigma)   ### 파라메터: sigma
            {
                lines(x, hcosine(x, mu=mu[i], sigma=sigma[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("mu = ", mu[i], " / sigma = ", sigma[j], sep=""))
            }
        }
        legend('topright', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
    }
}

par(mfrow = c(3, 3))
plot.cosine_seq(x, mu, sigma, xlim=c(min(x), max(x)), ylim=c(-5, 5), func="dcosine")

par(mfrow = c(3, 3))
plot.cosine_seq(x, mu, sigma, xlim=c(min(x), max(x)), ylim=c(0, 1), func="pcosine")

par(mfrow = c(3, 3))
plot.cosine_seq(x, mu, sigma, xlim=c(min(x), max(x)), ylim=c(0, 1), func="scosine")

par(mfrow = c(3, 3))
plot.cosine_seq(x, mu, sigma, xlim=c(min(x), max(x)), ylim=c(-20, 20), func="hcosine")
