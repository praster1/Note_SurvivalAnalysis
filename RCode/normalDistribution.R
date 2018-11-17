setwd("/home/lv999/Dropbox/Github/SurvivalAnalysis/RCode")
source("colorPalette.R")


##### Chi-square Distribution
### parameter
mean = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
sd = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x <- seq(-10, 10, length.out = 1000)


### 수명 분포
dnorm(x, mean = mean, sd = sd)


### 분위수 함수
qnorm(x, mean = mean, sd = sd)


### 난수 함수
rnorm(x, mean = mean, sd = sd)


### 누적분포함수
pnorm(x, mean = mean, sd = sd)


### 생존함수
snorm = function (x, mean = 0, sd = 1)
{
    fx <- 1 - pnorm(x, mean = mean, sd = sd)
    return(fx)
}


### 위험함수
hnorm = function (x, mean = 0, sd = 1)
{
    fx <- dnorm(x, mean = mean, sd = sd) / snorm(x, mean = mean, sd = sd)
    return(fx)
}





##### Plot
plot.norm_seq = function(x, mean = 1, sd = 0, xlim=c(0, 10), ylim=c(0, 5), func="dnorm")
{
    color=colorPalette(300)

    len_mean = length(mean)       # mean 파라메터의 길이
    len_sd = length(sd)          # sd 파라메터의 길이
    
    color_counter = 1
    for (i in 1:len_mean)  ### 파라메터: mean
    {
        color_counter_init = color_counter
        legend_name = NULL;
        
        if (func=="dnorm")     # 수명분포
        {
            plot(x, dnorm(x, mean=mean[1], sd=sd[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Life Distribution Function")
            for (j in 1:len_sd)   ### 파라메터: sd
            {
                lines(x, dnorm(x, mean=mean[i], sd=sd[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("mean = ", mean[i], " / sd = ", sd[j], sep=""))
            }
        }
        else if (func == "pnorm")  # 누적분포함수
        {
            plot(x, pnorm(x, mean=mean[1], sd=sd[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Cumeanlative Distribution Function")
            for (j in 1:len_sd)   ### 파라메터: sd
            {
                lines(x, pnorm(x, mean=mean[i], sd=sd[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("mean = ", mean[i], " / sd = ", sd[j], sep=""))
            }
        }
        else if (func == "snorm")  # 생존함수
        {
            plot(x, snorm(x, mean=mean[1], sd=sd[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Survival Function")
            for (j in 1:len_sd)   ### 파라메터: sd
            {
                lines(x, snorm(x, mean=mean[i], sd=sd[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("mean = ", mean[i], " / sd = ", sd[j], sep=""))
            }
        }
        else if (func == "hnorm")  # 위험함수
        {
            plot(x, hnorm(x, mean=mean[1], sd=sd[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Hazard Function")
            for (j in 1:len_sd)   ### 파라메터: sd
            {
                lines(x, hnorm(x, mean=mean[i], sd=sd[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("mean = ", mean[i], " / sd = ", sd[j], sep=""))
            }
        }
        legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
    }
}

par(mfrow = c(3, 3))
plot.norm_seq(x, mean, sd, xlim=c(min(x), max(x)), ylim=c(0, 10), func="dnorm")

par(mfrow = c(3, 3))
plot.norm_seq(x, mean, sd, xlim=c(min(x), max(x)), ylim=c(0, 1), func="pnorm")

par(mfrow = c(3, 3))
plot.norm_seq(x, mean, sd, xlim=c(min(x), max(x)), ylim=c(0, 1), func="snorm")

par(mfrow = c(3, 3))
plot.norm_seq(x, mean, sd, xlim=c(min(x), max(x)), ylim=c(0, 25), func="hnorm")
