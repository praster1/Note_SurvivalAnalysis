setwd("/home/lv999/Dropbox/Github/SurvivalAnalysis/RCode")
source("colorPalette.R")


##### Chi-square Distribution
### parameter
meanlog = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
sdlog = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x = seq(0, 10, length.out = 1000)


### 수명 분포
dlnorm(x, meanlog = meanlog, sdlog = sdlog)


### 분위수 함수
qlnorm(x, meanlog = meanlog, sdlog = sdlog)


### 난수 함수
rlnorm(x, meanlog = meanlog, sdlog = sdlog)


### 누적분포함수
plnorm(x, meanlog = meanlog, sdlog = sdlog)


### 생존함수
slnorm = function (x, meanlog = 0, sdlog = 1)
{
    fx = 1 - plnorm(x, meanlog = meanlog, sdlog = sdlog)
    return(fx)
}


### 위험함수
hlnorm = function (x, meanlog = 0, sdlog = 1)
{
    fx = dlnorm(x, meanlog = meanlog, sdlog = sdlog) / slnorm(x, meanlog = meanlog, sdlog = sdlog)
    return(fx)
}





##### Plot
plot.lnorm_seq = function(x, meanlog = 1, sdlog = 0, xlim=c(0, 10), ylim=c(0, 5), func="dlnorm")
{
    color=colorPalette(300)

    len_meanlog = length(meanlog)       # meanlog 파라메터의 길이
    len_sdlog = length(sdlog)          # sdlog 파라메터의 길이
    
    color_counter = 1
    for (i in 1:len_meanlog)  ### 파라메터: meanlog
    {
        color_counter_init = color_counter
        legend_name = NULL;
        
        if (func=="dlnorm")     # 수명분포
        {
            plot(x, dlnorm(x, meanlog=meanlog[1], sdlog=sdlog[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Life Distribution Function")
            for (j in 1:len_sdlog)   ### 파라메터: sdlog
            {
                lines(x, dlnorm(x, meanlog=meanlog[i], sdlog=sdlog[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("meanlog = ", meanlog[i], " / sdlog = ", sdlog[j], sep=""))
            }
        }
        else if (func == "plnorm")  # 누적분포함수
        {
            plot(x, plnorm(x, meanlog=meanlog[1], sdlog=sdlog[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Cumeanloglative Distribution Function")
            for (j in 1:len_sdlog)   ### 파라메터: sdlog
            {
                lines(x, plnorm(x, meanlog=meanlog[i], sdlog=sdlog[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("meanlog = ", meanlog[i], " / sdlog = ", sdlog[j], sep=""))
            }
        }
        else if (func == "slnorm")  # 생존함수
        {
            plot(x, slnorm(x, meanlog=meanlog[1], sdlog=sdlog[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Survival Function")
            for (j in 1:len_sdlog)   ### 파라메터: sdlog
            {
                lines(x, slnorm(x, meanlog=meanlog[i], sdlog=sdlog[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("meanlog = ", meanlog[i], " / sdlog = ", sdlog[j], sep=""))
            }
        }
        else if (func == "hlnorm")  # 위험함수
        {
            plot(x, hlnorm(x, meanlog=meanlog[1], sdlog=sdlog[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Hazard Function")
            for (j in 1:len_sdlog)   ### 파라메터: sdlog
            {
                lines(x, hlnorm(x, meanlog=meanlog[i], sdlog=sdlog[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("meanlog = ", meanlog[i], " / sdlog = ", sdlog[j], sep=""))
            }
        }
        legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
    }
}

par(mfrow = c(3, 3))
plot.lnorm_seq(x, meanlog, sdlog, xlim=c(min(x), max(x)), ylim=c(0, 10), func="dlnorm")

par(mfrow = c(3, 3))
plot.lnorm_seq(x, meanlog, sdlog, xlim=c(min(x), max(x)), ylim=c(0, 1), func="plnorm")

par(mfrow = c(3, 3))
plot.lnorm_seq(x, meanlog, sdlog, xlim=c(min(x), max(x)), ylim=c(0, 1), func="slnorm")

par(mfrow = c(3, 3))
plot.lnorm_seq(x, meanlog, sdlog, xlim=c(min(x), max(x)), ylim=c(0, 25), func="hlnorm")
