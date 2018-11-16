setwd("/home/lv999/Dropbox/Github/SurvivalAnalysis/RCode")
source("colorPalette.R")
require(rmutil)


##### laplace Distribution
### parameter
m = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
s = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x <- seq(-10, 10, length.out = 1000)


### 수명 분포
dlaplace(x, m = 0, s = 1)


### 분위수 함수
qlaplace(x, m = 0, s = 1)


### 난수 함수
rlaplace(x, m = 0, s = 1)


### 누적분포함수
plaplace(x, m = 0, s = 1)


### 생존함수
slaplace = function(x, m = 0, s = 1)
{
    fx <- 1 - plaplace(x, m = m, s = s)
    return(fx)
}


### 위험함수
hlaplace = function(x, m = 0, s = 1)
{
    fx <- dlaplace(x, m = m, s = s) / slaplace(x, m = m, s = s)
    return(fx)
}





##### Plot
plot.laplace_seq = function(x, m = 0, s = 1, xlim=c(0, 10), ylim=c(0, 5), func="dlaplace")
{
    color=colorPalette(300)

    len_m = length(m)       # m 파라메터의 길이
    len_s = length(s)          # s 파라메터의 길이
    
    color_counter = 1
    for (i in 1:len_m)  ### 파라메터: m
    {
        color_counter_init = color_counter
        legend_name = NULL;
        
        if (func=="dlaplace")     # 수명분포
        {
            plot(x, dlaplace(x, m=m[1], s=s[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Life Distribution Function")
            for (j in 1:len_s)   ### 파라메터: s
            {
                lines(x, dlaplace(x, m=m[i], s=s[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("m = ", m[i], " / s = ", s[j], sep=""))
            }
        }
        else if (func == "plaplace")  # 누적분포함수
        {
            plot(x, plaplace(x, m=m[1], s=s[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Cumulative Distribution Function")
            for (j in 1:len_s)   ### 파라메터: s
            {
                lines(x, plaplace(x, m=m[i], s=s[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("m = ", m[i], " / s = ", s[j], sep=""))
            }
        }
        else if (func == "slaplace")  # 생존함수
        {
            plot(x, slaplace(x, m=m[1], s=s[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Survival Function")
            for (j in 1:len_s)   ### 파라메터: s
            {
                lines(x, slaplace(x, m=m[i], s=s[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("m = ", m[i], " / s = ", s[j], sep=""))
            }
        }
        else if (func == "hlaplace")  # 위험함수
        {
            plot(x, hlaplace(x, m=m[1], s=s[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Hazard Function")
            for (j in 1:len_s)   ### 파라메터: s
            {
                lines(x, hlaplace(x, m=m[i], s=s[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("m = ", m[i], " / s = ", s[j], sep=""))
            }
        }
        legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
    }
}

par(mfrow = c(3, 3))
plot.laplace_seq(x, m, s, xlim=c(min(x), max(x)), ylim=c(0, 5), func="dlaplace")

par(mfrow = c(3, 3))
plot.laplace_seq(x, m, s, xlim=c(min(x), max(x)), ylim=c(0, 1), func="plaplace")

par(mfrow = c(3, 3))
plot.laplace_seq(x, m, s, xlim=c(min(x), max(x)), ylim=c(0, 1), func="slaplace")

par(mfrow = c(3, 3))
plot.laplace_seq(x, m, s, xlim=c(min(x), max(x)), ylim=c(0, 10), func="hlaplace")
