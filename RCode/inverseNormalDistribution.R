setwd("/home/lv999/Dropbox/Github/SurvivalAnalysis/RCode")
source("colorPalette.R")
require(rmtil)


##### Chi-square Distribution
### parameter
m = c(0.25, 0.5, 0.75, 1, 2, 4, 8)
s = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x <- seq(0, 10, length.out = 1000)


### 수명 분포
dinvgauss(x, m = 1, s = 1)


### 분위수 함수
qinvgauss(x, m = 1, s = 1)


### 난수 함수
rinvgauss(x, m = 1, s = 1)


### 누적분포함수
pinvgauss(x, m = 1, s = 1)


### 생존함수
sinvgauss = function (x, m = 1, s = 1)
{
    fx <- 1 - pinvgauss(x, m = m, s = s)
    return(fx)
}


### 위험함수
hinvgauss = function (x, m = 1, s = 1)
{
    fx <- dinvgauss(x, m = m, s = s) / sinvgauss(x, m = m, s = s)
    return(fx)
}





##### Plot
plot.invgauss_seq = function(x, m = 1, s = 1, xlim=c(0, 10), ylim=c(0, 5), func="dinvgauss")
{
    color=colorPalette(300)

    len_m = length(m)       # m 파라메터의 길이
    len_s = length(s)          # s 파라메터의 길이
    
    color_counter = 1
    for (i in 1:len_m)  ### 파라메터: m
    {
        color_counter_init = color_counter
        legend_name = NULL;
        
        if (func=="dinvgauss")     # 수명분포
        {
            plot(x, dinvgauss(x, m=m[1], s=s[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Life Distribution Function")
            for (j in 1:len_s)   ### 파라메터: s
            {
                lines(x, dinvgauss(x, m=m[i], s=s[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("m = ", m[i], " / s = ", s[j], sep=""))
            }
        }
        else if (func == "pinvgauss")  # 누적분포함수
        {
            plot(x, pinvgauss(x, m=m[1], s=s[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Cumlative Distribution Function")
            for (j in 1:len_s)   ### 파라메터: s
            {
                lines(x, pinvgauss(x, m=m[i], s=s[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("m = ", m[i], " / s = ", s[j], sep=""))
            }
        }
        else if (func == "sinvgauss")  # 생존함수
        {
            plot(x, sinvgauss(x, m=m[1], s=s[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Survival Function")
            for (j in 1:len_s)   ### 파라메터: s
            {
                lines(x, sinvgauss(x, m=m[i], s=s[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("m = ", m[i], " / s = ", s[j], sep=""))
            }
        }
        else if (func == "hinvgauss")  # 위험함수
        {
            plot(x, hinvgauss(x, m=m[1], s=s[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Hazard Function")
            for (j in 1:len_s)   ### 파라메터: s
            {
                lines(x, hinvgauss(x, m=m[i], s=s[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("m = ", m[i], " / s = ", s[j], sep=""))
            }
        }
        legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
    }
}

par(mfrow = c(3, 3))
plot.invgauss_seq(x, m, s, xlim=c(min(x), max(x)), ylim=c(0, 10), func="dinvgauss")

par(mfrow = c(3, 3))
plot.invgauss_seq(x, m, s, xlim=c(min(x), max(x)), ylim=c(0, 1), func="pinvgauss")

par(mfrow = c(3, 3))
plot.invgauss_seq(x, m, s, xlim=c(min(x), max(x)), ylim=c(0, 1), func="sinvgauss")

par(mfrow = c(3, 3))
plot.invgauss_seq(x, m, s, xlim=c(min(x), max(x)), ylim=c(0, 25), func="hinvgauss")
