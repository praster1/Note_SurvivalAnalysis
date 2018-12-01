setwd("/home/lv999/Dropbox/Github/SurvivalAnalysis/Distributions")
source("colorPalette.R")
require(rmutil)


##### Hjorth Distribution
### parameter
delta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)	# m (delta)
beta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)		# s (beta)
theta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)	# f (theta)

### input varialbe
x = seq(0.1, 1, length.out = 1000)


### 수명 분포
dhjorth(x, m = 1, s = 1, f = 1)


### 분위수 함수
qhjorth(x, m = 1, s = 1, f = 1)


### 난수 함수
rhjorth(x, m = 1, s = 1, f = 1)


### 누적분포함수
phjorth(x, m = 1, s = 1, f = 1)


### 생존함수
shjorth = function (x, m = 1, s = 1, f=1)
{
    fx = 1 - phjorth(x, m = m, s = m, f = f)
    return(fx)
}


### 위험함수
hhjorth = function (x, m = 1, s = 1, f=1)
{
    fx = dhjorth(x, m = m, s = m, f = f) / shjorth(x, m = m, s = m, f = f)
    return(fx)
}





##### Plot
plot.hjorth_seq = function(x, m = 1, s = 1, f = 1, xlim=c(0, 10), ylim=c(0, 5), func="dhjorth")
{
    color=colorPalette(300)

    len_m = length(m)       # m 파라메터의 길이
    len_s = length(s)          # s 파라메터의 길이
    len_f = length(f)  # f 파라메터의 길이
    
    color_counter = 1
    for (i in 1:len_m)  ### 파라메터: m
    {
        if (func=="dhjorth")     # 수명분포
        {
            for (j in 1:len_s)   ### 파라메터: s
            {
                color_counter_init = color_counter
                legend_name = NULL;
                plot(x, dhjorth(x, m=m[1], s=s[1], f=f[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Life Distribution Function")
                for (k in 1:len_f)   ### 파라메터: f
                {
                    lines(x, dhjorth(x, m=m[i], s=s[j], f=f[k]), col=color[color_counter], lwd=2);
                    color_counter = color_counter + 1;
                    legend_name = c(legend_name, paste("m = ", m[i], " / s = ", s[j], " / f = ", f[k], sep=""))
                }
                legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
            }
        }
        else if (func == "phjorth")  # 누적분포함수
        {
            for (j in 1:len_s)   ### 파라메터: s
            {
                color_counter_init = color_counter
                legend_name = NULL;
                plot(x, phjorth(x, m=m[1], s=s[1], f=f[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Cumulative Distribution Function")
                for (k in 1:len_f)   ### 파라메터: f
                {
                    lines(x, phjorth(x, m=m[i], s=s[j], f=f[k]), col=color[color_counter], lwd=2);
                    color_counter = color_counter + 1;
                    legend_name = c(legend_name, paste("m = ", m[i], " / s = ", s[j], " / f = ", f[k], sep=""))
                }
                legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
            }
        }
        else if (func == "shjorth")  # 생존함수
        {
            for (j in 1:len_s)   ### 파라메터: s
            {
                color_counter_init = color_counter
                legend_name = NULL;
                plot(x, shjorth(x, m=m[1], s=s[1], f=f[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Survival Function")
                for (k in 1:len_f)   ### 파라메터: f
                {
                    lines(x, shjorth(x, m=m[i], s=s[j], f=f[k]), col=color[color_counter], lwd=2);
                    color_counter = color_counter + 1;
                    legend_name = c(legend_name, paste("m = ", m[i], " / s = ", s[j], " / f = ", f[k], sep=""))
                }
                legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
            }
        }
        else if (func == "hhjorth")  # 위험함수
        {
            for (j in 1:len_s)   ### 파라메터: s
            {
                color_counter_init = color_counter
                legend_name = NULL;
                plot(x, hhjorth(x, m=m[1], s=s[1], f=f[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Hazard Function")
                for (k in 1:len_f)   ### 파라메터: f
                {
                    lines(x, hhjorth(x, m=m[i], s=s[j], f=f[k]), col=color[color_counter], lwd=2);
                    color_counter = color_counter + 1;
                    legend_name = c(legend_name, paste("m = ", m[i], " / s = ", s[j], " / f = ", f[k], sep=""))
                }
                legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
            }
        }
    }
}

par(mfrow = c(9, 8))
plot.hjorth_seq(x, delta, beta, theta, xlim=c(min(x), max(x)), ylim=c(-10, 10), func="dhjorth")

par(mfrow = c(9, 8))
plot.hjorth_seq(x, delta, beta, theta, xlim=c(min(x), max(x)), ylim=c(-5, 5), func="phjorth")

par(mfrow = c(9, 8))
plot.hjorth_seq(x, delta, beta, theta, xlim=c(min(x), max(x)), ylim=c(-5, 5), func="shjorth")

par(mfrow = c(9, 8))
plot.hjorth_seq(x, delta, beta, theta, xlim=c(min(x), max(x)), ylim=c(-30, 30), func="hhjorth")