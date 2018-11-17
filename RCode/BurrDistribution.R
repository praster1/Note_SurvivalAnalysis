setwd("/home/lv999/Dropbox/Github/SurvivalAnalysis/RCode")
source("colorPalette.R")
require(extremefit)


##### Burr Distribution
### parameter
alpha = c(0.25, 0.5, 0.75, 1, 2, 4, 8)
beta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x = seq(0, 3, length.out = 1000)


### 수명 분포
rburr(x, a=alpha, k=beta)


### 분위수 함수
qburr(x, a=alpha, k=beta)


### 난수 함수
rburr(x, a=alpha, k=beta)


### 누적분포함수
pburr(x, a=alpha, k=beta)


### 생존함수
sburr = function (x, a = 1, k = 0) 
{
    fx = 1 - pburr(x, a=a, k=k)
    return(fx)
}


### 위험함수
hburr = function (x, a=a, k=k)
{
    fx = dburr(x, a=a, k=k) / sburr(x, a=a, k=k)
    return(fx)
}





##### Plot
plot.burr_seq = function(x, alpha = 1, beta = 0, xlim=c(0, 10), ylim=c(0, 5), func="dburr")
{
    color=colorPalette(300)

    len_alpha = length(alpha)       # alpha 파라메터의 길이
    len_beta = length(beta)          # beta 파라메터의 길이
    
    color_counter = 1
    for (i in 1:len_alpha)  ### 파라메터: alpha
    {
        color_counter_init = color_counter
        legend_name = NULL;
        
        if (func=="dburr")     # 수명분포
        {
            plot(x, dburr(x, a=alpha[1], k=beta[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Life Distribution Function")
            for (j in 1:len_beta)   ### 파라메터: beta
            {
                lines(x, dburr(x, a=alpha[i], k=beta[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("alpha = ", alpha[i], " / beta = ", beta[j], sep=""))
            }
        }
        else if (func == "pburr")  # 누적분포함수
        {
            plot(x, pburr(x, a=alpha[1], k=beta[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Cumulative Distribution Function")
            for (j in 1:len_beta)   ### 파라메터: beta
            {
                lines(x, pburr(x, a=alpha[i], k=beta[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("alpha = ", alpha[i], " / beta = ", beta[j], sep=""))
            }
        }
        else if (func == "sburr")  # 생존함수
        {
            plot(x, sburr(x, a=alpha[1], k=beta[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Survival Function")
            for (j in 1:len_beta)   ### 파라메터: beta
            {
                lines(x, sburr(x, a=alpha[i], k=beta[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("alpha = ", alpha[i], " / beta = ", beta[j], sep=""))
            }
        }
        else if (func == "hburr")  # 위험함수
        {
            plot(x, hburr(x, a=alpha[1], k=beta[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Hazard Function")
            for (j in 1:len_beta)   ### 파라메터: beta
            {
                lines(x, hburr(x, a=alpha[i], k=beta[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("alpha = ", alpha[i], " / beta = ", beta[j], sep=""))
            }
        }
        legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
    }
}

par(mfrow = c(4, 2))
plot.burr_seq(x, alpha, beta, xlim=c(min(x), max(x)), ylim=c(0, 10), func="dburr")

par(mfrow = c(4, 2))
plot.burr_seq(x, alpha, beta, xlim=c(min(x), max(x)), ylim=c(0, 1), func="pburr")

par(mfrow = c(4, 2))
plot.burr_seq(x, alpha, beta, xlim=c(min(x), max(x)), ylim=c(0, 1), func="sburr")

par(mfrow = c(4, 2))
plot.burr_seq(x, alpha, beta, xlim=c(min(x), max(x)), ylim=c(0, 25), func="hburr")
