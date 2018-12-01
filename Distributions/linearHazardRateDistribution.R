setwd("/home/lv999/Dropbox/Github/SurvivalAnalysis/Distributions")
source("colorPalette.R")


##### 선형 증가 분포
### parameter
alpha = c(0, 0.25, 0.5, 0.75, 1, 2, 4, 8)	# m (delta)
beta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)		# s (beta)

### input varialbe
x = seq(0.1, 10, length.out = 1000)


### 수명 분포
dlinearFR = function(x, alpha = 0, beta = 1)
{
    fx = (alpha * x + beta) * exp(- (1/2) * alpha * x^2 + beta * x)
    return(fx)
}


### 누적분포함수
plinearFR = function(x, alpha = 0, beta = 1)
{
    Fx = 1 - exp(- (1/2) * alpha * x^2 + beta * x)
    return(Fx)
}


### 생존함수
slinearFR = function(x, alpha = 0, beta = 1)
{
    fx = 1 - plinearFR(x, alpha = alpha, beta = beta)
    return(fx)
}


### 위험함수
hlinearFR = function(x, alpha = 0, beta = 1)
{
    fx = dlinearFR(x, alpha = alpha, beta = beta) / slinearFR(x, alpha = alpha, beta = beta)
    return(fx)
}





##### Plot
plot.linearFR_seq = function(x, alpha = 0, beta = 1, xlim=c(0, 10), ylim=c(0, 5), func="dlinearFR")
{
    color=colorPalette(300)

    len_alpha = length(alpha)       # m 파라메터의 길이
    len_beta = length(beta)          # s 파라메터의 길이
    
    color_counter = 1
    for (i in 1:len_alpha)  ### 파라메터: m
    {
        color_counter_init = color_counter
        legend_name = NULL;
        
        if (func=="dlinearFR")     # 수명분포
        {
            plot(x, dlinearFR(x, alpha=alpha[1], beta=beta[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Life Distribution Function")
            for (j in 1:len_beta)   ### 파라메터: s
            {
                lines(x, dlinearFR(x, alpha=alpha[i], beta=beta[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("alpha = ", m[i], " / beta = ", s[j], sep=""))
            }
        }
        else if (func == "plinearFR")  # 누적분포함수
        {
            plot(x, plinearFR(x, alpha=alpha[1], beta=beta[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Cumulative Distribution Function")
            for (j in 1:len_beta)   ### 파라메터: s
            {
                lines(x, plinearFR(x, alpha=alpha[i], beta=beta[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("alpha = ", m[i], " / beta = ", s[j], sep=""))
            }
        }
        else if (func == "slinearFR")  # 생존함수
        {
            plot(x, slinearFR(x, alpha=alpha[1], beta=beta[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Survival Function")
            for (j in 1:len_beta)   ### 파라메터: s
            {
                lines(x, slinearFR(x, alpha=alpha[i], beta=beta[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("alpha = ", m[i], " / beta = ", s[j], sep=""))
            }
        }
        else if (func == "hlinearFR")  # 위험함수
        {
            plot(x, hlinearFR(x, alpha=alpha[1], beta=beta[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Hazard Function")
            for (j in 1:len_beta)   ### 파라메터: s
            {
                lines(x, hlinearFR(x, alpha=alpha[i], beta=beta[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("alpha = ", m[i], " / beta = ", s[j], sep=""))
            }
        }
        legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
    }
}

par(mfrow = c(3, 3))
plot.linearFR_seq(x, alpha, beta, xlim=c(min(x), max(x)), ylim=c(0, 50), func="dlinearFR")

par(mfrow = c(3, 3))
plot.linearFR_seq(x, alpha, beta, xlim=c(min(x), max(x)), ylim=c(0, 1), func="plinearFR")

par(mfrow = c(3, 3))
plot.linearFR_seq(x, alpha, beta, xlim=c(min(x), max(x)), ylim=c(0, 1), func="slinearFR")

par(mfrow = c(3, 3))
plot.linearFR_seq(x, alpha, beta, xlim=c(min(x), max(x)), ylim=c(0, 50), func="hlinearFR")

