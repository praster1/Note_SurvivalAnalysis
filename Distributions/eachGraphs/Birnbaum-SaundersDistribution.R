
source("colorPalette.R")
require(extraDistr)


##### Birnbaum-Saunders Distribution
### parameter
alpha = c(0.25, 0.5, 0.75, 1, 2, 4, 8)
beta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x = seq(0.1, 1, length.out = 1000)


### 수명 분포
dfatigue(x, alpha, beta = 1, mu = 0)


### 분위수 함수
qfatigue(x, alpha, beta, mu = 0)


### 난수 함수
rfatigue(x, alpha, beta, mu = 0)


### 누적분포함수
pfatigue(x, alpha, beta, mu = 0)


### 생존함수
sfatigue = function (x, alpha = 1, beta = 0) 
{
    fx = 1 - pfatigue(x, alpha, beta)
    return(fx)
}


### 위험함수
hfatigue = function (x, alpha = 1, beta = 0)
{
    fx = dfatigue(x, alpha, beta) / sfatigue(x, alpha, beta)
    return(fx)
}





##### Plot
plot.fatigue_seq = function(x, alpha = 1, beta = 0, xlim=c(0, 10), ylim=c(0, 5), func="dfatigue")
{
    color=colorPalette(300)

    len_alpha = length(alpha)       # alpha 파라메터의 길이
    len_beta = length(beta)          # beta 파라메터의 길이
    
    color_counter = 1
    for (i in 1:len_alpha)  ### 파라메터: alpha
    {
        color_counter_init = color_counter
        legend_name = NULL;
        
        if (func=="dfatigue")     # 수명분포
        {
            plot(x, dfatigue(x, alpha=alpha[1], beta=beta[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Life Distribution Function")
            for (j in 1:len_beta)   ### 파라메터: beta
            {
                lines(x, dfatigue(x, alpha=alpha[i], beta=beta[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("alpha = ", alpha[i], " / beta = ", beta[j], sep=""))
            }
        }
        else if (func == "pfatigue")  # 누적분포함수
        {
            plot(x, pfatigue(x, alpha=alpha[1], beta=beta[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Cumulative Distribution Function")
            for (j in 1:len_beta)   ### 파라메터: beta
            {
                lines(x, pfatigue(x, alpha=alpha[i], beta=beta[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("alpha = ", alpha[i], " / beta = ", beta[j], sep=""))
            }
        }
        else if (func == "sfatigue")  # 생존함수
        {
            plot(x, sfatigue(x, alpha=alpha[1], beta=beta[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Survival Function")
            for (j in 1:len_beta)   ### 파라메터: beta
            {
                lines(x, sfatigue(x, alpha=alpha[i], beta=beta[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("alpha = ", alpha[i], " / beta = ", beta[j], sep=""))
            }
        }
        else if (func == "hfatigue")  # 위험함수
        {
            plot(x, hfatigue(x, alpha=alpha[1], beta=beta[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Hazard Function")
            for (j in 1:len_beta)   ### 파라메터: beta
            {
                lines(x, hfatigue(x, alpha=alpha[i], beta=beta[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("alpha = ", alpha[i], " / beta = ", beta[j], sep=""))
            }
        }
        legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
    }
}

par(mfrow = c(4, 2))
plot.fatigue_seq(x, alpha, beta, xlim=c(min(x), max(x)), ylim=c(0, 20), func="dfatigue")

par(mfrow = c(4, 2))
plot.fatigue_seq(x, alpha, beta, xlim=c(min(x), max(x)), ylim=c(0, 1), func="pfatigue")

par(mfrow = c(4, 2))
plot.fatigue_seq(x, alpha, beta, xlim=c(min(x), max(x)), ylim=c(0, 1), func="sfatigue")

par(mfrow = c(4, 2))
plot.fatigue_seq(x, alpha, beta, xlim=c(min(x), max(x)), ylim=c(0, 25), func="hfatigue")
