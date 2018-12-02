setwd("/home/lv999/Dropbox/Github/SurvivalAnalysis/Distributions")
source("colorPalette.R")


##### Beta Distribution
### parameter
alpha = c(0.25, 0.5, 0.75, 1, 2, 4, 8)
beta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x = seq(0.1, 1, length.out = 1000)


### 수명 분포
dbeta(x, alpha, beta)


### 분위수 함수
qbeta(x, alpha, beta)


### 난수 함수
rbeta(x, alpha, beta)


### 누적분포함수
pbeta(x, alpha, beta)


### 생존함수
sbeta = function (x, shape1 = 1, shape2 = 0) 
{
    fx = 1 - pbeta(x, shape1=shape1, shape2=shape2)
    return(fx)
}


### 위험함수
hbeta = function (x, shape1 = 1, shape2 = 0)
{
    fx = dbeta(x, shape1=shape1, shape2=shape2) / sbeta(x, shape1=shape1, shape2=shape2)
    return(fx)
}




##### Plot
plot.beta_seq = function(x, alpha = 1, beta = 0, xlim=c(0, 10), ylim=c(0, 5), func="dbeta")
{
    color=colorPalette(300)

    len_alpha = length(alpha)       # alpha 파라메터의 길이
    len_beta = length(beta)          # beta 파라메터의 길이
    
    color_counter = 1
    for (i in 1:len_alpha)  ### 파라메터: alpha
    {
        color_counter_init = color_counter
        legend_name = NULL;
        
        if (func=="dbeta")     # 수명분포
        {
            plot(x, dbeta(x, shape1=alpha[1], shape2=beta[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Life Distribution Function")
            for (j in 1:len_beta)   ### 파라메터: beta
            {
                lines(x, dbeta(x, shape1=alpha[i], shape2=beta[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("alpha = ", alpha[i], " / beta = ", beta[j], sep=""))
            }
        }
        else if (func == "pbeta")  # 누적분포함수
        {
            plot(x, pbeta(x, shape1=alpha[1], shape2=beta[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Cumulative Distribution Function")
            for (j in 1:len_beta)   ### 파라메터: beta
            {
                lines(x, pbeta(x, shape1=alpha[i], shape2=beta[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("alpha = ", alpha[i], " / beta = ", beta[j], sep=""))
            }
        }
        else if (func == "sbeta")  # 생존함수
        {
            plot(x, sbeta(x, shape1=alpha[1], shape2=beta[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Survival Function")
            for (j in 1:len_beta)   ### 파라메터: beta
            {
                lines(x, sbeta(x, shape1=alpha[i], shape2=beta[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("alpha = ", alpha[i], " / beta = ", beta[j], sep=""))
            }
        }
        else if (func == "hbeta")  # 위험함수
        {
            plot(x, hbeta(x, shape1=alpha[1], shape2=beta[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Hazard Function")
            for (j in 1:len_beta)   ### 파라메터: beta
            {
                lines(x, hbeta(x, shape1=alpha[i], shape2=beta[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("alpha = ", alpha[i], " / beta = ", beta[j], sep=""))
            }
        }
        legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
    }
}

par(mfrow = c(4, 2))
plot.beta_seq(x, alpha, beta, xlim=c(min(x), max(x)), ylim=c(0, 5), func="dbeta")

par(mfrow = c(4, 2))
plot.beta_seq(x, alpha, beta, xlim=c(min(x), max(x)), ylim=c(0, 1), func="pbeta")

par(mfrow = c(4, 2))
plot.beta_seq(x, alpha, beta, xlim=c(min(x), max(x)), ylim=c(0, 1), func="sbeta")

par(mfrow = c(4, 2))
plot.beta_seq(x, alpha, beta, xlim=c(min(x), max(x)), ylim=c(0, 30), func="hbeta")
