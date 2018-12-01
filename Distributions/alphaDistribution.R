setwd("/home/lv999/Dropbox/Github/SurvivalAnalysis/RCode")
source("colorPalette.R")


##### Alpha Distribution
### parameter
alpha = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
beta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x = seq(0, 10, length.out = 1000)


### 수명 분포
dalpha = function(x, alpha = 1, beta = 0) 
{
    if (sum((beta <= 0) * 1) > 0)     {        stop("beta is not positive.")    }        # beta > 0 이어야 한다.
    if (sum((x < 0) * 1) > 0)           {        stop("x is not positive or 0.")    }       # x >= 0 이어야 한다.
    
    fx = (beta * exp(-(1/2) *(alpha - beta / x)^2)) / (sqrt(2 * pi) * pnorm(alpha) * x^2)
    return(fx)
}


### 누적분포함수
palpha = function (x, alpha = 1, beta = 0) 
{
    if (sum((beta <= 0) * 1) > 0)     {        stop("beta is not positive.")    }        # beta > 0 이어야 한다.
    if (sum((x < 0) * 1) > 0)           {        stop("x is not positive or 0.")    }       # x >= 0 이어야 한다.
    
    fx = pnorm(alpha - beta/x) / pnorm(alpha)
    return(fx)
}


### 생존함수
salpha = function (x, alpha = 1, beta = 0) 
{
    if (sum((beta <= 0) * 1) > 0)     {        stop("beta is not positive.")    }        # beta > 0 이어야 한다.
    if (sum((x < 0) * 1) > 0)           {        stop("x is not positive or 0.")    }       # x >= 0 이어야 한다.
    
    fx = 1 - (pnorm(alpha - beta/x) / pnorm(alpha))
    return(fx)
}


### 위험함수
halpha = function (x, alpha = 1, beta = 0) 
{
    if (sum((beta <= 0) * 1) > 0)     {        stop("beta is not positive.")    }        # beta > 0 이어야 한다.
    if (sum((x < 0) * 1) > 0)           {        stop("x is not positive or 0.")    }       # x >= 0 이어야 한다.
    
    fx = dalpha(x, alpha, beta) / salpha(x, alpha, beta)
    return(fx)
}





##### Plot
plot.alpha_seq = function(x, alpha = 1, beta = 0, xlim=c(0, 10), ylim=c(0, 5), func="dalpha")
{
    color=colorPalette(300)

    len_alpha = length(alpha)       # alpha 파라메터의 길이
    len_beta = length(beta)          # beta 파라메터의 길이
    
    color_counter = 1
    for (i in 1:len_alpha)  ### 파라메터: alpha
    {
        color_counter_init = color_counter
        legend_name = NULL;
        
        if (func=="dalpha")     # 수명분포
        {
            plot(x, dalpha(x, alpha=alpha[1], beta=beta[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Life Distribution Function")
            for (j in 1:len_beta)   ### 파라메터: beta
            {
                lines(x, dalpha(x, alpha=alpha[i], beta=beta[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("alpha = ", alpha[i], " / beta = ", beta[j], sep=""))
            }
        }
        else if (func == "palpha")  # 누적분포함수
        {
            plot(x, palpha(x, alpha=alpha[1], beta=beta[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Cumulative Distribution Function")
            for (j in 1:len_beta)   ### 파라메터: beta
            {
                lines(x, palpha(x, alpha=alpha[i], beta=beta[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("alpha = ", alpha[i], " / beta = ", beta[j], sep=""))
            }
        }
        else if (func == "salpha")  # 생존함수
        {
            plot(x, salpha(x, alpha=alpha[1], beta=beta[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Survival Function")
            for (j in 1:len_beta)   ### 파라메터: beta
            {
                lines(x, salpha(x, alpha=alpha[i], beta=beta[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("alpha = ", alpha[i], " / beta = ", beta[j], sep=""))
            }
        }
        else if (func == "halpha")  # 위험함수
        {
            plot(x, halpha(x, alpha=alpha[1], beta=beta[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Hazard Function")
            for (j in 1:len_beta)   ### 파라메터: beta
            {
                lines(x, halpha(x, alpha=alpha[i], beta=beta[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("alpha = ", alpha[i], " / beta = ", beta[j], sep=""))
            }
        }
        legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
    }
}

par(mfrow = c(3, 3))
plot.alpha_seq(x, alpha, beta, xlim=c(min(x), max(x)), ylim=c(0, 5), func="dalpha")

par(mfrow = c(3, 3))
plot.alpha_seq(x, alpha, beta, xlim=c(min(x), max(x)), ylim=c(0, 1), func="palpha")

par(mfrow = c(3, 3))
plot.alpha_seq(x, alpha, beta, xlim=c(min(x), max(x)), ylim=c(0, 1), func="salpha")

par(mfrow = c(3, 3))
plot.alpha_seq(x, alpha, beta, xlim=c(min(x), max(x)), ylim=c(0, 6), func="halpha")
