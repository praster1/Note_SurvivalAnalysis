
source("colorPalette.R")
require(VGAM)


##### makeham Distribution
### parameter
theta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x = seq(0.1, 10, length.out = 1000)


### 수명 분포
dmakeham(x, shape = theta)


### 분위수 함수
qmakeham(x, shape = theta)


### 난수 함수
rmakeham(x, shape = theta)


### 누적분포함수
pmakeham(x, shape = theta)


### 생존함수
smakeham = function (x, shape = 1)
{
    fx = 1 - pmakeham(x, shape = theta)
    return(fx)
}


### 위험함수
hmakeham = function (x, shape = 1)
{
    fx = dmakeham(x, shape = theta) / smakeham(x, shape = theta)
    return(fx)
}





##### Plot
plot.makeham_seq = function(x, shape = 1, xlim=c(0, 10), ylim=c(0, 5), func="dmakeham")
{
    color=colorPalette(300)

    len_theta = length(theta)       # theta 파라메터의 길이
    
    color_counter = 1
    color_counter_init = color_counter
    legend_name = NULL;


    if (func=="dmakeham")     # 수명분포
    {    
        plot(x, dmakeham(x, shape=theta[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Life Distribution Function")
        for (i in 1:len_theta)  ### 파라메터: theta
        {
            lines(x, dmakeham(x, shape=theta[i]), col=color[color_counter], lwd=2);
            color_counter = color_counter + 1;
            legend_name = c(legend_name, paste("shape = ", theta[i], sep=""))
        }
    }
    else if (func == "pmakeham")  # 누적분포함수
    {
        plot(x, pmakeham(x, shape=theta[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Cumulative Distribution Function")
        for (i in 1:len_theta)  ### 파라메터: theta
        {
            lines(x, pmakeham(x, shape=theta[i]), col=color[color_counter], lwd=2);
            color_counter = color_counter + 1;
            legend_name = c(legend_name, paste("shape = ", theta[i], sep=""))
        }
    }
    else if (func == "smakeham")  # 생존함수
    {
        plot(x, smakeham(x, shape=theta[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Survival Function")
        for (i in 1:len_theta)  ### 파라메터: theta
        {
            lines(x, smakeham(x, shape=theta[i]), col=color[color_counter], lwd=2);
            color_counter = color_counter + 1;
            legend_name = c(legend_name, paste("shape = ", theta[i], sep=""))
        }
    }
    else if (func == "hmakeham")  # 위험함수
    {
        plot(x, hmakeham(x, shape=theta[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Hazard Function")
        for (i in 1:len_theta)  ### 파라메터: theta
        {
            lines(x, hmakeham(x, shape=theta[i]), col=color[color_counter], lwd=2);
            color_counter = color_counter + 1;
            legend_name = c(legend_name, paste("shape = ", theta[i], sep=""))
        }
    }
    legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
}

par(mfrow = c(2, 2))
plot.makeham_seq(x, theta, xlim=c(min(x), max(x)), ylim=c(0, 2), func="dmakeham")
plot.makeham_seq(x, theta, xlim=c(min(x), max(x)), ylim=c(0, 1), func="pmakeham")
plot.makeham_seq(x, theta, xlim=c(min(x), max(x)), ylim=c(0, 1), func="smakeham")
plot.makeham_seq(x, theta, xlim=c(min(x), max(x)), ylim=c(0, 50), func="hmakeham")


