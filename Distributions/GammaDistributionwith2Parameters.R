setwd("/home/lv999/Dropbox/Github/SurvivalAnalysis/RCode")
source("colorPalette.R")


##### Gamma Distribution
### parameter
shape = c(0.25, 0.5, 0.75, 1, 2, 4, 8)
scale = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x = seq(0, 10, length.out = 1000)


### 수명 분포
dgamma2 = function(x, shape=shape, scale=scale)
{
    fx = dgamma(x, shape=shape, scale=scale)
    return(fx)
}


### 분위수 함수
qgamma2 = function(x, shape=shape, scale=scale)
{
    fx = qgamma(x, shape=shape, scale=scale)
    return(fx)
}


### 난수 함수
rgamma2 = function(x, shape=shape, scale=scale)
{
    fx = rgamma(x, shape=shape, scale=scale)
    return(fx)
}


### 누적분포함수
pgamma2 = function(x, shape=shape, scale=scale)
{
    fx = pgamma(x, shape=shape, scale=scale)
    return(fx)
}


### 생존함수
sgamma2 = function(x, shape=1, scale=1)
{
    fx = 1 - pgamma2(x, shape=shape, scale=scale)
    return(fx)
}


### 위험함수
hgamma2 = function (x, shape=shape, scale=scale)
{
    fx = dgamma2(x, shape=shape, scale=scale) / sgamma2(x, shape=shape, scale=scale)
    return(fx)
}





##### Plot
plot.gamma2_seq = function(x, shape = 1, scale = 1, xlim=c(0, 10), ylim=c(0, 5), func="dgamma2")
{
    color=colorPalette(300)

    len_shape = length(shape)       # shape 파라메터의 길이
    len_scale = length(scale)          # scale 파라메터의 길이
    
    color_counter = 1
    for (i in 1:len_shape)  ### 파라메터: shape
    {
        color_counter_init = color_counter
        legend_name = NULL;
        
        if (func=="dgamma2")     # 수명분포
        {
            plot(x, dgamma2(x, shape=shape[1], scale=scale[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Life Distribution Function")
            for (j in 1:len_scale)   ### 파라메터: scale
            {
                lines(x, dgamma2(x, shape=shape[i], scale=scale[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("shape = ", shape[i], " / scale = ", scale[j], sep=""))
            }
        }
        else if (func == "pgamma2")  # 누적분포함수
        {
            plot(x, pgamma2(x, shape=shape[1], scale=scale[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Cumulative Distribution Function")
            for (j in 1:len_scale)   ### 파라메터: scale
            {
                lines(x, pgamma2(x, shape=shape[i], scale=scale[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("shape = ", shape[i], " / scale = ", scale[j], sep=""))
            }
        }
        else if (func == "sgamma2")  # 생존함수
        {
            plot(x, sgamma2(x, shape=shape[1], scale=scale[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Survival Function")
            for (j in 1:len_scale)   ### 파라메터: scale
            {
                lines(x, sgamma2(x, shape=shape[i], scale=scale[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("shape = ", shape[i], " / scale = ", scale[j], sep=""))
            }
        }
        else if (func == "hgamma2")  # 위험함수
        {
            plot(x, hgamma2(x, shape=shape[1], scale=scale[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Hazard Function")
            for (j in 1:len_scale)   ### 파라메터: scale
            {
                lines(x, hgamma2(x, shape=shape[i], scale=scale[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("shape = ", shape[i], " / scale = ", scale[j], sep=""))
            }
        }
        legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
    }
}

par(mfrow = c(3, 3))
plot.gamma2_seq(x, shape, scale, xlim=c(min(x), max(x)), ylim=c(0, 1), func="dgamma2")

par(mfrow = c(3, 3))
plot.gamma2_seq(x, shape, scale, xlim=c(min(x), max(x)), ylim=c(0, 1), func="pgamma2")

par(mfrow = c(3, 3))
plot.gamma2_seq(x, shape, scale, xlim=c(min(x), max(x)), ylim=c(0, 1), func="sgamma2")

par(mfrow = c(3, 3))
plot.gamma2_seq(x, shape, scale, xlim=c(min(x), max(x)), ylim=c(0, 10), func="hgamma2")