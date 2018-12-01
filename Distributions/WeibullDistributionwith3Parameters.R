setwd("/home/lv999/Dropbox/Github/SurvivalAnalysis/RCode")
source("colorPalette.R")


##### weibull Distribution with Location Parameters
### parameter
shape = c(0.25, 0.5, 0.75, 1, 2, 4, 8)
scale = c(0.25, 0.5, 0.75, 1, 2, 4, 8)
location = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x = seq(0, 10, length.out = 1000)


### 수명 분포
dweibull3 = function(x, shape=shape, scale=scale, location=location)
{
    fx = dweibull(x-location, shape=shape, scale=scale)
    return(fx)
}


### 분위수 함수
qweibull3 = function(x, shape=shape, scale=scale, location=location)
{
    fx = qweibull(x-location, shape=shape, scale=scale)
    return(fx)
}


### 난수 함수
rweibull3 = function(x, shape=shape, scale=scale, location=location)
{
    fx = rweibull(x-location, shape=shape, scale=scale)
    return(fx)
}


### 누적분포함수
pweibull3 = function(x, shape=shape, scale=scale, location=location)
{
    fx = pweibull(x-location, shape=shape, scale=scale)
    return(fx)
}


### 생존함수
sweibull3 = function(x, shape=1, scale=1, location=0)
{
    fx = 1 - pweibull3(x, shape=shape, scale=scale, location=location)
    return(fx)
}


### 위험함수
hweibull3 = function (x, shape=shape, scale=scale, location=0)
{
    fx = dweibull3(x, shape=shape, scale=scale, location=location) / sweibull3(x, shape=shape, scale=scale, location=location)
    return(fx)
}





##### Plot
plot.weibull3_seq = function(x, shape = 1, scale = 1, location = 1, xlim=c(0, 10), ylim=c(0, 5), func="dweibull3")
{
    color=colorPalette(300)

    len_location = length(location)       # location 파라메터의 길이
    len_shape = length(shape)          # shape 파라메터의 길이
    len_scale = length(scale)  # scale 파라메터의 길이
    
    color_counter = 1
    for (i in 1:len_location)  ### 파라메터: location
    {
        if (func=="dweibull3")     # 수명분포
        {
            for (j in 1:len_shape)   ### 파라메터: shape
            {
                color_counter_init = color_counter
                legend_name = NULL;
                plot(x, dweibull3(x, location=location[1], shape=shape[1], scale=scale[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Life Distribution Function")
                for (k in 1:len_scale)   ### 파라메터: scale
                {
                    lines(x, dweibull3(x, location=location[i], shape=shape[j], scale=scale[k]), col=color[color_counter], lwd=2);
                    color_counter = color_counter + 1;
                    legend_name = c(legend_name, paste("location = ", location[i], " / shape = ", shape[j], " / scale = ", scale[k], sep=""))
                }
                legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
            }
        }
        else if (func == "pweibull3")  # 누적분포함수
        {
            for (j in 1:len_shape)   ### 파라메터: shape
            {
                color_counter_init = color_counter
                legend_name = NULL;
                plot(x, pweibull3(x, location=location[1], shape=shape[1], scale=scale[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Cumulative Distribution Function")
                for (k in 1:len_scale)   ### 파라메터: scale
                {
                    lines(x, pweibull3(x, location=location[i], shape=shape[j], scale=scale[k]), col=color[color_counter], lwd=2);
                    color_counter = color_counter + 1;
                    legend_name = c(legend_name, paste("location = ", location[i], " / shape = ", shape[j], " / scale = ", scale[k], sep=""))
                }
                legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
            }
        }
        else if (func == "sweibull3")  # 생존함수
        {
            for (j in 1:len_shape)   ### 파라메터: shape
            {
                color_counter_init = color_counter
                legend_name = NULL;
                plot(x, sweibull3(x, location=location[1], shape=shape[1], scale=scale[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Survival Function")
                for (k in 1:len_scale)   ### 파라메터: scale
                {
                    lines(x, sweibull3(x, location=location[i], shape=shape[j], scale=scale[k]), col=color[color_counter], lwd=2);
                    color_counter = color_counter + 1;
                    legend_name = c(legend_name, paste("location = ", location[i], " / shape = ", shape[j], " / scale = ", scale[k], sep=""))
                }
                legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
            }
        }
        else if (func == "hweibull3")  # 위험함수
        {
            for (j in 1:len_shape)   ### 파라메터: shape
            {
                color_counter_init = color_counter
                legend_name = NULL;
                plot(x, hweibull3(x, location=location[1], shape=shape[1], scale=scale[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Hazard Function")
                for (k in 1:len_scale)   ### 파라메터: scale
                {
                    lines(x, hweibull3(x, location=location[i], shape=shape[j], scale=scale[k]), col=color[color_counter], lwd=2);
                    color_counter = color_counter + 1;
                    legend_name = c(legend_name, paste("location = ", location[i], " / shape = ", shape[j], " / scale = ", scale[k], sep=""))
                }
                legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
            }
        }
    }
}

par(mfrow = c(9, 8))
plot.weibull3_seq(x, shape, scale, location, xlim=c(min(x), max(x)), ylim=c(-10, 10), func="dweibull3")

par(mfrow = c(9, 8))
plot.weibull3_seq(x, shape, scale, location, xlim=c(min(x), max(x)), ylim=c(-5, 5), func="pweibull3")

par(mfrow = c(9, 8))
plot.weibull3_seq(x, shape, scale, location, xlim=c(min(x), max(x)), ylim=c(-5, 5), func="sweibull3")

par(mfrow = c(9, 8))
plot.weibull3_seq(x, shape, scale, location, xlim=c(min(x), max(x)), ylim=c(-30, 30), func="hweibull3")