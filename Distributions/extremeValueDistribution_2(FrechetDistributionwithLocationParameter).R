setwd("/home/lv999/Dropbox/Github/SurvivalAnalysis/Distributions")
source("colorPalette.R")
require(VGAM)


##### lfrechet Distribution
### parameter
location = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
shape = c(0.25, 0.5, 0.75, 1, 2, 4, 8)
scale = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x = seq(0, 10, length.out = 1000)

# m : shape / c
# beta : scale / b
# mu : location / a
### 수명 분포
dlfrechet = function(x, location = 0, shape = 1, scale = 1)
{
    fx = (shape/scale) * ((location - x) / scale)^(-(shape+1)) * exp(-((location - x) / scale)^(-shape))
}


### 누적분포함수
plfrechet = function(x, location = 0, shape = 1, scale = 1)
{
    fx = -(slfrechet(x, alpha, beta) - 1)
}


### 생존함수
slfrechet = function (x, location = 1, shape = 1, scale = 1)
{
    fx = exp(-((location - x) / scale)^(-shape))
    return(fx)
}


### 위험함수
hlfrechet = function (x, location = 1, shape = 1, scale = 1)
{
    fx = dlfrechet(x, location = location, shape = shape, scale = scale) / slfrechet(x, location = location, shape = shape, scale = scale)
    return(fx)
}





##### Plot
plot.lfrechet_seq = function(x, location = 1, shape = 1, scale = 1, xlim=c(0, 10), ylim=c(0, 5), func="dlfrechet")
{
    color=colorPalette(300)

    len_location = length(location)       # location 파라메터의 길이
    len_shape = length(shape)          # shape 파라메터의 길이
    len_scale = length(scale)  # scale 파라메터의 길이
    
    color_counter = 1
    for (i in 1:len_location)  ### 파라메터: location
    {
        if (func=="dlfrechet")     # 수명분포
        {
            for (j in 1:len_shape)   ### 파라메터: shape
            {
                color_counter_init = color_counter
                legend_name = NULL;
                plot(x, dlfrechet(x, location=location[1], shape=shape[1], scale=scale[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Life Distribution Function")
                for (k in 1:len_scale)   ### 파라메터: scale
                {
                    lines(x, dlfrechet(x, location=location[i], shape=shape[j], scale=scale[k]), col=color[color_counter], lwd=2);
                    color_counter = color_counter + 1;
                    legend_name = c(legend_name, paste("location = ", location[i], " / shape = ", shape[j], " / scale = ", scale[k], sep=""))
                }
                legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
            }
        }
        else if (func == "plfrechet")  # 누적분포함수
        {
            for (j in 1:len_shape)   ### 파라메터: shape
            {
                color_counter_init = color_counter
                legend_name = NULL;
                plot(x, plfrechet(x, location=location[1], shape=shape[1], scale=scale[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Culocationlative Distribution Function")
                for (k in 1:len_scale)   ### 파라메터: scale
                {
                    lines(x, plfrechet(x, location=location[i], shape=shape[j], scale=scale[k]), col=color[color_counter], lwd=2);
                    color_counter = color_counter + 1;
                    legend_name = c(legend_name, paste("location = ", location[i], " / shape = ", shape[j], " / scale = ", scale[k], sep=""))
                }
                legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
            }
        }
        else if (func == "slfrechet")  # 생존함수
        {
            for (j in 1:len_shape)   ### 파라메터: shape
            {
                color_counter_init = color_counter
                legend_name = NULL;
                plot(x, slfrechet(x, location=location[1], shape=shape[1], scale=scale[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Survival Function")
                for (k in 1:len_scale)   ### 파라메터: scale
                {
                    lines(x, slfrechet(x, location=location[i], shape=shape[j], scale=scale[k]), col=color[color_counter], lwd=2);
                    color_counter = color_counter + 1;
                    legend_name = c(legend_name, paste("location = ", location[i], " / shape = ", shape[j], " / scale = ", scale[k], sep=""))
                }
                legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
            }
        }
        else if (func == "hlfrechet")  # 위험함수
        {
            for (j in 1:len_shape)   ### 파라메터: shape
            {
                color_counter_init = color_counter
                legend_name = NULL;
                plot(x, hlfrechet(x, location=location[1], shape=shape[1], scale=scale[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Hazard Function")
                for (k in 1:len_scale)   ### 파라메터: scale
                {
                    lines(x, hlfrechet(x, location=location[i], shape=shape[j], scale=scale[k]), col=color[color_counter], lwd=2);
                    color_counter = color_counter + 1;
                    legend_name = c(legend_name, paste("location = ", location[i], " / shape = ", shape[j], " / scale = ", scale[k], sep=""))
                }
                legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
            }
        }
    }
}

par(mfrow = c(8, 8))
plot.lfrechet_seq(x, location, shape, scale, xlim=c(min(x), max(x)), ylim=c(-10, 10), func="dlfrechet")

par(mfrow = c(8, 8))
plot.lfrechet_seq(x, location, shape, scale, xlim=c(min(x), max(x)), ylim=c(-5, 5), func="plfrechet")

par(mfrow = c(8, 8))
plot.lfrechet_seq(x, location, shape, scale, xlim=c(min(x), max(x)), ylim=c(-5, 5), func="slfrechet")

par(mfrow = c(8, 8))
plot.lfrechet_seq(x, location, shape, scale, xlim=c(min(x), max(x)), ylim=c(-30, 30), func="hlfrechet")
