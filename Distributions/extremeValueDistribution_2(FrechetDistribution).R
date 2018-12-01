setwd("/home/lv999/Dropbox/Github/SurvivalAnalysis/RCode")
source("colorPalette.R")
require(VGAM)


##### frechet Distribution
### parameter
location = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
shape = c(0.25, 0.5, 0.75, 1, 2, 4, 8)
scale = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x = seq(0, 10, length.out = 1000)


### 수명 분포
dfrechet(x, location = 0, shape = 1, scale = 1)


### 분위수 함수
qfrechet(x, location = 0, shape = 1, scale = 1)


### 난수 함수
rfrechet(x, location = 0, shape = 1, scale = 1)


### 누적분포함수
pfrechet(x, location = 0, shape = 1, scale = 1)


### 생존함수
sfrechet = function (x, location = 1, shape = 1, scale = 1)
{
    fx = 1 - pfrechet(x, location = location, shape = shape, scale = scale)
    return(fx)
}


### 위험함수
hfrechet = function (x, location = 1, shape = 1, scale = 1)
{
    fx = dfrechet(x, location = location, shape = shape, scale = scale) / sfrechet(x, location = location, shape = shape, scale = scale)
    return(fx)
}





##### Plot
plot.frechet_seq = function(x, location = 1, shape = 1, scale = 1, xlim=c(0, 10), ylim=c(0, 5), func="dfrechet")
{
    color=colorPalette(300)

    len_location = length(location)       # location 파라메터의 길이
    len_shape = length(shape)          # shape 파라메터의 길이
    len_scale = length(scale)  # scale 파라메터의 길이
    
    color_counter = 1
    for (i in 1:len_location)  ### 파라메터: location
    {
        if (func=="dfrechet")     # 수명분포
        {
            for (j in 1:len_shape)   ### 파라메터: shape
            {
                color_counter_init = color_counter
                legend_name = NULL;
                plot(x, dfrechet(x, location=location[1], shape=shape[1], scale=scale[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Life Distribution Function")
                for (k in 1:len_scale)   ### 파라메터: scale
                {
                    lines(x, dfrechet(x, location=location[i], shape=shape[j], scale=scale[k]), col=color[color_counter], lwd=2);
                    color_counter = color_counter + 1;
                    legend_name = c(legend_name, paste("location = ", location[i], " / shape = ", shape[j], " / scale = ", scale[k], sep=""))
                }
                legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
            }
        }
        else if (func == "pfrechet")  # 누적분포함수
        {
            for (j in 1:len_shape)   ### 파라메터: shape
            {
                color_counter_init = color_counter
                legend_name = NULL;
                plot(x, pfrechet(x, location=location[1], shape=shape[1], scale=scale[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Culocationlative Distribution Function")
                for (k in 1:len_scale)   ### 파라메터: scale
                {
                    lines(x, pfrechet(x, location=location[i], shape=shape[j], scale=scale[k]), col=color[color_counter], lwd=2);
                    color_counter = color_counter + 1;
                    legend_name = c(legend_name, paste("location = ", location[i], " / shape = ", shape[j], " / scale = ", scale[k], sep=""))
                }
                legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
            }
        }
        else if (func == "sfrechet")  # 생존함수
        {
            for (j in 1:len_shape)   ### 파라메터: shape
            {
                color_counter_init = color_counter
                legend_name = NULL;
                plot(x, sfrechet(x, location=location[1], shape=shape[1], scale=scale[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Survival Function")
                for (k in 1:len_scale)   ### 파라메터: scale
                {
                    lines(x, sfrechet(x, location=location[i], shape=shape[j], scale=scale[k]), col=color[color_counter], lwd=2);
                    color_counter = color_counter + 1;
                    legend_name = c(legend_name, paste("location = ", location[i], " / shape = ", shape[j], " / scale = ", scale[k], sep=""))
                }
                legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
            }
        }
        else if (func == "hfrechet")  # 위험함수
        {
            for (j in 1:len_shape)   ### 파라메터: shape
            {
                color_counter_init = color_counter
                legend_name = NULL;
                plot(x, hfrechet(x, location=location[1], shape=shape[1], scale=scale[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Hazard Function")
                for (k in 1:len_scale)   ### 파라메터: scale
                {
                    lines(x, hfrechet(x, location=location[i], shape=shape[j], scale=scale[k]), col=color[color_counter], lwd=2);
                    color_counter = color_counter + 1;
                    legend_name = c(legend_name, paste("location = ", location[i], " / shape = ", shape[j], " / scale = ", scale[k], sep=""))
                }
                legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
            }
        }
    }
}

par(mfrow = c(8, 8))
plot.frechet_seq(x, location, shape, scale, xlim=c(min(x), max(x)), ylim=c(-10, 10), func="dfrechet")

par(mfrow = c(8, 8))
plot.frechet_seq(x, location, shape, scale, xlim=c(min(x), max(x)), ylim=c(-5, 5), func="pfrechet")

par(mfrow = c(8, 8))
plot.frechet_seq(x, location, shape, scale, xlim=c(min(x), max(x)), ylim=c(-5, 5), func="sfrechet")

par(mfrow = c(8, 8))
plot.frechet_seq(x, location, shape, scale, xlim=c(min(x), max(x)), ylim=c(-30, 30), func="hfrechet")
