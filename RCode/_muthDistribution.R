setwd("/home/lv999/Dropbox/Github/SurvivalAnalysis/RCode")
source("colorPalette.R")


##### Gamma Distribution with Location Parameters
### parameter
location = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
shape = c(0.25, 0.5, 0.75, 1, 2, 4, 8)
scale = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x <- seq(0, 10, length.out = 1000)


### 수명 분포
dmuth = function(x, shape=shape, scale=scale, location=location)
{
    fx = dgamma(x-location, shape=shape, scale=scale)
    return(fx)
}


### 분위수 함수
qmuth = function(x, shape=shape, scale=scale, location=location)
{
    fx = qgamma(x-location, shape=shape, scale=scale)
    return(fx)
}


### 난수 함수
rmuth = function(x, shape=shape, scale=scale, location=location)
{
    fx = rgamma(x-location, shape=shape, scale=scale)
    return(fx)
}


### 누적분포함수
pmuth = function(x, shape=shape, scale=scale, location=location)
{
    fx = pgamma(x-location, shape=shape, scale=scale)
    return(fx)
}


### 생존함수
smuth = function(x, shape=1, scale=1, location=0)
{
    fx <- 1 - pmuth(x, shape=shape, scale=scale, location=location)
    return(fx)
}


### 위험함수
hmuth = function (x, shape=shape, scale=scale, location=0)
{
    fx <- dmuth(x, shape=shape, scale=scale, location=location) / smuth(x, shape=shape, scale=scale, location=location)
    return(fx)
}





##### Plot
plot.muth_seq = function(x, shape = 1, scale = 1, location = 1, xlim=c(0, 10), ylim=c(0, 5), func="dmuth")
{
    color=colorPalette(300)

    len_location = length(location)       # location 파라메터의 길이
    len_shape = length(shape)          # shape 파라메터의 길이
    len_scale = length(scale)  # scale 파라메터의 길이
    
    color_counter = 1
    for (i in 1:len_location)  ### 파라메터: location
    {
        if (func=="dmuth")     # 수명분포
        {
            for (j in 1:len_shape)   ### 파라메터: shape
            {
                color_counter_init = color_counter
                legend_name = NULL;
                plot(x, dmuth(x, location=location[1], shape=shape[1], scale=scale[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Life Distribution Function")
                for (k in 1:len_scale)   ### 파라메터: scale
                {
                    lines(x, dmuth(x, location=location[i], shape=shape[j], scale=scale[k]), col=color[color_counter], lwd=2);
                    color_counter = color_counter + 1;
                    legend_name = c(legend_name, paste("location = ", location[i], " / shape = ", shape[j], " / scale = ", scale[k], sep=""))
                }
                legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
            }
        }
        else if (func == "pmuth")  # 누적분포함수
        {
            for (j in 1:len_shape)   ### 파라메터: shape
            {
                color_counter_init = color_counter
                legend_name = NULL;
                plot(x, pmuth(x, location=location[1], shape=shape[1], scale=scale[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Cumulative Distribution Function")
                for (k in 1:len_scale)   ### 파라메터: scale
                {
                    lines(x, pmuth(x, location=location[i], shape=shape[j], scale=scale[k]), col=color[color_counter], lwd=2);
                    color_counter = color_counter + 1;
                    legend_name = c(legend_name, paste("location = ", location[i], " / shape = ", shape[j], " / scale = ", scale[k], sep=""))
                }
                legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
            }
        }
        else if (func == "smuth")  # 생존함수
        {
            for (j in 1:len_shape)   ### 파라메터: shape
            {
                color_counter_init = color_counter
                legend_name = NULL;
                plot(x, smuth(x, location=location[1], shape=shape[1], scale=scale[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Survival Function")
                for (k in 1:len_scale)   ### 파라메터: scale
                {
                    lines(x, smuth(x, location=location[i], shape=shape[j], scale=scale[k]), col=color[color_counter], lwd=2);
                    color_counter = color_counter + 1;
                    legend_name = c(legend_name, paste("location = ", location[i], " / shape = ", shape[j], " / scale = ", scale[k], sep=""))
                }
                legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
            }
        }
        else if (func == "hmuth")  # 위험함수
        {
            for (j in 1:len_shape)   ### 파라메터: shape
            {
                color_counter_init = color_counter
                legend_name = NULL;
                plot(x, hmuth(x, location=location[1], shape=shape[1], scale=scale[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Hazard Function")
                for (k in 1:len_scale)   ### 파라메터: scale
                {
                    lines(x, hmuth(x, location=location[i], shape=shape[j], scale=scale[k]), col=color[color_counter], lwd=2);
                    color_counter = color_counter + 1;
                    legend_name = c(legend_name, paste("location = ", location[i], " / shape = ", shape[j], " / scale = ", scale[k], sep=""))
                }
                legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
            }
        }
    }
}

par(mfrow = c(9, 8))
plot.muth_seq(x, shape, scale, location, xlim=c(min(x), max(x)), ylim=c(-10, 10), func="dmuth")

par(mfrow = c(9, 8))
plot.muth_seq(x, shape, scale, location, xlim=c(min(x), max(x)), ylim=c(-5, 5), func="pmuth")

par(mfrow = c(9, 8))
plot.muth_seq(x, shape, scale, location, xlim=c(min(x), max(x)), ylim=c(-5, 5), func="smuth")

par(mfrow = c(9, 8))
plot.muth_seq(x, shape, scale, location, xlim=c(min(x), max(x)), ylim=c(-30, 30), func="hmuth")