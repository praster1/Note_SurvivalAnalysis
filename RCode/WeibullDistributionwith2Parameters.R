setwd("/home/lv999/Dropbox/Github/SurvivalAnalysis/RCode")
source("colorPalette.R")


##### 극치 분포: Gumbel 최대값 분포
### parameter
shape = c(0.25, 0.5, 0.75, 1, 2, 4, 8)
scale = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x = seq(0, 10, length.out = 1000)


### 수명 분포
dweibull(x, shape = 1, scale = 1)


### 분위수 함수
qweibull(x, shape = 1, scale = 1)


### 난수 함수
rweibull(x, shape = 1, scale = 1)


### 누적분포함수
pweibull(x, shape = 1, scale = 1)


### 생존함수
sweibull = function (x, shape = 1, scale = 1)
{
    fx = 1 - pweibull(x, shape = shape, scale = scale)
    return(fx)
}


### 위험함수
hweibull = function (x, shape = 1, scale = 0)
{
    fx = dweibull(x, shape = shape, scale = scale) / sweibull(x, shape = shape, scale = scale)
    return(fx)
}



### 위험함수
hweibull = function (x, scale = 1, shape = 0) 
{
    fx = dweibull(x, shape = shape, scale = scale) / sweibull(x, shape = shape, scale = scale)
    return(fx)
}





##### Plot
plot.weibull_seq = function(x, shape = 1, scale = 0, xlim=c(0, 10), ylim=c(0, 5), func="dweibull")
{
    color=colorPalette(300)

    len_shape = length(shape)       # shape 파라메터의 길이
    len_scale = length(scale)          # scale 파라메터의 길이
    
    color_counter = 1
    for (i in 1:len_shape)  ### 파라메터: shape
    {
        color_counter_init = color_counter
        legend_name = NULL;
        
        if (func=="dweibull")     # 수명분포
        {
            plot(x, dweibull(x, shape=shape[1], scale=scale[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Life Distribution Function")
            for (j in 1:len_scale)   ### 파라메터: scale
            {
                lines(x, dweibull(x, shape=shape[i], scale=scale[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("shape = ", shape[i], " / scale = ", scale[j], sep=""))
            }
        }
        else if (func == "pweibull")  # 누적분포함수
        {
            plot(x, pweibull(x, shape=shape[1], scale=scale[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Cumulative Distribution Function")
            for (j in 1:len_scale)   ### 파라메터: scale
            {
                lines(x, pweibull(x, shape=shape[i], scale=scale[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("shape = ", shape[i], " / scale = ", scale[j], sep=""))
            }
        }
        else if (func == "sweibull")  # 생존함수
        {
            plot(x, sweibull(x, shape=shape[1], scale=scale[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Survival Function")
            for (j in 1:len_scale)   ### 파라메터: scale
            {
                lines(x, sweibull(x, shape=shape[i], scale=scale[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("shape = ", shape[i], " / scale = ", scale[j], sep=""))
            }
        }
        else if (func == "hweibull")  # 위험함수
        {
            plot(x, hweibull(x, shape=shape[1], scale=scale[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Hazard Function")
            for (j in 1:len_scale)   ### 파라메터: scale
            {
                lines(x, hweibull(x, shape=shape[i], scale=scale[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("shape = ", shape[i], " / scale = ", scale[j], sep=""))
            }
        }
        legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
    }
}

par(mfrow = c(3, 3))
plot.weibull_seq(x, shape, scale, xlim=c(min(x), max(x)), ylim=c(0, 1), func="dweibull")

par(mfrow = c(3, 3))
plot.weibull_seq(x, shape, scale, xlim=c(min(x), max(x)), ylim=c(0, 1), func="pweibull")

par(mfrow = c(3, 3))
plot.weibull_seq(x, shape, scale, xlim=c(min(x), max(x)), ylim=c(0, 1), func="sweibull")

par(mfrow = c(3, 3))
plot.weibull_seq(x, shape, scale, xlim=c(min(x), max(x)), ylim=c(0, 5), func="hweibull")


