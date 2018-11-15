setwd("/home/lv999/Dropbox/Github/SurvivalAnalysis/RCode")
source("colorPalette.R")


##### 극치 분포: Gumbel 최대값 분포
### parameter
location = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
scale = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x <- seq(-10, 10, length.out = 1000)


### 수명 분포
dgumbel_max = function (x, scale = 1, location = 0) 
{
    fx <- 1/scale * exp(-(x - location)/scale) * exp(-exp(-(x - location)/scale))
    return(fx)
}


### 난수 함수
rgumbel_max = function (x, scale = 1, location = 0) 
{
	qgumbel(runif(n), scale, location)
}


### 누적분포함수
pgumbel_max = function (q, scale = 1, location = 0) 
{
    fx = -(sgumbel_max(x, scale, location) - 1)
    return(fx)
}


### 생존함수
sgumbel_max = function (x, scale = 1, location = 0) 
{
    fx <- 1 - exp(-exp(-(x - location)/scale))
    return(fx)
}


### 위험함수
hgumbel_max = function (x, scale = 1, location = 0) 
{
    fx <- dgumbel_max(x, scale, location) / sgumbel_max(x, scale, location)
    return(fx)
}





##### Plot
plot.gumbel_max_seq = function(x, location = 1, scale = 0, xlim=c(0, 10), ylim=c(0, 5), func="dgumbel_max")
{
    color=colorPalette(300)

    len_location = length(location)       # location 파라메터의 길이
    len_scale = length(scale)          # scale 파라메터의 길이
    
    color_counter = 1
    for (i in 1:len_location)  ### 파라메터: location
    {
        color_counter_init = color_counter
        legend_name = NULL;
        
        if (func=="dgumbel_max")     # 수명분포
        {
            plot(x, dgumbel_max(x, location=location[1], scale=scale[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Life Distribution Function")
            for (j in 1:len_scale)   ### 파라메터: scale
            {
                lines(x, dgumbel_max(x, location=location[i], scale=scale[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("location = ", location[i], " / scale = ", scale[j], sep=""))
            }
        }
        else if (func == "pgumbel_max")  # 누적분포함수
        {
            plot(x, pgumbel_max(x, location=location[1], scale=scale[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Cumulative Distribution Function")
            for (j in 1:len_scale)   ### 파라메터: scale
            {
                lines(x, pgumbel_max(x, location=location[i], scale=scale[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("location = ", location[i], " / scale = ", scale[j], sep=""))
            }
        }
        else if (func == "sgumbel_max")  # 생존함수
        {
            plot(x, sgumbel_max(x, location=location[1], scale=scale[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Survival Function")
            for (j in 1:len_scale)   ### 파라메터: scale
            {
                lines(x, sgumbel_max(x, location=location[i], scale=scale[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("location = ", location[i], " / scale = ", scale[j], sep=""))
            }
        }
        else if (func == "hgumbel_max")  # 위험함수
        {
            plot(x, hgumbel_max(x, location=location[1], scale=scale[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Hazard Function")
            for (j in 1:len_scale)   ### 파라메터: scale
            {
                lines(x, hgumbel_max(x, location=location[i], scale=scale[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("location = ", location[i], " / scale = ", scale[j], sep=""))
            }
        }
        legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
    }
}

par(mfrow = c(3, 3))
plot.gumbel_max_seq(x, location, scale, xlim=c(min(x), max(x)), ylim=c(0, 1), func="dgumbel_max")

par(mfrow = c(3, 3))
plot.gumbel_max_seq(x, location, scale, xlim=c(min(x), max(x)), ylim=c(0, 1), func="pgumbel_max")

par(mfrow = c(3, 3))
plot.gumbel_max_seq(x, location, scale, xlim=c(min(x), max(x)), ylim=c(0, 1), func="sgumbel_max")

par(mfrow = c(3, 3))
plot.gumbel_max_seq(x, location, scale, xlim=c(min(x), max(x)), ylim=c(0, 5), func="hgumbel_max")
