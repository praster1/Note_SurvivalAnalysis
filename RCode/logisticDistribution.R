setwd("/home/lv999/Dropbox/Github/SurvivalAnalysis/RCode")
source("colorPalette.R")


##### Logistic Distribution
### parameter
location = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
scale = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x <- seq(-10, 10, length.out = 1000)


### 수명 분포
dlogis(x, location, scale)


### 분위수 함수
qlogis(x, location, scale)


### 난수 함수
rlogis(x, location, scale)


### 누적분포함수
plogis(x, location, scale)


### 생존함수
slogis = function (x, location = 1, scale = 0) 
{
    fx <- 1 - plogis(x, location, scale)
    return(fx)
}


### 위험함수
hlogis = function (x, location = 1, scale = 0)
{
    fx <- dlogis(x, location, scale) / slogis(x, location, scale)
    return(fx)
}





##### Plot
plot.logis_seq = function(x, location = 1, scale = 0, xlim=c(0, 10), ylim=c(0, 5), func="dlogis")
{
    color=colorPalette(300)

    len_location = length(location)       # location 파라메터의 길이
    len_scale = length(scale)          # scale 파라메터의 길이
    
    color_counter = 1
    for (i in 1:len_location)  ### 파라메터: location
    {
        color_counter_init = color_counter
        legend_name = NULL;
        
        if (func=="dlogis")     # 수명분포
        {
            plot(x, dlogis(x, location=location[1], scale=scale[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Life Distribution Function")
            for (j in 1:len_scale)   ### 파라메터: scale
            {
                lines(x, dlogis(x, location=location[i], scale=scale[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("location = ", location[i], " / scale = ", scale[j], sep=""))
            }
        }
        else if (func == "plogis")  # 누적분포함수
        {
            plot(x, plogis(x, location=location[1], scale=scale[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Cumulative Distribution Function")
            for (j in 1:len_scale)   ### 파라메터: scale
            {
                lines(x, plogis(x, location=location[i], scale=scale[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("location = ", location[i], " / scale = ", scale[j], sep=""))
            }
        }
        else if (func == "slogis")  # 생존함수
        {
            plot(x, slogis(x, location=location[1], scale=scale[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Survival Function")
            for (j in 1:len_scale)   ### 파라메터: scale
            {
                lines(x, slogis(x, location=location[i], scale=scale[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("location = ", location[i], " / scale = ", scale[j], sep=""))
            }
        }
        else if (func == "hlogis")  # 위험함수
        {
            plot(x, hlogis(x, location=location[1], scale=scale[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Hazard Function")
            for (j in 1:len_scale)   ### 파라메터: scale
            {
                lines(x, hlogis(x, location=location[i], scale=scale[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("location = ", location[i], " / scale = ", scale[j], sep=""))
            }
        }
        legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
    }
}

par(mfrow = c(3, 3))
plot.logis_seq(x, location, scale, xlim=c(min(x), max(x)), ylim=c(0, 1), func="dlogis")

par(mfrow = c(3, 3))
plot.logis_seq(x, location, scale, xlim=c(min(x), max(x)), ylim=c(0, 1), func="plogis")

par(mfrow = c(3, 3))
plot.logis_seq(x, location, scale, xlim=c(min(x), max(x)), ylim=c(0, 1), func="slogis")

par(mfrow = c(3, 3))
plot.logis_seq(x, location, scale, xlim=c(min(x), max(x)), ylim=c(0, 5), func="hlogis")
