setwd("/home/lv999/Dropbox/Github/SurvivalAnalysis/RCode")
source("colorPalette.R")


##### Rayleigh Distribution
### parameter
scale = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x = seq(0, 10, length.out = 1000)


### 수명 분포
drayleigh(x, scale = scale)


### 분위수 함수
qrayleigh(x, scale = scale)


### 난수 함수
rrayleigh(x, scale = scale)


### 누적분포함수
prayleigh(x, scale = scale)


### 생존함수
srayleigh = function (x, scale = 1)
{
    fx = 1 - prayleigh(x, scale = scale)
    return(fx)
}


### 위험함수
hrayleigh = function (x, scale = 1)
{
    fx = drayleigh(x, scale = scale) / srayleigh(x, scale = scale)
    return(fx)
}





##### Plot
plot.rayleigh_seq = function(x, scale = 1, xlim=c(0, 10), ylim=c(0, 5), func="drayleigh")
{
    color=colorPalette(300)

    len_scale = length(scale)       # scale 파라메터의 길이
    
    color_counter = 1
    color_counter_init = color_counter
    legend_name = NULL;


    if (func=="drayleigh")     # 수명분포
    {    
        plot(x, drayleigh(x, scale=scale[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Life Distribution Function")
        for (i in 1:len_scale)  ### 파라메터: scale
        {
            lines(x, drayleigh(x, scale=scale[i]), col=color[color_counter], lwd=2);
            color_counter = color_counter + 1;
            legend_name = c(legend_name, paste("scale = ", scale[i], sep=""))
        }
    }
    else if (func == "prayleigh")  # 누적분포함수
    {
        plot(x, prayleigh(x, scale=scale[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Cumulative Distribution Function")
        for (i in 1:len_scale)  ### 파라메터: scale
        {
            lines(x, prayleigh(x, scale=scale[i]), col=color[color_counter], lwd=2);
            color_counter = color_counter + 1;
            legend_name = c(legend_name, paste("scale = ", scale[i], sep=""))
        }
    }
    else if (func == "srayleigh")  # 생존함수
    {
        plot(x, srayleigh(x, scale=scale[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Survival Function")
        for (i in 1:len_scale)  ### 파라메터: scale
        {
            lines(x, srayleigh(x, scale=scale[i]), col=color[color_counter], lwd=2);
            color_counter = color_counter + 1;
            legend_name = c(legend_name, paste("scale = ", scale[i], sep=""))
        }
    }
    else if (func == "hrayleigh")  # 위험함수
    {
        plot(x, hrayleigh(x, scale=scale[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Hazard Function")
        for (i in 1:len_scale)  ### 파라메터: scale
        {
            lines(x, hrayleigh(x, scale=scale[i]), col=color[color_counter], lwd=2);
            color_counter = color_counter + 1;
            legend_name = c(legend_name, paste("scale = ", scale[i], sep=""))
        }
    }
    legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
}

par(mfrow = c(2, 2))
plot.rayleigh_seq(x, scale, xlim=c(min(x), max(x)), ylim=c(0, 2), func="drayleigh")
plot.rayleigh_seq(x, scale, xlim=c(min(x), max(x)), ylim=c(0, 1), func="prayleigh")
plot.rayleigh_seq(x, scale, xlim=c(min(x), max(x)), ylim=c(0, 1), func="srayleigh")
plot.rayleigh_seq(x, scale, xlim=c(min(x), max(x)), ylim=c(0, 2), func="hrayleigh")


##### 레일리 분포
library(VGAM) 

### parameter
k_p = c(0.5, 1, 2, 4, 8)	# k_p

### Input Variable
x = seq(0, 10, length.out = 101)


color = rainbow(10)
par(mfrow = c(2, 2))

### Life Distribution
plot(x, drayleigh(x, k_p[1]), xlim=c(0, 10), ylim=c(0, 1.5), col=color[1], lwd=2, type = 'l', main="Life Distribution")
for (i in 2:5)	{	lines(x, drayleigh(x, k_p[i]), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('k_p = 0.5', 'k_p = 1', 'k_p = 2', 'k_p = 4', 'k_p = 8'))

### Cumulative Distribution
plot(x, prayleigh(x, k_p[1]), xlim=c(0, 10), ylim=c(0, 1), col=color[1], lwd=2, type = 'l', main="Cumulative Distribution")
for (i in 2:5)	{	lines(x, prayleigh(x, k_p[i]), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('k_p = 0.5', 'k_p = 1', 'k_p = 2', 'k_p = 4', 'k_p = 8'))

### Survival Function
plot(x, 1-prayleigh(x, k_p[1]), xlim=c(0, 10), ylim=c(0, 1), col=color[1], lwd=2, type = 'l', main="Survival Function")
for (i in 2:5)	{	lines(x, 1-prayleigh(x, k_p[i]), col=color[i], lwd=2);	}
legend('right', bty = 'n', lwd=2, col=color[1:5], legend = c('k_p = 0.5', 'k_p = 1', 'k_p = 2', 'k_p = 4', 'k_p = 8'))

### Hazard Function
plot(x, drayleigh(x, k_p[1])/(1-prayleigh(x, k_p[1])), xlim=c(0, 10), ylim=c(0, 10), col=color[1], lwd=2, type = 'l', main="Hazard Function")
for (i in 2:5) {	lines(x, drayleigh(x, k_p[i])/(1-prayleigh(x, k_p[i])), col=color[i], lwd=2);	}
legend('topright', bty = 'n', lwd=2, col=color[1:5], legend = c('k_p = 0.5', 'k_p = 1', 'k_p = 2', 'k_p = 4', 'k_p = 8'))