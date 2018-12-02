
source("colorPalette.R")


##### 극치 분포: Gumbel 최소값 분포
### parameter
location = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
scale = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x = seq(-10, 10, length.out = 1000)


### 수명 분포
dgumbel_min = function (x, scale = 1, location = 0) 
{
    fx = 1/scale * exp(-(x - location)/scale) * exp(-exp((x - location)/scale))
    return(fx)
}


### 난수 함수
rgumbel_min = function (n, min=-10, max=10, scale = 1, location = 0) 
{
    normalization = function(x)	{	(x-min(x))/(max(x)-min(x));	}

	xseq = seq(min, max, length=1000000)
	res = sample(xseq, size=n, prob=normalization(dgumbel_min(xseq, scale=scale, location=location)), replace=TRUE)
	return(res)
}


### 누적분포함수
pgumbel_min = function (q, scale = 1, location = 0) 
{
    fx = -(sgumbel_min(x, scale, location) - 1)
    return(fx)
}


### 생존함수
sgumbel_min = function (x, scale = 1, location = 0) 
{
    fx = exp(- exp((x - location)/scale))
    return(fx)
}


### 위험함수
hgumbel_min = function (x, scale = 1, location = 0) 
{
    fx = dgumbel_min(x, scale, location) / sgumbel_min(x, scale, location)
    return(fx)
}





##### Plot
plot.gumbel_min_seq = function(x, location = 1, scale = 0, xlim=c(0, 10), ylim=c(0, 5), func="dgumbel_min")
{
    color=colorPalette(300)

    len_location = length(location)       # location 파라메터의 길이
    len_scale = length(scale)          # scale 파라메터의 길이
    
    color_counter = 1
    for (i in 1:len_location)  ### 파라메터: location
    {
        color_counter_init = color_counter
        legend_name = NULL;
        
        if (func=="dgumbel_min")     # 수명분포
        {
            plot(x, dgumbel_min(x, location=location[1], scale=scale[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Life Distribution Function")
            for (j in 1:len_scale)   ### 파라메터: scale
            {
                lines(x, dgumbel_min(x, location=location[i], scale=scale[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("location = ", location[i], " / scale = ", scale[j], sep=""))
            }
        }
        else if (func == "pgumbel_min")  # 누적분포함수
        {
            plot(x, pgumbel_min(x, location=location[1], scale=scale[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Cumulative Distribution Function")
            for (j in 1:len_scale)   ### 파라메터: scale
            {
                lines(x, pgumbel_min(x, location=location[i], scale=scale[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("location = ", location[i], " / scale = ", scale[j], sep=""))
            }
        }
        else if (func == "sgumbel_min")  # 생존함수
        {
            plot(x, sgumbel_min(x, location=location[1], scale=scale[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Survival Function")
            for (j in 1:len_scale)   ### 파라메터: scale
            {
                lines(x, sgumbel_min(x, location=location[i], scale=scale[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("location = ", location[i], " / scale = ", scale[j], sep=""))
            }
        }
        else if (func == "hgumbel_min")  # 위험함수
        {
            plot(x, hgumbel_min(x, location=location[1], scale=scale[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Hazard Function")
            for (j in 1:len_scale)   ### 파라메터: scale
            {
                lines(x, hgumbel_min(x, location=location[i], scale=scale[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("location = ", location[i], " / scale = ", scale[j], sep=""))
            }
        }
        legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
    }
}

par(mfrow = c(3, 3))
plot.gumbel_min_seq(x, location, scale, xlim=c(min(x), max(x)), ylim=c(0, 1), func="dgumbel_min")

par(mfrow = c(3, 3))
plot.gumbel_min_seq(x, location, scale, xlim=c(min(x), max(x)), ylim=c(0, 1), func="pgumbel_min")

par(mfrow = c(3, 3))
plot.gumbel_min_seq(x, location, scale, xlim=c(min(x), max(x)), ylim=c(0, 1), func="sgumbel_min")

par(mfrow = c(3, 3))
plot.gumbel_min_seq(x, location, scale, xlim=c(min(x), max(x)), ylim=c(0, 1), func="hgumbel_min")
